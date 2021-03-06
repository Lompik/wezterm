use super::GuiSystem;
use crate::config::Config;
use crate::font::FontConfiguration;
use crate::futurecore;
use crate::gliumwindows;
pub use crate::gliumwindows::GliumTerminalWindow;
use crate::guicommon::tabs::Tab;
use crate::guicommon::window::TerminalWindow;
use crate::guiloop::SessionTerminated;
use crate::mux::{Mux, PtyEvent, PtyEventSender};
use crate::spawn_tab;
use failure::Error;
use futures::future;
use glium;
use glium::glutin::EventsLoopProxy;
pub use glium::glutin::WindowId;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::mpsc::{self, Receiver, SyncSender, TryRecvError};
use std::thread;
use std::time::{Duration, SystemTime};

#[derive(Clone)]
pub struct GuiSender<T: Send> {
    tx: SyncSender<T>,
    proxy: EventsLoopProxy,
}

impl<T: Send> GuiSender<T> {
    pub fn send(&self, what: T) -> Result<(), Error> {
        match self.tx.send(what) {
            Ok(_) => {}
            Err(err) => bail!("send failed: {:?}", err),
        };
        self.proxy.wakeup()?;
        Ok(())
    }
}

impl futurecore::CoreSender for GuiSender<usize> {
    fn send(&self, idx: usize) -> Result<(), Error> {
        GuiSender::send(self, idx)
    }
}

impl PtyEventSender for GuiSender<PtyEvent> {
    fn send(&self, event: PtyEvent) -> Result<(), Error> {
        GuiSender::send(self, event)
    }
}

impl futurecore::CoreReceiver for Receiver<usize> {
    fn try_recv(&self) -> Result<usize, mpsc::TryRecvError> {
        Receiver::try_recv(self)
    }
}

pub fn channel<T: Send>(proxy: EventsLoopProxy) -> (GuiSender<T>, Receiver<T>) {
    // Set an upper bound on the number of items in the queue, so that
    // we don't swamp the gui loop; this puts back pressure on the
    // producer side so that we have a chance for eg: processing CTRL-C
    let (tx, rx) = mpsc::sync_channel(4);
    (GuiSender { tx, proxy }, rx)
}

/// This struct holds references to Windows.
/// The primary mapping is from `WindowId` -> `GliumTerminalWindow`.
#[derive(Default)]
struct Windows {
    by_id: HashMap<WindowId, gliumwindows::GliumTerminalWindow>,
}

/// The `GuiEventLoop` represents the combined gui event processor,
/// and a core for spawning tasks from futures.  It acts as the manager
/// for various events and is responsible for driving things forward.
pub struct GuiEventLoop {
    pub event_loop: RefCell<glium::glutin::EventsLoop>,
    windows: Rc<RefCell<Windows>>,
    core: futurecore::Core,
    poll_tx: GuiSender<PtyEvent>,
    poll_rx: Receiver<PtyEvent>,
    tick_rx: Receiver<()>,
    mux: Rc<Mux>,
}

const TICK_INTERVAL: Duration = Duration::from_millis(50);
const MAX_POLL_LOOP_DURATION: Duration = Duration::from_millis(500);

pub struct GlutinGuiSystem {
    event_loop: Rc<GuiEventLoop>,
}

impl GlutinGuiSystem {
    pub fn try_new(mux: &Rc<Mux>) -> Result<Rc<GuiSystem>, Error> {
        let event_loop = Rc::new(GuiEventLoop::new(mux)?);
        Ok(Rc::new(Self { event_loop }))
    }

    /// Loop through the core and dispatch any tasks that have been
    /// notified as ready to run.  Returns once all such tasks have
    /// been polled and there are no more pending task notifications.
    fn process_futures(&self) {
        loop {
            if !self.event_loop.core.turn() {
                break;
            }
        }
    }
}

impl GuiSystem for GlutinGuiSystem {
    fn pty_sender(&self) -> Box<PtyEventSender> {
        Box::new(self.event_loop.poll_tx.clone())
    }

    fn run_forever(&self) -> Result<(), Error> {
        // This convoluted run() signature is present because of this issue:
        // https://github.com/tomaka/winit/issues/413
        let myself = &self.event_loop;
        loop {
            self.process_futures();

            // Check the window count; if after processing the futures there
            // are no windows left, then we are done.
            {
                let windows = myself.windows.borrow();
                if windows.by_id.is_empty() {
                    debug!("No more windows; done!");
                    return Ok(());
                }
            }

            myself.run_event_loop()?;
            myself.process_poll()?;
            myself.process_tick()?;
        }
    }

    fn spawn_new_window(
        &self,
        config: &Rc<crate::config::Config>,
        fontconfig: &Rc<crate::font::FontConfiguration>,
        tab: &Rc<Tab>,
    ) -> Result<(), Error> {
        let window = GliumTerminalWindow::new(&self.event_loop, fontconfig, config, tab)?;

        self.event_loop.add_window(window)
    }
}

impl GuiEventLoop {
    pub fn new(mux: &Rc<Mux>) -> Result<Self, Error> {
        let event_loop = glium::glutin::EventsLoop::new();

        let (fut_tx, fut_rx) = channel(event_loop.create_proxy());
        let fut_tx2 = fut_tx.clone();
        let core = futurecore::Core::new(Box::new(fut_tx), Box::new(fut_tx2), Box::new(fut_rx));

        let (poll_tx, poll_rx) = channel(event_loop.create_proxy());

        // The glutin/glium plumbing has no native tick/timer stuff, so
        // we implement one using a thread.  Nice.
        let (tick_tx, tick_rx) = channel(event_loop.create_proxy());
        thread::spawn(move || loop {
            std::thread::sleep(TICK_INTERVAL);
            if tick_tx.send(()).is_err() {
                return;
            }
        });

        Ok(Self {
            core,
            poll_tx,
            poll_rx,
            tick_rx,
            event_loop: RefCell::new(event_loop),
            windows: Rc::new(RefCell::new(Default::default())),
            mux: Rc::clone(mux),
        })
    }

    pub fn register_tab(&self, tab: &Rc<Tab>) -> Result<(), Error> {
        self.mux.add_tab(Box::new(self.poll_tx.clone()), tab)
    }

    fn do_spawn_new_window(
        events: &Rc<Self>,
        config: &Rc<Config>,
        fonts: &Rc<FontConfiguration>,
    ) -> Result<(), Error> {
        let tab = spawn_tab(&config, None)?;
        let sender = Box::new(events.poll_tx.clone());
        events.mux.add_tab(sender, &tab)?;
        let window = GliumTerminalWindow::new(&events, &fonts, &config, &tab)?;

        events.add_window(window)
    }

    pub fn schedule_spawn_new_window(
        events: &Rc<Self>,
        config: &Rc<Config>,
        fonts: &Rc<FontConfiguration>,
    ) {
        let myself = Rc::clone(events);
        let config = Rc::clone(config);
        let fonts = Rc::clone(fonts);
        events.core.spawn(futures::future::poll_fn(move || {
            Self::do_spawn_new_window(&myself, &config, &fonts)
                .map(futures::Async::Ready)
                .map_err(|_| ())
        }));
    }

    pub fn with_window<F: 'static + Fn(&mut TerminalWindow) -> Result<(), Error>>(
        events: &Rc<Self>,
        window_id: WindowId,
        func: F,
    ) {
        let myself = Rc::clone(events);
        events.core.spawn(futures::future::poll_fn(move || {
            let mut windows = myself.windows.borrow_mut();
            if let Some(window) = windows.by_id.get_mut(&window_id) {
                func(window).map(futures::Async::Ready).map_err(|_| ())
            } else {
                Err(())
            }
        }));
    }

    /// Add a window to the event loop and run it.
    pub fn add_window(&self, window: gliumwindows::GliumTerminalWindow) -> Result<(), Error> {
        let window_id = window.window_id();
        let mut windows = self.windows.borrow_mut();
        windows.by_id.insert(window_id, window);
        Ok(())
    }

    /// Process a single winit event
    fn process_gui_event(
        &self,
        event: &glium::glutin::Event,
    ) -> Result<glium::glutin::ControlFlow, Error> {
        use glium::glutin::ControlFlow::{Break, Continue};
        use glium::glutin::Event;
        let result = match *event {
            Event::WindowEvent { window_id, .. } => {
                let dead = match self.windows.borrow_mut().by_id.get_mut(&window_id) {
                    Some(window) => match window.dispatch_event(event) {
                        Ok(_) => None,
                        Err(err) => match err.downcast_ref::<SessionTerminated>() {
                            Some(_) => Some(window_id),
                            _ => return Err(err),
                        },
                    },
                    None => None,
                };

                if let Some(window_id) = dead {
                    self.schedule_window_close(window_id)?;
                }
                Continue
            }
            Event::Awakened => Break,
            _ => Continue,
        };
        Ok(result)
    }

    /// Spawns a future that will gracefully shut down the resources associated
    /// with the specified window.
    fn schedule_window_close(&self, window_id: WindowId) -> Result<(), Error> {
        let windows = Rc::clone(&self.windows);

        self.core.spawn(futures::future::lazy(move || {
            let mut windows = windows.borrow_mut();
            windows.by_id.remove(&window_id);
            future::ok(())
        }));

        Ok(())
    }

    /// Run through all of the windows and cause them to paint if they need it.
    /// This happens ~50ms or so.
    fn do_paint(&self) {
        for window in &mut self.windows.borrow_mut().by_id.values_mut() {
            window.paint_if_needed().unwrap();
        }
    }

    /// Process events on poll_rx.  We may have a pty
    /// event or our interval timer may have expired, indicating that
    /// we need to paint.
    fn process_poll(&self) -> Result<(), Error> {
        let start = SystemTime::now();
        loop {
            match start.elapsed() {
                Ok(elapsed) if elapsed > MAX_POLL_LOOP_DURATION => {
                    return Ok(());
                }
                Err(_) => {
                    return Ok(());
                }
                _ => {}
            }
            match self.poll_rx.try_recv() {
                Ok(event) => self.mux.process_pty_event(event)?,
                Err(TryRecvError::Empty) => return Ok(()),
                Err(err) => bail!("poll_rx disconnected {:?}", err),
            }
        }
    }

    fn process_tick(&self) -> Result<(), Error> {
        loop {
            match self.tick_rx.try_recv() {
                Ok(_) => {
                    self.test_for_child_exit();
                    self.do_paint();
                }
                Err(TryRecvError::Empty) => return Ok(()),
                Err(err) => bail!("tick_rx disconnected {:?}", err),
            }
        }
    }

    fn test_for_child_exit(&self) {
        let window_ids: Vec<WindowId> = self
            .windows
            .borrow_mut()
            .by_id
            .iter_mut()
            .filter_map(|(window_id, window)| {
                if window.test_for_child_exit() {
                    Some(*window_id)
                } else {
                    None
                }
            })
            .collect();

        for window_id in window_ids {
            self.schedule_window_close(window_id).ok();
        }
    }

    /// Runs the winit event loop.  This blocks until a wakeup signal
    /// is delivered to the event loop.  The `GuiSender` is our way
    /// of trigger those wakeups.
    fn run_event_loop(&self) -> Result<(), Error> {
        let mut event_loop = self.event_loop.borrow_mut();
        event_loop.run_forever(|event| {
            use glium::glutin::ControlFlow::{Break, Continue};

            let result = self.process_gui_event(&event);

            match result {
                Ok(Continue) => {
                    self.do_paint();
                    Continue
                }
                Ok(Break) => Break,
                Err(err) => {
                    eprintln!("Error in event loop: {:?}", err);
                    Break
                }
            }
        });
        Ok(())
    }
}
