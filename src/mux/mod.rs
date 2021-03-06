use crate::guicommon::tabs::{Tab, TabId};
use crate::MasterPty;
use failure::Error;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Read;
use std::rc::Rc;
use std::thread;
use term::TerminalHost;
use termwiz::hyperlink::Hyperlink;

#[derive(Default)]
pub struct Mux {
    tabs: RefCell<HashMap<TabId, Rc<Tab>>>,
}

#[derive(Clone)]
pub enum PtyEvent {
    Data { tab_id: TabId, data: Vec<u8> },
    Terminated { tab_id: TabId },
}

pub trait PtyEventSender: Send {
    fn send(&self, event: PtyEvent) -> Result<(), Error>;
}

fn read_from_tab_pty(sender: Box<PtyEventSender>, tab_id: TabId, mut pty: MasterPty) {
    const BUFSIZE: usize = 32 * 1024;
    let mut buf = [0; BUFSIZE];
    loop {
        match pty.read(&mut buf) {
            Ok(size) if size == 0 => {
                eprintln!("read_pty EOF: tab_id {}", tab_id);
                sender.send(PtyEvent::Terminated { tab_id }).ok();
                return;
            }
            Ok(size) => {
                if sender
                    .send(PtyEvent::Data {
                        tab_id,
                        data: buf[0..size].to_vec(),
                    })
                    .is_err()
                {
                    return;
                }
            }
            Err(err) => {
                eprintln!("read_pty failed: tab {} {:?}", tab_id, err);
                sender.send(PtyEvent::Terminated { tab_id }).ok();
                return;
            }
        }
    }
}

/// This is just a stub impl of TerminalHost; it really only exists
/// in order to parse data sent by the peer (so, just to parse output).
/// As such it only really has Host::writer get called.
/// The GUI driven flows provide their own impl of TerminalHost.
struct Host<'a> {
    pty: &'a mut MasterPty,
}

impl<'a> TerminalHost for Host<'a> {
    fn writer(&mut self) -> &mut std::io::Write {
        &mut self.pty
    }

    fn click_link(&mut self, link: &Rc<Hyperlink>) {
        match open::that(link.uri()) {
            Ok(_) => {}
            Err(err) => eprintln!("failed to open {}: {:?}", link.uri(), err),
        }
    }

    fn get_clipboard(&mut self) -> Result<String, Error> {
        eprintln!("peer requested clipboard; ignoring");
        Ok("".into())
    }

    fn set_clipboard(&mut self, _clip: Option<String>) -> Result<(), Error> {
        Ok(())
    }

    fn set_title(&mut self, _title: &str) {}
}

impl Mux {
    pub fn get_tab(&self, tab_id: TabId) -> Option<Rc<Tab>> {
        self.tabs.borrow().get(&tab_id).map(Rc::clone)
    }

    pub fn add_tab(&self, sender: Box<PtyEventSender>, tab: &Rc<Tab>) -> Result<(), Error> {
        self.tabs.borrow_mut().insert(tab.tab_id(), Rc::clone(tab));

        let pty = tab.pty().try_clone()?;
        pty.clear_nonblocking()?;
        let tab_id = tab.tab_id();
        thread::spawn(move || read_from_tab_pty(sender, tab_id, pty));

        Ok(())
    }

    pub fn process_pty_event(&self, event: PtyEvent) -> Result<(), Error> {
        match event {
            PtyEvent::Data { tab_id, data } => {
                if let Some(tab) = self.get_tab(tab_id) {
                    tab.terminal().advance_bytes(
                        data,
                        &mut Host {
                            pty: &mut tab.pty(),
                        },
                    );
                }
            }
            PtyEvent::Terminated { tab_id } => {
                // The fact that we woke up is enough to trigger each
                // window to check for termination
                eprintln!("tab {} terminated", tab_id);
                self.tabs.borrow_mut().remove(&tab_id);
            }
        }
        Ok(())
    }

    pub fn is_empty(&self) -> bool {
        self.tabs.borrow().is_empty()
    }
}
