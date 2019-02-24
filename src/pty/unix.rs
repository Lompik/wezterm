//! Working with pseudo-terminals

use failure::Error;
use libc::{self, winsize};
use mio::event::Evented;
use mio::unix::EventedFd;
use mio::{Poll, PollOpt, Ready, Token};
use std::io;
use std::mem;
use std::os::unix::io::{AsRawFd, FromRawFd, RawFd};
use std::os::unix::process::CommandExt;
use std::process::Stdio;
use std::ptr;

pub use std::process::{Child, Command, ExitStatus};

/// Represents the master end of a pty.
/// The file descriptor will be closed when the Pty is dropped.
pub struct MasterPty {
    fd: RawFd,
}

/// Represents the slave end of a pty.
/// The file descriptor will be closed when the Pty is dropped.
pub struct SlavePty {
    fd: RawFd,
}

impl Drop for MasterPty {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.fd);
        }
    }
}

impl Drop for SlavePty {
    fn drop(&mut self) {
        unsafe {
            libc::close(self.fd);
        }
    }
}

/// Helper function to set the close-on-exec flag for a raw descriptor
fn cloexec(fd: RawFd) -> Result<(), Error> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFD) };
    if flags == -1 {
        bail!(
            "fcntl to read flags failed: {:?}",
            io::Error::last_os_error()
        );
    }
    let result = unsafe { libc::fcntl(fd, libc::F_SETFD, flags | libc::FD_CLOEXEC) };
    if result == -1 {
        bail!(
            "fcntl to set CLOEXEC failed: {:?}",
            io::Error::last_os_error()
        );
    }
    Ok(())
}

/// Helper function to duplicate a file descriptor.
/// The duplicated descriptor will have the close-on-exec flag set.
fn dup(fd: RawFd) -> Result<RawFd, Error> {
    // Note that linux has a variant of the dup syscall that can set
    // the CLOEXEC flag at dup time.  We could use that here but the
    // additional code complexity isn't worth it: it's just a couple
    // of syscalls at startup to do it the portable way below.
    let new_fd = unsafe { libc::dup(fd) };
    if new_fd == -1 {
        bail!("dup of pty fd failed: {:?}", io::Error::last_os_error())
    }
    match cloexec(new_fd) {
        Ok(_) => Ok(new_fd),
        Err(err) => {
            unsafe { libc::close(new_fd) };
            Err(err)
        }
    }
}

fn clear_nonblocking(fd: RawFd) -> Result<(), Error> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL, 0) };
    if flags == -1 {
        bail!(
            "fcntl to read flags failed: {:?}",
            io::Error::last_os_error()
        );
    }
    let result = unsafe { libc::fcntl(fd, libc::F_SETFL, flags & !libc::O_NONBLOCK) };
    if result == -1 {
        bail!(
            "fcntl to set NONBLOCK failed: {:?}",
            io::Error::last_os_error()
        );
    }
    Ok(())
}

fn set_nonblocking(fd: RawFd) -> Result<(), Error> {
    let flags = unsafe { libc::fcntl(fd, libc::F_GETFL, 0) };
    if flags == -1 {
        bail!(
            "fcntl to read flags failed: {:?}",
            io::Error::last_os_error()
        );
    }
    let result = unsafe { libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK) };
    if result == -1 {
        bail!(
            "fcntl to set NONBLOCK failed: {:?}",
            io::Error::last_os_error()
        );
    }
    Ok(())
}

/// Create a new Pty instance with the window size set to the specified
/// dimensions.  Returns a (master, slave) Pty pair.  The master side
/// is used to drive the slave side.
pub fn openpty(
    num_rows: u16,
    num_cols: u16,
    pixel_width: u16,
    pixel_height: u16,
) -> Result<(MasterPty, SlavePty), Error> {
    let mut master: RawFd = -1;
    let mut slave: RawFd = -1;

    let mut size = winsize {
        ws_row: num_rows,
        ws_col: num_cols,
        ws_xpixel: pixel_width,
        ws_ypixel: pixel_height,
    };

    let result = unsafe {
        // BSDish systems may require mut pointers to some args
        #[cfg_attr(feature = "cargo-clippy", allow(unnecessary_mut_passed))]
        libc::openpty(
            &mut master,
            &mut slave,
            ptr::null_mut(),
            ptr::null_mut(),
            &mut size,
        )
    };

    if result != 0 {
        bail!("failed to openpty: {:?}", io::Error::last_os_error());
    }

    let master = MasterPty { fd: master };
    let slave = SlavePty { fd: slave };

    // Ensure that these descriptors will get closed when we execute
    // the child process.  This is done after constructing the Pty
    // instances so that we ensure that the Ptys get drop()'d if
    // the cloexec() functions fail (unlikely!).
    cloexec(master.fd)?;
    cloexec(slave.fd)?;

    set_nonblocking(master.fd)?;

    Ok((master, slave))
}

impl SlavePty {
    /// Helper for setting up a Command instance
    fn as_stdio(&self) -> Result<Stdio, Error> {
        dup(self.fd).map(|fd| unsafe { Stdio::from_raw_fd(fd) })
    }

    /// this method prepares a Command builder to spawn a process with the Pty
    /// set up to be the controlling terminal, and then spawns the command.
    /// This method consumes the slave Pty instance and the Command builder
    /// instance so that the associated file descriptors are closed.
    /// The `cmd` parameter is set up to reference the slave
    /// Pty for its stdio streams, as well as to establish itself as the session
    /// leader.
    pub fn spawn_command(self, mut cmd: Command) -> Result<Child, Error> {
        cmd.stdin(self.as_stdio()?)
            .stdout(self.as_stdio()?)
            .stderr(self.as_stdio()?)
            .before_exec(move || {
                // Clean up a few things before we exec the program
                unsafe {
                    // Clear out any potentially problematic signal
                    // dispositions that we might have inherited
                    for signo in &[
                        libc::SIGCHLD,
                        libc::SIGHUP,
                        libc::SIGINT,
                        libc::SIGQUIT,
                        libc::SIGTERM,
                        libc::SIGALRM,
                    ] {
                        libc::signal(*signo, libc::SIG_DFL);
                    }

                    // Establish ourselves as a session leader.
                    if libc::setsid() == -1 {
                        return Err(io::Error::last_os_error());
                    }

                    // Set the pty as the controlling terminal.
                    // Failure to do this means that delivery of
                    // SIGWINCH won't happen when we resize the
                    // terminal, among other undesirable effects.
                    if libc::ioctl(0, libc::TIOCSCTTY as _, 0) == -1 {
                        return Err(io::Error::last_os_error());
                    }
                    Ok(())
                }
            });

        let mut child = cmd.spawn()?;

        // Ensure that we close out the slave fds that Child retains;
        // they are not what we need (we need the master side to reference
        // them) and won't work in the usual way anyway.
        // In practice these are None, but it seems best to be move them
        // out in case the behavior of Command changes in the future.
        child.stdin.take();
        child.stdout.take();
        child.stderr.take();

        Ok(child)
    }

    pub fn spawn_command_with_stdin(self, mut cmd: Command, text: &str) -> Result<Child, Error> {

        let mut stdin_pipe: [libc::c_int; 2] = [0, 0];
        let res = unsafe{
            libc::pipe(stdin_pipe.as_mut_ptr())
        };
        if res != 0 {
            eprintln!("error setting pipe");
        }
        eprintln!("{:?}", stdin_pipe);

        let txt: String = text.into();
        std::thread::spawn(move|| {
            use std::io::Write;
            let mut cw = unsafe {
                std::fs::File::from_raw_fd(stdin_pipe[1])
            }; // close when out of scope
            //use std::io::BufWriter;
            cw.write_all(txt.as_bytes()).expect("Failed to write to child pipe");
            eprintln!("stding wirtten");
        });

        //use std::process::Stdio::FromRawFd;
        cmd.stdin(self.as_stdio()?)
            .stdout(self.as_stdio()?)
            .stderr(self.as_stdio()?)
            .before_exec(move || {
                // Clean up a few things before we exec the program

                unsafe {
                    // Clear out any potentially problematic signal
                    // dispositions that we might have inherited
                    eprintln!("pty:{}", "setting signals");
                    for signo in &[
                        libc::SIGCHLD,
                        libc::SIGHUP,
                        libc::SIGINT,
                        libc::SIGQUIT,
                        libc::SIGTERM,
                        libc::SIGALRM,
                    ] {
                        libc::signal(*signo, libc::SIG_DFL);
                    }

                    // Establish ourselves as a session leader.
                    if libc::setsid() == -1 {
                        return Err(io::Error::last_os_error());
                    }
                    eprintln!("pty:{}", "stsid done");
                    // Set the pty as the controlling terminal.
                    // Failure to do this means that delivery of
                    // SIGWINCH won't happen when we resize the
                    // terminal, among other undesirable effects.
                    if libc::ioctl(0, libc::TIOCSCTTY as _, 0) == -1 {
                        return Err(io::Error::last_os_error());
                    }
                    eprintln!("pty: befoire exec");

                    libc::dup2(stdin_pipe[0], 0);
                    libc::close(stdin_pipe[0]);
                    libc::close(stdin_pipe[1]);

                    Ok(())
                }
            });

        let mut child = cmd.spawn()?;
        eprintln!("pty:spawned !");
        // Ensure that we close out the slave fds that Child retains;
        // they are not what we need (we need the master side to reference
        // them) and won't work in the usual way anyway.
        // In practice these are None, but it seems best to be move them
        // out in case the behavior of Command changes in the future.
        child.stdin.take();
        child.stdout.take();
        child.stderr.take();

        Ok(child)
    }
}

impl MasterPty {
    /// Inform the kernel and thus the child process that the window resized.
    /// It will update the winsize information maintained by the kernel,
    /// and generate a signal for the child to notice and update its state.
    pub fn resize(
        &self,
        num_rows: u16,
        num_cols: u16,
        pixel_width: u16,
        pixel_height: u16,
    ) -> Result<(), Error> {
        let size = winsize {
            ws_row: num_rows,
            ws_col: num_cols,
            ws_xpixel: pixel_width,
            ws_ypixel: pixel_height,
        };

        if unsafe { libc::ioctl(self.fd, libc::TIOCSWINSZ, &size as *const _) } != 0 {
            bail!(
                "failed to ioctl(TIOCSWINSZ): {:?}",
                io::Error::last_os_error()
            );
        }

        Ok(())
    }

    pub fn get_size(&self) -> Result<winsize, Error> {
        let mut size: winsize = unsafe { mem::zeroed() };
        if unsafe { libc::ioctl(self.fd, libc::TIOCGWINSZ, &mut size as *mut _) } != 0 {
            bail!(
                "failed to ioctl(TIOCGWINSZ): {:?}",
                io::Error::last_os_error()
            );
        }
        Ok(size)
    }

    pub fn clear_nonblocking(&self) -> Result<(), Error> {
        clear_nonblocking(self.fd)
    }

    pub fn try_clone(&self) -> Result<Self, Error> {
        let fd = dup(self.fd)?;
        Ok(Self { fd })
    }
}

impl AsRawFd for MasterPty {
    fn as_raw_fd(&self) -> RawFd {
        self.fd
    }
}

impl io::Write for MasterPty {
    fn write(&mut self, buf: &[u8]) -> Result<usize, io::Error> {
        let size = unsafe { libc::write(self.fd, buf.as_ptr() as *const _, buf.len()) };
        if size == -1 {
            Err(io::Error::last_os_error())
        } else {
            Ok(size as usize)
        }
    }
    fn flush(&mut self) -> Result<(), io::Error> {
        Ok(())
    }
}

impl io::Read for MasterPty {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, io::Error> {
        let size = unsafe { libc::read(self.fd, buf.as_mut_ptr() as *mut _, buf.len()) };
        if size == -1 {
            Err(io::Error::last_os_error())
        } else {
            Ok(size as usize)
        }
    }
}

/// Glue for working with mio
impl Evented for MasterPty {
    fn register(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> io::Result<()> {
        EventedFd(&self.fd).register(poll, token, interest, opts)
    }

    fn reregister(
        &self,
        poll: &Poll,
        token: Token,
        interest: Ready,
        opts: PollOpt,
    ) -> io::Result<()> {
        EventedFd(&self.fd).reregister(poll, token, interest, opts)
    }

    fn deregister(&self, poll: &Poll) -> io::Result<()> {
        EventedFd(&self.fd).deregister(poll)
    }
}
