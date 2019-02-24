//! # Terminal Wizardry
//!
//! This is a rust crate that provides a number of support functions
//! for applications interesting in either displaying data to a terminal
//! or in building a terminal emulator.
//!
//! It is currently in active development and subject to fairly wild
//! sweeping changes.
//!
//! Included functionality:
//!
//! * `Surface` models a terminal display and its component `Cell`s
//! * Terminal attributes are aware of modern features such as
//!   True Color, [Hyperlinks](https://gist.github.com/egmontkob/eb114294efbcd5adb1944c9f3cb5feda)
//!   and will also support sixel and iterm style terminal graphics display.
//! * `Surface`s include a log of `Change`s and an API for consuming
//!   and applying deltas.  This is a powerful building block for
//!   synchronizing screen instances.
//! * Escape sequence parser decodes inscrutable escape sequences
//!   and gives them semantic meaning, making the code that uses
//!   them clearer.  The decoded escapes can be re-encoded, allowing
//!   applications to start with the semantic meaning and emit
//!   the appropriate escape sequence without embedding obscure
//!   binary bytes.
//! * `Capabilities` allows probing for terminal capabilities
//!   that may not be included in the system terminfo database,
//!   and overriding them in an embedding application.
//! * `Terminal` trait provides an abstraction over unix style ttys
//!   and Windows style console APIs.  `Change`s from `Surface`
//!   can be rendered to `Terminal`s.  `Terminal`s allow decoding
//!   mouse and keyboard inputs in both blocking or non-blocking
//!   mode.
//! * `Widget` trait allows composition of UI elements at a higher
//!   level.
#[macro_use]
extern crate failure;
extern crate base64;
extern crate fnv;
extern crate libc;
extern crate palette;
extern crate semver;
extern crate serde;
#[cfg(unix)]
extern crate signal_hook;
extern crate terminfo;
#[cfg(unix)]
extern crate termios;
#[cfg(windows)]
extern crate winapi;
#[macro_use]
extern crate serde_derive;
pub extern crate num;
pub extern crate vte;
#[macro_use]
extern crate num_derive;
#[macro_use]
extern crate derive_builder;
#[macro_use]
extern crate bitflags;

extern crate cassowary;
extern crate image as image_crate;
extern crate memmem;
extern crate ordered_float;
extern crate regex;
#[macro_use]
extern crate smallvec;
extern crate unicode_segmentation;
extern crate unicode_width;

pub mod caps;
pub mod cell;
pub mod cellcluster;
pub mod color;
pub mod escape;
pub mod hyperlink;
pub mod image;
pub mod input;
pub mod istty;
pub mod keymap;
mod range;
mod readbuf;
pub mod render;
pub mod surface;
pub mod terminal;
pub mod widgets;
