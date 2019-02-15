use super::*;
use image::{self, GenericImage};
use ordered_float::NotNaN;
use std::fmt::Write;
use termwiz::escape::csi::{
    Cursor, DecPrivateMode, DecPrivateModeCode, Device, Edit, EraseInDisplay, EraseInLine, Mode,
    Sgr,
};
use termwiz::escape::osc::{ITermFileData, ITermProprietary};
use termwiz::escape::{Action, ControlCode, Esc, EscCode, OperatingSystemCommand, CSI};
use termwiz::hyperlink::Rule as HyperlinkRule;
use termwiz::image::{ImageCell, ImageData, TextureCoordinate};
use unicode_segmentation::UnicodeSegmentation;

struct TabStop {
    tabs: Vec<bool>,
    tab_width: usize,
}

impl TabStop {
    fn new(screen_width: usize, tab_width: usize) -> Self {
        let mut tabs = Vec::with_capacity(screen_width);

        for i in 0..screen_width {
            tabs.push((i % tab_width) == 0);
        }
        Self { tabs, tab_width }
    }

    fn set_tab_stop(&mut self, col: usize) {
        self.tabs[col] = true;
    }

    fn find_next_tab_stop(&self, col: usize) -> Option<usize> {
        for i in col + 1..self.tabs.len() {
            if self.tabs[i] {
                return Some(i);
            }
        }
        None
    }

    /// Respond to the terminal resizing.
    /// If the screen got bigger, we need to expand the tab stops
    /// into the new columns with the appropriate width.
    fn resize(&mut self, screen_width: usize) {
        let current = self.tabs.len();
        if screen_width > current {
            for i in current..screen_width {
                self.tabs.push((i % self.tab_width) == 0);
            }
        }
    }
}

#[derive(Clone)]
enum Charset{
    USAscii,
    SpecialCharacterAndLineDrawing
}

impl Charset {
    fn map(&self, c: char) -> char {
        match self{
            Charset::USAscii => c,
            Charset::SpecialCharacterAndLineDrawing => {
                match c {
                    '`' => '◆',
                    'a' => '▒',
                    'b' => '\t',
                    'c' => '\u{000c}',
                    'd' => '\r',
                    'e' => '\n',
                    'f' => '°',
                    'g' => '±',
                    'h' => '\u{2424}',
                    'i' => '\u{000b}',
                    'j' => '┘',
                    'k' => '┐',
                    'l' => '┌',
                    'm' => '└',
                    'n' => '┼',
                    'o' => '⎺',
                    'p' => '⎻',
                    'q' => '─',
                    'r' => '⎼',
                    's' => '⎽',
                    't' => '├',
                    'u' => '┤',
                    'v' => '┴',
                    'w' => '┬',
                    'x' => '│',
                    'y' => '≤',
                    'z' => '≥',
                    '{' => 'π',
                    '|' => '≠',
                    '}' => '£',
                    '~' => '·',
                    _ => c
                }
            }
        }
    }
}

pub struct TerminalState {
    screen: ScreenOrAlt,
    /// The current set of attributes in effect for the next
    /// attempt to print to the display
    pen: CellAttributes,
    /// The current cursor position, relative to the top left
    /// of the screen.  0-based index.
    cursor: CursorPosition,
    saved_cursor: CursorPosition,

    /// if true, implicitly move to the next line on the next
    /// printed character
    wrap_next: bool,

    /// The scroll region
    scroll_region: Range<VisibleRowIndex>,

    /// When set, modifies the sequence of bytes sent for keys
    /// designated as cursor keys.  This includes various navigation
    /// keys.  The code in key_down() is responsible for interpreting this.
    application_cursor_keys: bool,

    /// When set, modifies the sequence of bytes sent for keys
    /// in the numeric keypad portion of the keyboard.
    application_keypad: bool,

    /// When set, pasting the clipboard should bracket the data with
    /// designated marker characters.
    bracketed_paste: bool,

    sgr_mouse: bool,
    button_event_mouse: bool,
    current_mouse_button: MouseButton,
    mouse_position: CursorPosition,
    cursor_visible: bool,
    linewrap: bool,
    lf_nl: bool,

    /// Which hyperlink is considered to be highlighted, because the
    /// mouse_position is over a cell with a Hyperlink attribute.
    current_highlight: Option<Rc<Hyperlink>>,

    /// Keeps track of double and triple clicks
    last_mouse_click: Option<LastMouseClick>,

    /// Used to compute the offset to the top of the viewport.
    /// This is used to display the scrollback of the terminal.
    /// It is distinct from the scroll_region in that the scroll region
    /// afects how the terminal output is scrolled as data is output,
    /// and the viewport_offset is used to index into the scrollback
    /// purely for display purposes.
    /// The offset is measured from the top of the physical viewable
    /// screen with larger numbers going backwards.
    pub(crate) viewport_offset: VisibleRowIndex,

    /// Remembers the starting coordinate of the selection prior to
    /// dragging.
    selection_start: Option<SelectionCoordinate>,
    /// Holds the not-normalized selection range.
    selection_range: Option<SelectionRange>,

    tabs: TabStop,

    hyperlink_rules: Vec<HyperlinkRule>,

    /// The terminal title string
    title: String,

    active_charset: Charset
}

/// Like Write::write_all except that we keep looping
/// when we get WouldBlock
fn write_all(w: &mut std::io::Write, mut buf: &[u8]) -> std::io::Result<()> {
    use std::io::ErrorKind;
    while !buf.is_empty() {
        match w.write(buf) {
            Ok(0) => {
                return Err(std::io::Error::new(
                    ErrorKind::WriteZero,
                    "failed to write whole buffer",
                ));
            }
            Ok(n) => buf = &buf[n..],
            Err(ref e)
                if e.kind() == ErrorKind::Interrupted || e.kind() == ErrorKind::WouldBlock => {}
            Err(e) => return Err(e),
        }
    }
    Ok(())
}

impl TerminalState {
    pub fn new(
        physical_rows: usize,
        physical_cols: usize,
        scrollback_size: usize,
        hyperlink_rules: Vec<HyperlinkRule>,
    ) -> TerminalState {
        let screen = ScreenOrAlt::new(physical_rows, physical_cols, scrollback_size);

        TerminalState {
            screen,
            pen: CellAttributes::default(),
            cursor: CursorPosition::default(),
            saved_cursor: CursorPosition::default(),
            scroll_region: 0..physical_rows as VisibleRowIndex,
            wrap_next: false,
            application_cursor_keys: false,
            application_keypad: false,
            bracketed_paste: false,
            sgr_mouse: false,
            button_event_mouse: false,
            cursor_visible: true,
            current_mouse_button: MouseButton::None,
            mouse_position: CursorPosition::default(),
            current_highlight: None,
            last_mouse_click: None,
            viewport_offset: 0,
            selection_range: None,
            selection_start: None,
            tabs: TabStop::new(physical_cols, 8),
            hyperlink_rules,
            title: "wezterm".to_string(),
            active_charset: Charset::USAscii,
            linewrap: true,
            lf_nl: false,
        }
    }

    pub fn get_title(&self) -> &str {
        &self.title
    }

    pub fn screen(&self) -> &Screen {
        &self.screen
    }

    pub fn screen_mut(&mut self) -> &mut Screen {
        &mut self.screen
    }

    pub fn get_selection_text(&self) -> String {
        let mut s = String::new();

        if let Some(sel) = self.selection_range.as_ref().map(|r| r.normalize()) {
            let screen = self.screen();
            for y in sel.rows() {
                let idx = screen.scrollback_or_visible_row(y);
                let cols = sel.cols_for_row(y);
                if !s.is_empty() {
                    s.push('\n');
                }
                s.push_str(screen.lines[idx].columns_as_str(cols).trim_end());
            }
        }

        s
    }

    /// Dirty the lines in the current selection range
    fn dirty_selection_lines(&mut self) {
        if let Some(sel) = self.selection_range.as_ref().map(|r| r.normalize()) {
            let screen = self.screen_mut();
            for y in screen.scrollback_or_visible_range(&sel.rows()) {
                screen.line_mut(y).set_dirty();
            }
        }
    }

    pub fn clear_selection(&mut self) {
        self.dirty_selection_lines();
        self.selection_range = None;
        self.selection_start = None;
    }

    /// If `cols` on the specified `row` intersect with the selection range,
    /// clear the selection rnage.  This doesn't invalidate the selection,
    /// it just cancels rendering the selected text.
    /// Returns true if the selection is invalidated or not present, which
    /// is useful to terminate a loop when there is no more work to be done.
    fn clear_selection_if_intersects(
        &mut self,
        cols: Range<usize>,
        row: ScrollbackOrVisibleRowIndex,
    ) -> bool {
        let sel = self.selection_range.take();
        match sel {
            Some(sel) => {
                let sel_cols = sel.cols_for_row(row);
                if intersects_range(cols, sel_cols) {
                    // Intersects, so clear the selection
                    self.clear_selection();
                    true
                } else {
                    self.selection_range = Some(sel);
                    false
                }
            }
            None => true,
        }
    }

    /// If `rows` intersect with the selection range, clear the selection rnage.
    /// This doesn't invalidate the selection, it just cancels rendering the
    /// selected text.
    /// Returns true if the selection is invalidated or not present, which
    /// is useful to terminate a loop when there is no more work to be done.
    fn clear_selection_if_intersects_rows(
        &mut self,
        rows: Range<ScrollbackOrVisibleRowIndex>,
    ) -> bool {
        let sel = self.selection_range.take();
        match sel {
            Some(sel) => {
                let sel_rows = sel.rows();
                if intersects_range(rows, sel_rows) {
                    // Intersects, so clear the selection
                    self.clear_selection();
                    true
                } else {
                    self.selection_range = Some(sel);
                    false
                }
            }
            None => true,
        }
    }

    fn hyperlink_for_cell(
        &mut self,
        x: usize,
        y: ScrollbackOrVisibleRowIndex,
    ) -> Option<Rc<Hyperlink>> {
        let rules = &self.hyperlink_rules;

        let idx = self.screen.scrollback_or_visible_row(y);
        match self.screen.lines.get_mut(idx) {
            Some(ref mut line) => {
                line.scan_and_create_hyperlinks(rules);
                match line.cells().get(x) {
                    Some(cell) => cell.attrs().hyperlink.as_ref().cloned(),
                    None => None,
                }
            }
            None => None,
        }
    }

    /// Invalidate rows that have hyperlinks
    fn invalidate_hyperlinks(&mut self) {
        let screen = self.screen_mut();
        for line in &mut screen.lines {
            if line.has_hyperlink() {
                line.set_dirty();
            }
        }
    }

    /// Called after a mouse move or viewport scroll to recompute the
    /// current highlight
    fn recompute_highlight(&mut self) {
        let line_idx = self.mouse_position.y as ScrollbackOrVisibleRowIndex
            - self.viewport_offset as ScrollbackOrVisibleRowIndex;
        let x = self.mouse_position.x;
        self.current_highlight = self.hyperlink_for_cell(x, line_idx);
        self.invalidate_hyperlinks();
    }

    #[cfg_attr(feature = "cargo-clippy", allow(cyclomatic_complexity))]
    pub fn mouse_event(
        &mut self,
        mut event: MouseEvent,
        host: &mut TerminalHost,
    ) -> Result<(), Error> {
        // Clamp the mouse coordinates to the size of the model.
        // This situation can trigger for example when the
        // window is resized and leaves a partial row at the bottom of the
        // terminal.  The mouse can move over that portion and the gui layer
        // can thus send us out-of-bounds row or column numbers.  We want to
        // make sure that we clamp this and handle it nicely at the model layer.
        event.y = event.y.min(self.screen().physical_rows as i64 - 1);
        event.x = event.x.min(self.screen().physical_cols - 1);

        // Remember the last reported mouse position so that we can use it
        // for highlighting clickable things elsewhere.
        let new_position = CursorPosition {
            x: event.x,
            y: event.y as VisibleRowIndex,
        };

        if new_position != self.mouse_position {
            self.mouse_position = new_position;
            self.recompute_highlight();
        }

        // First pass to figure out if we're messing with the selection
        let send_event = self.sgr_mouse && !event.modifiers.contains(KeyModifiers::SHIFT);

        // Perform click counting
        if event.kind == MouseEventKind::Press {
            let click = match self.last_mouse_click.take() {
                None => LastMouseClick::new(event.button),
                Some(click) => click.add(event.button),
            };
            self.last_mouse_click = Some(click);
        }

        if !send_event {
            match (event, self.current_mouse_button) {
                (
                    MouseEvent {
                        kind: MouseEventKind::Press,
                        button: MouseButton::Left,
                        ..
                    },
                    _,
                ) => {
                    self.current_mouse_button = MouseButton::Left;
                    self.dirty_selection_lines();
                    match self.last_mouse_click.as_ref() {
                        // Single click prepares the start of a new selection
                        Some(&LastMouseClick { streak: 1, .. }) => {
                            // Prepare to start a new selection.
                            // We don't form the selection until the mouse drags.
                            self.selection_range = None;
                            self.selection_start = Some(SelectionCoordinate {
                                x: event.x,
                                y: event.y as ScrollbackOrVisibleRowIndex
                                    - self.viewport_offset as ScrollbackOrVisibleRowIndex,
                            });
                            host.set_clipboard(None)?;
                        }
                        // Double click to select a word on the current line
                        Some(&LastMouseClick { streak: 2, .. }) => {
                            let y = event.y as ScrollbackOrVisibleRowIndex
                                - self.viewport_offset as ScrollbackOrVisibleRowIndex;
                            let idx = self.screen().scrollback_or_visible_row(y);
                            let line = self.screen().lines[idx].as_str();

                            self.selection_start = None;
                            self.selection_range = None;
                            // TODO: allow user to configure the word boundary rules.
                            // Also consider making the default work with URLs?
                            for (x, word) in line.split_word_bound_indices() {
                                if event.x < x {
                                    break;
                                }
                                if event.x <= x + word.len() {
                                    // this is our word
                                    let start = SelectionCoordinate { x, y };
                                    let end = SelectionCoordinate {
                                        x: x + word.len() - 1,
                                        y,
                                    };
                                    self.selection_start = Some(start);
                                    self.selection_range = Some(SelectionRange { start, end });
                                    self.dirty_selection_lines();
                                    let text = self.get_selection_text();
                                    trace!(
                                        "finish 2click selection {:?} '{}'",
                                        self.selection_range, text
                                    );
                                    host.set_clipboard(Some(text))?;
                                    return Ok(());
                                }
                            }
                            host.set_clipboard(None)?;
                        }
                        // triple click to select the current line
                        Some(&LastMouseClick { streak: 3, .. }) => {
                            let y = event.y as ScrollbackOrVisibleRowIndex
                                - self.viewport_offset as ScrollbackOrVisibleRowIndex;
                            self.selection_start = Some(SelectionCoordinate { x: event.x, y });
                            self.selection_range = Some(SelectionRange {
                                start: SelectionCoordinate { x: 0, y },
                                end: SelectionCoordinate {
                                    x: usize::max_value(),
                                    y,
                                },
                            });
                            self.dirty_selection_lines();
                            let text = self.get_selection_text();
                            trace!(
                                "finish 3click selection {:?} '{}'",
                                self.selection_range, text
                            );
                            host.set_clipboard(Some(text))?;
                        }
                        // otherwise, clear out the selection
                        _ => {
                            self.selection_range = None;
                            self.selection_start = None;
                            host.set_clipboard(None)?;
                        }
                    }

                    return Ok(());
                }
                (
                    MouseEvent {
                        kind: MouseEventKind::Release,
                        button: MouseButton::Left,
                        ..
                    },
                    _,
                ) => {
                    // Finish selecting a region, update clipboard
                    self.current_mouse_button = MouseButton::None;
                    if let Some(&LastMouseClick { streak: 1, .. }) = self.last_mouse_click.as_ref()
                    {
                        // Only consider a drag selection if we have a streak==1.
                        // The double/triple click cases are handled above.
                        let text = self.get_selection_text();
                        if !text.is_empty() {
                            trace!(
                                "finish drag selection {:?} '{}'",
                                self.selection_range, text
                            );
                            host.set_clipboard(Some(text))?;
                        } else if let Some(link) = self.current_highlight() {
                            // If the button release wasn't a drag, consider
                            // whether it was a click on a hyperlink
                            host.click_link(&link);
                        }
                        return Ok(());
                    }
                }
                (
                    MouseEvent {
                        kind: MouseEventKind::Move,
                        ..
                    },
                    MouseButton::Left,
                ) => {
                    // dragging out the selection region
                    // TODO: may drag and change the viewport
                    self.dirty_selection_lines();
                    let end = SelectionCoordinate {
                        x: event.x,
                        y: event.y as ScrollbackOrVisibleRowIndex
                            - self.viewport_offset as ScrollbackOrVisibleRowIndex,
                    };
                    let sel = match self.selection_range.take() {
                        None => {
                            SelectionRange::start(self.selection_start.unwrap_or(end)).extend(end)
                        }
                        Some(sel) => sel.extend(end),
                    };
                    self.selection_range = Some(sel);
                    // Dirty lines again to reflect new range
                    self.dirty_selection_lines();
                    return Ok(());
                }
                _ => {}
            }
        }

        match event {
            MouseEvent {
                kind: MouseEventKind::Press,
                button: MouseButton::WheelUp,
                ..
            }
            | MouseEvent {
                kind: MouseEventKind::Press,
                button: MouseButton::WheelDown,
                ..
            } => {
                let (report_button, scroll_delta, key) = if event.button == MouseButton::WheelUp {
                    (64, -1, KeyCode::UpArrow)
                } else {
                    (65, 1, KeyCode::DownArrow)
                };

                if self.sgr_mouse {
                    write!(
                        host.writer(),
                        "\x1b[<{};{};{}M",
                        report_button,
                        event.x + 1,
                        event.y + 1
                    )?;
                } else if self.screen.is_alt_screen_active() {
                    // Send cursor keys instead (equivalent to xterm's alternateScroll mode)
                    self.key_down(key, KeyModifiers::default(), host)?;
                } else {
                    self.scroll_viewport(scroll_delta)
                }
            }
            MouseEvent {
                kind: MouseEventKind::Press,
                ..
            } => {
                self.current_mouse_button = event.button;
                if let Some(button) = match event.button {
                    MouseButton::Left => Some(0),
                    MouseButton::Middle => Some(1),
                    MouseButton::Right => Some(2),
                    _ => None,
                } {
                    if self.sgr_mouse {
                        write!(
                            host.writer(),
                            "\x1b[<{};{};{}M",
                            button,
                            event.x + 1,
                            event.y + 1
                        )?;
                    } else if event.button == MouseButton::Middle {
                        let clip = host.get_clipboard()?;
                        if self.bracketed_paste {
                            write!(host.writer(), "\x1b[200~{}\x1b[201~", clip)?;
                        } else {
                            write!(host.writer(), "{}", clip)?;
                        }
                    }
                }
            }
            MouseEvent {
                kind: MouseEventKind::Release,
                ..
            } => {
                if self.current_mouse_button != MouseButton::None {
                    self.current_mouse_button = MouseButton::None;
                    if self.sgr_mouse {
                        write!(host.writer(), "\x1b[<3;{};{}m", event.x + 1, event.y + 1)?;
                    }
                }
            }
            MouseEvent {
                kind: MouseEventKind::Move,
                ..
            } => {
                if let Some(button) = match (self.current_mouse_button, self.button_event_mouse) {
                    (MouseButton::Left, true) => Some(32),
                    (MouseButton::Middle, true) => Some(33),
                    (MouseButton::Right, true) => Some(34),
                    (..) => None,
                } {
                    if self.sgr_mouse {
                        write!(
                            host.writer(),
                            "\x1b[<{};{};{}M",
                            button,
                            event.x + 1,
                            event.y + 1
                        )?;
                    }
                }
            }
        }
        Ok(())
    }

    /// Processes a key_down event generated by the gui/render layer
    /// that is embedding the Terminal.  This method translates the
    /// keycode into a sequence of bytes to send to the slave end
    /// of the pty via the `Write`-able object provided by the caller.
    pub fn key_down(
        &mut self,
        key: KeyCode,
        mods: KeyModifiers,
        host: &mut TerminalHost,
    ) -> Result<(), Error> {
        const CTRL: KeyModifiers = KeyModifiers::CTRL;
        const SHIFT: KeyModifiers = KeyModifiers::SHIFT;
        const ALT: KeyModifiers = KeyModifiers::ALT;
        const NO: KeyModifiers = KeyModifiers::NONE;
        const APPCURSOR: bool = true;
        use crate::KeyCode::*;

        let ctrl = mods & CTRL;
        let shift = mods & SHIFT;
        let alt = mods & ALT;

        let mut buf = String::new();

        // TODO: also respect self.application_keypad

        if mods == KeyModifiers::SUPER && key == KeyCode::Char('n') {
            host.new_window();
            return Ok(());
        }
        if mods == KeyModifiers::SUPER && key == KeyCode::Char('t') {
            host.new_tab();
            return Ok(());
        }
        if mods == (KeyModifiers::CTRL | KeyModifiers::SHIFT)  && key == KeyCode::Char('H'){
            host.new_tab_with_cmd("less", &["-iMSx4", "-FXR", "+G", "--", "-"], true);
            return Ok(());
        }
        if mods == (KeyModifiers::SUPER | KeyModifiers::SHIFT)
            && (key == KeyCode::Char('[') || key == KeyCode::Char('{'))
        {
            host.activate_tab_relative(-1);
            return Ok(());
        }
        if mods == (KeyModifiers::SUPER | KeyModifiers::SHIFT)
            && (key == KeyCode::Char(']') || key == KeyCode::Char('}'))
        {
            host.activate_tab_relative(1);
            return Ok(());
        }

        if (mods == KeyModifiers::SUPER || mods == KeyModifiers::CTRL) && key == KeyCode::Char('-')
        {
            host.decrease_font_size();
            return Ok(());
        }
        if (mods == KeyModifiers::SUPER || mods == KeyModifiers::CTRL) && key == KeyCode::Char('=')
        {
            host.increase_font_size();
            return Ok(());
        }
        if (mods == KeyModifiers::SUPER || mods == KeyModifiers::CTRL) && key == KeyCode::Char('0')
        {
            host.reset_font_size();
            return Ok(());
        }

        if mods == KeyModifiers::SUPER {
            if let Char(c) = key {
                if c >= '0' && c <= '9' {
                    let tab_number = c as u32 - 0x30;
                    // Treat 0 as 10 as that is physically right of 9 on
                    // a keyboard
                    let tab_number = if tab_number == 0 { 10 } else { tab_number - 1 };
                    host.activate_tab(tab_number as usize);
                    return Ok(());
                }
            }
        }

        macro_rules! paste {
            () => {{
                let clip = host.get_clipboard()?;
                if self.bracketed_paste {
                    write!(buf, "\x1b[200~{}\x1b[201~", clip)?;
                } else {
                    buf = clip;
                }
                buf.as_str()
            }};
        }

        let to_send = match (key, ctrl, alt, shift, self.application_cursor_keys) {
            (Char('v'), _, ALT, _, _) => paste!(),
            (Enter, _, ALT, ..) | (Char('\r'), _, ALT, ..) | (Char('\n'), _, ALT, ..) => {
                host.toggle_full_screen();
                return Ok(());
            }
            (LeftTab, _,_,_,..) => "\x1b[Z",
            (Tab, ..) => "\t",
            (Enter, ..) => "\r",
            (Backspace, ..) => "\x08",
            (Escape, ..) => "\x1b",
            // Delete
            (Char('\x7f'), _, _, _, false) | (Delete, _, _, _, false) => "\x7f",
            (Char('\x7f'), ..) | (Delete, ..) => "\x1b[3~",

            (Insert, _, _, SHIFT, _) => paste!(),
            (Char('v'), ..) if mods == KeyModifiers::SUPER => paste!(),
            (Char('/'), CTRL, _, _, _) => {
                buf.push(('_' as u8 - 0x40) as char);
                buf.as_str()
            },
            (Char(' '), CTRL, _, _, _) | (Char('~'), CTRL, _, _, _) | (Char('@'), CTRL, _, _, _)=> "\0",
            (Char(c), CTRL, _, SHIFT, _) if c <= 0xff as char && c > 0x40 as char => {
                // If shift is held we have C == 0x43 and want to translate
                // that into 0x03
                buf.push((c as u8 - 0x40) as char);
                buf.as_str()
            }
            (Char(c), CTRL, ..) if c <= 0xff as char && c > 0x60 as char => {
                // If shift is not held we have C == 0x63 and want to translate
                // that into 0x03
                buf.push((c as u8 - 0x60) as char);
                buf.as_str()
            }
            (Char(c), _, ALT, ..) => {
                buf.push(0x1b as char);
                buf.push(c);
                buf.as_str()
            }
            (Char(c), ..) => {
                buf.push(c);
                buf.as_str()
            }

            (UpArrow, _, _, _, APPCURSOR) => "\x1bOA",
            (DownArrow, _, _, _, APPCURSOR) => "\x1bOB",
            (RightArrow, _, _, _, APPCURSOR) => "\x1bOC",
            (LeftArrow, _, _, _, APPCURSOR) => "\x1bOD",
            (Home, _, _, _, APPCURSOR) => "\x1bOH",
            (End, _, _, _, APPCURSOR) => "\x1bOF",

            (UpArrow, _, ALT, _, _) => "\x1b[1;3A",
            (DownArrow, _, ALT, _, _) => "\x1b[1;3B",
            (RightArrow, _, ALT, _, _) => "\x1b[1;3C",
            (LeftArrow, _, ALT, _, _) => "\x1b[1;3D",
            (Home, _, ALT, _, _) => "\x1b[1;3H",
            (End, _, ALT, _, _) => "\x1b[1;3F",

            (UpArrow, _, _, SHIFT, _) | (MoveLineUp, _, _, _, _) => {
                let rows = 1;
                self.scroll_viewport(-rows);
                ""
            }
            (DownArrow, _, _, SHIFT, _) | (MoveLineDown, _, _, _, _) => {
                let rows = 1;
                self.scroll_viewport(rows);
                ""
            }
            (UpArrow, ..) => "\x1b[A",
            (DownArrow, ..) => "\x1b[B",
            (RightArrow, ..) => "\x1b[C",
            (LeftArrow, ..) => "\x1b[D",

            (PageUp, _, _, SHIFT, _) => {
                let rows = self.screen().physical_rows as i64;
                self.scroll_viewport(-rows);
                ""
            }
            (PageDown, _, _, SHIFT, _) => {
                let rows = self.screen().physical_rows as i64;
                self.scroll_viewport(rows);
                ""
            }
            (PageUp, ..) => "\x1b[5~",
            (PageDown, ..) => "\x1b[6~",
            (Home, ..) => "\x1b[H",
            (End, ..) => "\x1b[F",
            (Insert, ..) => "\x1b[2~",

            (Function(n), ..) => {
                let modifier = match (ctrl, alt, shift) {
                    (NO, NO, NO) => "",
                    (NO, NO, SHIFT) => ";2",
                    (NO, ALT, NO) => ";3",
                    (NO, ALT, SHIFT) => ";4",
                    (CTRL, NO, NO) => ";5",
                    (CTRL, NO, SHIFT) => ";6",
                    (CTRL, ALT, NO) => ";7",
                    (CTRL, ALT, SHIFT) => ";8",
                    _ => unreachable!("invalid modifiers!?"),
                };

                if modifier.is_empty() && n < 5 {
                    // F1-F4 are encoded using SS3 if there are no modifiers
                    match n {
                        1 => "\x1bOP",
                        2 => "\x1bOQ",
                        3 => "\x1bOR",
                        4 => "\x1bOS",
                        _ => unreachable!("wat?"),
                    }
                } else {
                    // Higher numbered F-keys plus modified F-keys are encoded
                    // using CSI instead of SS3.
                    let intro = match n {
                        1 => "\x1b[11",
                        2 => "\x1b[12",
                        3 => "\x1b[13",
                        4 => "\x1b[14",
                        5 => "\x1b[15",
                        6 => "\x1b[17",
                        7 => "\x1b[18",
                        8 => "\x1b[19",
                        9 => "\x1b[20",
                        10 => "\x1b[21",
                        11 => "\x1b[23",
                        12 => "\x1b[24",
                        _ => bail!("unhandled fkey number {}", n),
                    };
                    write!(buf, "{}{}~", intro, modifier)?;
                    buf.as_str()
                }
            }

            // TODO: emit numpad sequences
            (Numpad0, ..) | (Numpad1, ..) | (Numpad2, ..) | (Numpad3, ..) | (Numpad4, ..)
            | (Numpad5, ..) | (Numpad6, ..) | (Numpad7, ..) | (Numpad8, ..) | (Numpad9, ..)
            | (Multiply, ..) | (Add, ..) | (Separator, ..) | (Subtract, ..) | (Decimal, ..)
            | (Divide, ..) => "",

            // Modifier keys pressed on their own don't expand to anything
            (Control, ..) | (LeftControl, ..) | (RightControl, ..) | (Alt, ..) | (LeftAlt, ..)
            | (RightAlt, ..) | (Menu, ..) | (LeftMenu, ..) | (RightMenu, ..) | (Super, ..)
            | (Hyper, ..) | (Shift, ..) | (LeftShift, ..) | (RightShift, ..) | (Meta, ..)
            | (LeftWindows, ..) | (RightWindows, ..) | (NumLock, ..) | (ScrollLock, ..) => "",

            (Cancel, ..)
            | (Clear, ..)
            | (Pause, ..)
            | (CapsLock, ..)
            | (Select, ..)
            | (Print, ..)
            | (PrintScreen, ..)
            | (Execute, ..)
            | (Help, ..)
            | (Applications, ..)
            | (Sleep, ..)
            | (BrowserBack, ..)
            | (BrowserForward, ..)
            | (BrowserRefresh, ..)
            | (BrowserStop, ..)
            | (BrowserSearch, ..)
            | (BrowserFavorites, ..)
            | (BrowserHome, ..)
            | (VolumeMute, ..)
            | (VolumeDown, ..)
            | (VolumeUp, ..)
            | (MediaNextTrack, ..)
            | (MediaPrevTrack, ..)
            | (MediaStop, ..)
            | (MediaPlayPause, ..)
            | (InternalPasteStart, ..)
            | (InternalPasteEnd, ..) => "",
        };

        // eprintln!("sending {:?}", to_send);
        write_all(host.writer(), to_send.as_bytes())?;

        // Reset the viewport if we sent data to the parser
        if !to_send.is_empty() && self.viewport_offset != 0 {
            // TODO: some folks like to configure this behavior.
            self.set_scroll_viewport(0);
        }

        Ok(())
    }

    pub fn key_up(
        &mut self,
        _: KeyCode,
        _: KeyModifiers,
        _: &mut TerminalHost,
    ) -> Result<(), Error> {
        Ok(())
    }

    pub fn resize(&mut self, physical_rows: usize, physical_cols: usize) {
        self.screen.resize(physical_rows, physical_cols);
        self.scroll_region = 0..physical_rows as i64;
        self.tabs.resize(physical_cols);
        self.set_scroll_viewport(0);
        // Ensure that the cursor is within the new bounds of the screen
        self.set_cursor_pos(&Position::Relative(0), &Position::Relative(0));
    }

    /// Returns true if any of the visible lines are marked dirty
    pub fn has_dirty_lines(&mut self) -> bool {
        let offset = self.get_viewport_offset();
        if ! self.screen.is_alt_screen_active(){
            self.screen_mut().fill_lines(offset as i32);
        }
        let screen = self.screen();

        for line in screen.lines.iter() {
            if line.is_dirty() {
                return true;
            }
        }

        false
    }

    /// Returns the set of visible lines that are dirty.
    /// The return value is a Vec<(line_idx, line, selrange)>, where
    /// line_idx is relative to the top of the viewport.
    /// The selrange value is the column range representing the selected
    /// columns on this line.
    pub fn get_dirty_lines(&self) -> Vec<(usize, &Line, Range<usize>)> {
        let mut res = Vec::new();

        let screen = self.screen();
        let height = screen.physical_rows;

        let selection = self.selection_range.map(|r| r.normalize());
        for (i, mut line) in screen.lines.iter().enumerate() {
            if i >= height {
                // When scrolling back, make sure we don't emit lines that
                // are below the bottom of the viewport
                break;
            }
            if line.is_dirty() {
                let selrange = match selection {
                    None => 0..0,
                    Some(sel) => {
                        // i is relative to the viewport, convert it back to
                        // something we can relate to the selection
                        let row = (i as ScrollbackOrVisibleRowIndex)
                            - self.viewport_offset as ScrollbackOrVisibleRowIndex;
                        sel.cols_for_row(row)
                    }
                };
                res.push((i, &*line, selrange));
            }
        }

        res
    }

    pub fn get_viewport_offset(&self) -> VisibleRowIndex {
        self.viewport_offset
    }

    /// Clear the dirty flag for all dirty lines
    pub fn clean_dirty_lines(&mut self) {
        let screen = self.screen_mut();
        for line in &mut screen.lines {
            line.clear_dirty();
        }
    }

    /// When dealing with selection, mark a range of lines as dirty
    pub fn make_all_lines_dirty(&mut self) {
        let screen = self.screen_mut();
        for line in &mut screen.lines {
            line.set_dirty();
        }
    }

    /// Returns the 0-based cursor position relative to the top left of
    /// the visible screen
    pub fn cursor_pos(&self) -> CursorPosition {
        // TODO: figure out how to expose cursor visibility; Option<CursorPosition>?
        CursorPosition {
            x: self.cursor.x,
            y: self.cursor.y + self.viewport_offset,
        }
    }

    /// Returns the currently highlighted hyperlink
    pub fn current_highlight(&self) -> Option<Rc<Hyperlink>> {
        self.current_highlight.as_ref().cloned()
    }

    /// Sets the cursor position. x and y are 0-based and relative to the
    /// top left of the visible screen.
    /// TODO: DEC origin mode impacts the interpreation of these
    fn set_cursor_pos(&mut self, x: &Position, y: &Position) {
        let x = match *x {
            Position::Relative(x) => (self.cursor.x as i64 + x).max(0),
            Position::Absolute(x) => x,
        };
        let y = match *y {
            Position::Relative(y) => (self.cursor.y + y).max(0),
            Position::Absolute(y) => y,
        };

        let rows = self.screen().physical_rows;
        let cols = self.screen().physical_cols;
        let old_y = self.cursor.y;
        let new_y = y.min(rows as i64 - 1);

        self.cursor.x = x.min(cols as i64 - 1) as usize;
        self.cursor.y = new_y;
        self.wrap_next = false;

        let screen = self.screen_mut();
        screen.dirty_line(old_y);
        screen.dirty_line(new_y);
    }

    fn set_scroll_viewport(&mut self, position: VisibleRowIndex) {
        self.clear_selection();
        let position = position.max(0);

        let width = self.screen().physical_cols;
        let rows = self.screen().physical_rows;
        let avail_scrollback = self.screen().hlines.len() - rows;

        let position = position.min(avail_scrollback as i64);

        if self.viewport_offset != position {
            for y in 0..rows{
                self.screen_mut().line_at(y).resize_and_clear(width);
            }
        }
        trace!("{}: position:{}", "set_scroll_viewport", position);
        self.viewport_offset = position;
        let top = self.screen().hlines.len() - (rows + position as usize);
        {
            let screen = self.screen_mut();
            for y in top..top + rows {
                screen.hline_mut(y).set_dirty();
            }
        }
        //self.recompute_highlight();
    }

    /// Adjust the scroll position of the viewport by delta.
    /// Dirties the lines that are now in view.
    pub fn scroll_viewport(&mut self, delta: VisibleRowIndex) {
        let position = self.viewport_offset - delta;
        self.set_scroll_viewport(position);
    }

    fn scroll_up(&mut self, num_rows: usize) {
        self.clear_selection();
        let scroll_region = self.scroll_region.clone();
        self.screen_mut().scroll_up(&scroll_region, num_rows)
    }

    fn scroll_down(&mut self, num_rows: usize) {
        self.clear_selection();
        let scroll_region = self.scroll_region.clone();
        self.screen_mut().scroll_down(&scroll_region, num_rows)
    }

    fn new_line(&mut self, move_to_first_column: bool) {
        let x = if move_to_first_column {
            0
        } else {
            self.cursor.x
        };
        let y = self.cursor.y;
        let y = if y == self.scroll_region.end - 1 {
            self.scroll_up(1);
            y
        } else {
            y + 1
        };
        self.set_cursor_pos(&Position::Absolute(x as i64), &Position::Absolute(y as i64));
    }

    /// Moves the cursor down one line in the same column.
    /// If the cursor is at the bottom margin, the page scrolls up.
    fn c1_index(&mut self) {
        let y = self.cursor.y;
        let y = if y == self.scroll_region.end - 1 {
            self.scroll_up(1);
            y
        } else {
            y + 1
        };
        self.set_cursor_pos(&Position::Relative(0), &Position::Absolute(y as i64));
    }

    /// Moves the cursor to the first position on the next line.
    /// If the cursor is at the bottom margin, the page scrolls up.
    fn c1_nel(&mut self) {
        self.new_line(true);
    }

    /// Sets a horizontal tab stop at the column where the cursor is.
    fn c1_hts(&mut self) {
        self.tabs.set_tab_stop(self.cursor.x);
    }

    /// Moves the cursor to the next tab stop. If there are no more tab stops,
    /// the cursor moves to the right margin. HT does not cause text to auto
    /// wrap.
    fn c0_horizontal_tab(&mut self) {
        let x = match self.tabs.find_next_tab_stop(self.cursor.x) {
            Some(x) => x,
            None => self.screen().physical_cols - 1,
        };
        self.set_cursor_pos(&Position::Absolute(x as i64), &Position::Relative(0));
    }

    /// Move the cursor up 1 line.  If the position is at the top scroll margin,
    /// scroll the region down.
    fn c1_reverse_index(&mut self) {
        let y = self.cursor.y;
        let y = if y == self.scroll_region.start {
            self.scroll_down(1);
            y
        } else {
            y - 1
        };
        self.set_cursor_pos(&Position::Relative(0), &Position::Absolute(y as i64));
    }

    fn set_hyperlink(&mut self, link: Option<Hyperlink>) {
        self.pen.hyperlink = match link {
            Some(hyperlink) => Some(Rc::new(hyperlink)),
            None => None,
        }
    }

    fn set_image(&mut self, image: ITermFileData) {
        if !image.inline {
            eprintln!(
                "Ignoring file download request name={:?} size={}",
                image.name,
                image.data.len()
            );
            return;
        }

        // Decode the image data
        let decoded_image = match image::load_from_memory(&image.data) {
            Ok(im) => im,
            Err(e) => {
                eprintln!(
                    "Unable to decode image: {}: size={} {:?}",
                    e,
                    image.data.len(),
                    image
                );
                return;
            }
        };

        // Figure out the dimensions.
        // TODO: we need to understand pixels here, and we don't today,
        // so "guess" using the values that I see in my setup.
        let cell_pixel_width = 8;
        let cell_pixel_height = 15;

        let width = image
            .width
            .to_pixels(cell_pixel_width, self.screen().physical_cols);
        let height = image
            .height
            .to_pixels(cell_pixel_height, self.screen().physical_rows);

        // Compute any Automatic dimensions
        let (width, height) = match (width, height) {
            (None, None) => (
                decoded_image.width() as usize,
                decoded_image.height() as usize,
            ),
            (Some(w), None) => {
                let scale = decoded_image.width() as f32 / w as f32;
                let h = decoded_image.height() as f32 * scale;
                (w, h as usize)
            }
            (None, Some(h)) => {
                let scale = decoded_image.height() as f32 / h as f32;
                let w = decoded_image.width() as f32 * scale;
                (w as usize, h)
            }
            (Some(w), Some(h)) => (w, h),
        };

        let width_in_cells = width / cell_pixel_width;
        let height_in_cells = height / cell_pixel_height;

        let available_pixel_width = width_in_cells * cell_pixel_width;
        let available_pixel_height = height_in_cells * cell_pixel_height;

        // TODO: defer this to the actual renderer
        /*
        let resized_image = if image.preserve_aspect_ratio {
            let resized = decoded_image.resize(
                available_pixel_width as u32,
                available_pixel_height as u32,
                image::FilterType::Lanczos3,
            );
            // Pad with black bars to preserve aspect ratio
            // Assumption: new_rgba8 returns black/transparent pixels by default.
            let dest = DynamicImage::new_rgba8(available_pixel_width, available_pixel_height);
            dest.copy_from(resized, 0, 0);
            dest
        } else {
            decoded_image.resize_exact(
                available_pixel_width as u32,
                available_pixel_height as u32,
                image::FilterType::Lanczos3,
            )
        };
        */

        let image_data = Rc::new(ImageData::with_raw_data(image.data));

        let mut ypos = NotNaN::new(0.0).unwrap();
        let cursor_x = self.cursor.x;
        let cursor_y = self.cursor.y;
        let x_delta = 1.0 / available_pixel_width as f32;
        let y_delta = 1.0 / available_pixel_height as f32;
        eprintln!(
            "image is {}x{} cells, {}x{} pixels",
            width_in_cells, height_in_cells, width, height
        );
        for y in 0..height_in_cells {
            let mut xpos = NotNaN::new(0.0).unwrap();
            for x in 0..width_in_cells {
                self.screen_mut().set_cell(
                    cursor_x + x,
                    cursor_y + y as VisibleRowIndex,
                    &Cell::new(
                        ' ',
                        CellAttributes::default()
                            .set_image(Some(Box::new(ImageCell::new(
                                TextureCoordinate::new(xpos, ypos),
                                TextureCoordinate::new(
                                    xpos + cell_pixel_width as f32,
                                    ypos + cell_pixel_height as f32,
                                ),
                                image_data.clone(),
                            ))))
                            .clone(),
                    ),
                );
                xpos += x_delta;
            }
            ypos += y_delta;
        }

        // FIXME: check cursor positioning in iterm
        self.set_cursor_pos(
            &Position::Relative(width_in_cells as i64),
            &Position::Relative(0),
        );
    }

    fn perform_device(&mut self, dev: Device, host: &mut TerminalHost) {
        trace!(target: "perform_device", "perform_device: {}", dev);
        match dev {
            Device::DeviceAttributes(a) => eprintln!("unhandled: {:?}", a),
            Device::SoftReset => {
                self.pen = CellAttributes::default();
                // TODO: see https://vt100.net/docs/vt510-rm/DECSTR.html
            }
            Device::RequestPrimaryDeviceAttributes => {
                host.writer().write(DEVICE_IDENT).ok();
            }
            Device::RequestSecondaryDeviceAttributes => {
                host.writer().write(b"\x1b[>0;0;0c").ok();
            }
            Device::StatusReport => {
                host.writer().write(b"\x1b[0n").ok();
            }
        }
    }

    fn perform_csi_mode(&mut self, mode: Mode) {
        trace!(target: "perform_csi_mode", "mode: {:?}", mode);
        match mode {
            Mode::SetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::StartBlinkingCursor,
            ))
            | Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::StartBlinkingCursor,
            )) => {}

            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::BracketedPaste)) => {
                self.bracketed_paste = true;
            }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::BracketedPaste)) => {
                self.bracketed_paste = false;
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ApplicationCursorKeys,
            )) => {
                self.application_cursor_keys = true;
            }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ApplicationCursorKeys,
            )) => {
                self.application_cursor_keys = false;
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::ShowCursor)) => {
                self.cursor_visible = true;
            }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::ShowCursor)) => {
                self.cursor_visible = false;
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::MouseTracking))
            | Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::MouseTracking)) => {
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::HighlightMouseTracking,
            ))
            | Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::HighlightMouseTracking,
            )) => {}

            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::ButtonEventMouse)) => {
                self.button_event_mouse = true;
            }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ButtonEventMouse,
            )) => {
                self.button_event_mouse = false;
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::AnyEventMouse))
            | Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::AnyEventMouse)) => {
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::SGRMouse)) => {
                self.sgr_mouse = true;
            }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::SGRMouse)) => {
                self.sgr_mouse = false;
            }

            Mode::SetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ClearAndEnableAlternateScreen,
            )) => {
                if !self.screen.is_alt_screen_active() {
                    self.save_cursor();
                    self.screen.activate_alt_screen();
                    self.set_cursor_pos(&Position::Absolute(0), &Position::Absolute(0));
                    self.erase_in_display(EraseInDisplay::EraseDisplay);
                    self.set_scroll_viewport(0);
                }
            }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(
                DecPrivateModeCode::ClearAndEnableAlternateScreen,
            )) => {
                if self.screen.is_alt_screen_active() {
                    self.screen.activate_primary_screen();
                    self.restore_cursor();
                    self.set_scroll_viewport(0);
                }
            }
            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::LineWrap))
              => {
                self.linewrap = true;
              }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::LineWrap))
              => {
                self.linewrap = false;
              }
            Mode::SetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::LineFeedNewLine))
              => {
                self.lf_nl = true;
              }
            Mode::ResetDecPrivateMode(DecPrivateMode::Code(DecPrivateModeCode::LineFeedNewLine))
              => {
                self.lf_nl = false;
              }

            Mode::SaveDecPrivateMode(DecPrivateMode::Code(_))
            | Mode::RestoreDecPrivateMode(DecPrivateMode::Code(_)) => {
                eprintln!("save/restore dec mode unimplemented")
            }


            Mode::SetDecPrivateMode(DecPrivateMode::Unspecified(n))
            | Mode::ResetDecPrivateMode(DecPrivateMode::Unspecified(n))
            | Mode::SaveDecPrivateMode(DecPrivateMode::Unspecified(n))
            | Mode::RestoreDecPrivateMode(DecPrivateMode::Unspecified(n)) => {
                eprintln!("unhandled DecPrivateMode {}", n);
            }
        }
    }

    fn erase_in_display(&mut self, erase: EraseInDisplay) {
        let cy = self.cursor.y;
        let pen = self.pen.clone_sgr_only();
        let rows = self.screen().physical_rows as VisibleRowIndex;
        let col_range = 0..usize::max_value();
        let row_range = match erase {
            EraseInDisplay::EraseToEndOfDisplay => {
                self.perform_csi_edit(Edit::EraseInLine(EraseInLine::EraseToEndOfLine));
                cy + 1..rows
            }
            EraseInDisplay::EraseToStartOfDisplay => {
                self.perform_csi_edit(Edit::EraseInLine(EraseInLine::EraseToStartOfLine));
                0..cy
            }
            EraseInDisplay::EraseDisplay => 0..rows,
            EraseInDisplay::EraseScrollback => {
                eprintln!("TODO: ed: no support for xterm Erase Saved Lines yet");
                return;
            }
        };

        {
            let alt_screen = self.screen.is_alt_screen_active();
            let screen = self.screen_mut();
            for y in row_range.clone() {
                if alt_screen{
                    screen.clear_line(y, col_range.clone(), &pen);
                } else {
                    screen.clear_line_hack(y, col_range.clone(), &pen);
                }
            }
        }

        for y in row_range {
            if self
                .clear_selection_if_intersects(col_range.clone(), y as ScrollbackOrVisibleRowIndex)
            {
                break;
            }
        }
    }

    fn perform_csi_edit(&mut self, edit: Edit) {
        trace!(target: "perform_csi_edit", "perform_csi_edit: {:?}", edit);
        match edit {
            Edit::DeleteCharacter(n) => {
                let y = self.cursor.y;
                let x = self.cursor.x;
                let limit = (x + n as usize).min(self.screen().physical_cols);
                {
                    let screen = self.screen_mut();
                    for _ in x..limit as usize {
                        screen.erase_cell(x, y);
                    }
                }
                self.clear_selection_if_intersects(x..limit, y as ScrollbackOrVisibleRowIndex);
            }
            Edit::DeleteLine(n) => {
                if in_range(self.cursor.y, &self.scroll_region) {
                    let scroll_region = self.cursor.y..self.scroll_region.end;
                    self.screen_mut().scroll_up(&scroll_region, n as usize);

                    let scrollback_region = self.cursor.y as ScrollbackOrVisibleRowIndex
                        ..self.scroll_region.end as ScrollbackOrVisibleRowIndex;
                    self.clear_selection_if_intersects_rows(scrollback_region);
                }
            }
            Edit::EraseCharacter(n) => {
                let y = self.cursor.y;
                let x = self.cursor.x;
                trace!(target: "perform_csi_edit", "Erase Character: {:?}", (self.cursor));
                let limit = (x + n as usize).min(self.screen().physical_cols);
                {
                    let screen = self.screen_mut();
                    let blank = Cell::default();
                    for x in x..limit as usize {
                        screen.set_cell(x, y, &blank);
                    }
                }
                self.clear_selection_if_intersects(x..limit, y as ScrollbackOrVisibleRowIndex);
            }

            Edit::EraseInLine(erase) => {
                let cx = self.cursor.x;
                let cy = self.cursor.y;
                let pen = self.pen.clone_sgr_only();
                let cols = self.screen().physical_cols;
                let range = match erase {
                    EraseInLine::EraseToEndOfLine => cx..cols,
                    EraseInLine::EraseToStartOfLine => 0..cx,
                    EraseInLine::EraseLine => 0..cols,
                };

                if self.screen.is_alt_screen_active(){
                    self.screen_mut().clear_line(cy, range.clone(), &pen);
                } else {
                    self.screen_mut().clear_line_hack(cy, range.clone(), &pen);
                }
                self.clear_selection_if_intersects(range, cy as ScrollbackOrVisibleRowIndex);
            }
            Edit::InsertCharacter(n) => {
                let y = self.cursor.y;
                let x = self.cursor.x;
                // TODO: this limiting behavior may not be correct.  There's also a
                // SEM sequence that impacts the scope of ICH and ECH to consider.
                let limit = (x + n as usize).min(self.screen().physical_cols);
                {
                    let screen = self.screen_mut();
                    for x in x..limit as usize {
                        screen.insert_cell(x, y);
                    }
                }
                self.clear_selection_if_intersects(x..limit, y as ScrollbackOrVisibleRowIndex);
            }
            Edit::InsertLine(n) => {
                if in_range(self.cursor.y, &self.scroll_region) {
                    let scroll_region = self.cursor.y..self.scroll_region.end;
                    self.screen_mut().scroll_down(&scroll_region, n as usize);

                    let scrollback_region = self.cursor.y as ScrollbackOrVisibleRowIndex
                        ..self.scroll_region.end as ScrollbackOrVisibleRowIndex;
                    self.clear_selection_if_intersects_rows(scrollback_region);
                }
            }
            Edit::ScrollDown(n) => self.scroll_down(n as usize),
            Edit::ScrollUp(n) => self.scroll_up(n as usize),
            Edit::EraseInDisplay(erase) => self.erase_in_display(erase),
        }
    }

    fn perform_csi_cursor(&mut self, cursor: Cursor, host: &mut TerminalHost) {
        trace!(target: "perform_csi_cursor", "perform_csi_cursor: {:?}", cursor);
        match cursor {
            Cursor::SetTopAndBottomMargins { top, bottom } => {
                let rows = self.screen().physical_rows;
                let mut top = (top as i64).saturating_sub(1).min(rows as i64 - 1).max(0);
                let mut bottom = (bottom as i64)
                    .saturating_sub(1)
                    .min(rows as i64 - 1)
                    .max(0);
                if top > bottom {
                    std::mem::swap(&mut top, &mut bottom);
                }
                self.scroll_region = top..bottom + 1;
            }
            Cursor::ForwardTabulation(n) => {
                for _ in 0..n {
                    self.c0_horizontal_tab();
                }
            }
            Cursor::BackwardTabulation(_) => {}
            Cursor::TabulationClear(_) => {}
            Cursor::TabulationControl(_) => {}
            Cursor::LineTabulation(_) => {}

            Cursor::Left(n) => {
                self.set_cursor_pos(&Position::Relative(-(n as i64)), &Position::Relative(0))
            }
            Cursor::Right(n) => {
                self.set_cursor_pos(&Position::Relative(n as i64), &Position::Relative(0))
            }
            Cursor::Up(n) => {
                self.set_cursor_pos(&Position::Relative(0), &Position::Relative(-(n as i64)))
            }
            Cursor::Down(n) => {
                self.set_cursor_pos(&Position::Relative(0), &Position::Relative(n as i64))
            }
            Cursor::CharacterAndLinePosition { line, col } | Cursor::Position { line, col } => self
                .set_cursor_pos(
                    &Position::Absolute((col as i64).saturating_sub(1)),
                    &Position::Absolute((line as i64).saturating_sub(1)),
                ),
            Cursor::CharacterAbsolute(col) | Cursor::CharacterPositionAbsolute(col) => self
                .set_cursor_pos(
                    &Position::Absolute((col as i64).saturating_sub(1)),
                    &Position::Relative(0),
                ),
            Cursor::CharacterPositionBackward(col) => {
                self.set_cursor_pos(&Position::Relative(-(col as i64)), &Position::Relative(0))
            }
            Cursor::CharacterPositionForward(col) => {
                self.set_cursor_pos(&Position::Relative(col as i64), &Position::Relative(0))
            }
            Cursor::LinePositionAbsolute(line) => self.set_cursor_pos(
                &Position::Relative(0),
                &Position::Absolute((line as i64).saturating_sub(1)),
            ),
            Cursor::LinePositionBackward(line) => {
                self.set_cursor_pos(&Position::Relative(0), &Position::Relative(-(line as i64)))
            }
            Cursor::LinePositionForward(line) => {
                self.set_cursor_pos(&Position::Relative(0), &Position::Relative(line as i64))
            }
            Cursor::NextLine(n) => {
                for _ in 0..n {
                    self.new_line(true);
                }
            }
            Cursor::PrecedingLine(n) => {
                self.set_cursor_pos(&Position::Absolute(0), &Position::Relative(-(n as i64)))
            }
            Cursor::ActivePositionReport { .. } => {
                // This is really a response from the terminal, and
                // we don't need to process it as a terminal command
            }
            Cursor::RequestActivePositionReport => {
                let line = self.cursor.y as u32 + 1;
                let col = self.cursor.x as u32 + 1;
                let report = CSI::Cursor(Cursor::ActivePositionReport { line, col });
                write!(host.writer(), "{}", report).ok();
            }
            Cursor::SaveCursor => self.save_cursor(),
            Cursor::RestoreCursor => self.restore_cursor(),
            Cursor::CursorStyle(style) => eprintln!("unhandled: CursorStyle {:?}", style),
        }
    }

    fn save_cursor(&mut self) {
        self.saved_cursor = self.cursor;
    }
    fn restore_cursor(&mut self) {
        let x = self.saved_cursor.x;
        let y = self.saved_cursor.y;
        self.set_cursor_pos(&Position::Absolute(x as i64), &Position::Absolute(y));
    }

    fn perform_csi_sgr(&mut self, sgr: Sgr) {
        //trace!(target: "perform_csi_sgr", "{:?}", sgr);
        match sgr {
            Sgr::Reset => {
                let link = self.pen.hyperlink.take();
                self.pen = CellAttributes::default();
                self.pen.hyperlink = link;
            }
            Sgr::Intensity(intensity) => {
                self.pen.set_intensity(intensity);
            }
            Sgr::Underline(underline) => {
                self.pen.set_underline(underline);
            }
            Sgr::Blink(blink) => {
                self.pen.set_blink(blink);
            }
            Sgr::Italic(italic) => {
                self.pen.set_italic(italic);
            }
            Sgr::Inverse(inverse) => {
                self.pen.set_reverse(inverse);
            }
            Sgr::Invisible(invis) => {
                self.pen.set_invisible(invis);
            }
            Sgr::StrikeThrough(strike) => {
                self.pen.set_strikethrough(strike);
            }
            Sgr::Foreground(col) => {
                self.pen.set_foreground(col);
            }
            Sgr::Background(col) => {
                self.pen.set_background(col);
            }
            Sgr::Font(_) => {}
        }
    }
}

use termwiz::escape::{DeviceControlMode};
use termwiz::{num, vte};

/// A helper struct for implementing `vte::Perform` while compartmentalizing
/// the terminal state and the embedding/host terminal interface
pub(crate) struct Performer<'a> {
    pub state: &'a mut TerminalState,
    pub host: &'a mut TerminalHost,
    print: Option<Vec<char>>,
}

pub struct Parser {
    pub state_machine: vte::Parser,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            state_machine: vte::Parser::new(),
        }
    }
    pub fn parse<P: vte::Perform>(&mut self, bytes: &[u8], mut p:  P) {
        for b in bytes {
            self.state_machine.advance(&mut p, *b);
        }
    }
}

impl<'a> Deref for Performer<'a> {
    type Target = TerminalState;

    fn deref(&self) -> &TerminalState {
        self.state
    }
}

impl<'a> DerefMut for Performer<'a> {
    fn deref_mut(&mut self) -> &mut TerminalState {
        &mut self.state
    }
}

impl<'a> Drop for Performer<'a> {
    fn drop(&mut self) {
        self.flush_print();
    }
}

impl<'a> Performer<'a> {
    pub fn new(state: &'a mut TerminalState, host: &'a mut TerminalHost) -> Self {
        Self {
            state,
            host,
            print: None,
        }
    }

    fn flush_print(&mut self) {
        //self.flush_count = self.flush_count.wrapping_add(1);
        // if self.flush_count % 10 != 0{
        //     return
        // }
        let p = match self.print.take() {
            Some(s) => s,
            None => return,
        };
        let pen = self.pen.clone();

        //use std::iter::FromIterator;
        let width = self.screen().physical_cols;
        //let s = String::from_iter(p);
        if self.screen.is_alt_screen_active() {
            let mut it = p.iter();
            while let Some(mut g) = it.next()  //unicode_segmentation::UnicodeSegmentation::graphemes(s.as_str(), true).map(|x| x.chars()).flatten()
            {
                if self.wrap_next { // [TODO] handle self.linewrap
                    self.new_line(true);
                    self.wrap_next = false;
                }

                let active_charset = &self.active_charset.clone();
                let y = self.cursor.y;

                let mut x = self.cursor.x;
                let mut print_width =0;
                {
                    let line = self.screen_mut().line_at(y as usize);


                    // Assign the cell and extract its printable width

                    let mut has_next = true;
                    while x+print_width < width && has_next{
                        x+=print_width;
                        let (_, pw) = line
                            .update_or_set_cell(x, active_charset.map(*g), pen.clone());
                        print_width = pw;
                        if let Some(g1) = it.next(){
                            g = g1;
                        } else {
                            has_next=false;
                        };
                    }
                }                // the max(1) here is to ensure that we advance to the next cell
                self.cursor.x = x;
                // position for zero-width graphemes.  We want to make sure that
                // they occupy a cell so that we can re-emit them when we output them.
                // If we didn't do this, then we'd effectively filter them out from
                // the model, which seems like a lossy design choice.
                // cell.width().max(1)
                // width
                //};

                // self.clear_selection_if_intersects(
                //     x..x + print_width,
                //     y as ScrollbackOrVisibleRowIndex,
                // );

                if x + print_width < width {
                    self.cursor.x += print_width;
                } else {
                    self.wrap_next = true;
                }
            }
        } else {
            let y = self.cursor.y;
            let x = self.cursor.x;
            let len = p.len();
            self.screen_mut().hline_at(y).push(LineHisto::new(p, pen, x));
            self.cursor.x = (x + len).min(width);
        }


    }

    pub fn perform(&mut self, action: Action) {
        match action {
            Action::Print(c) => self.print(c),
            Action::Control(code) => self.control(code),
            Action::DeviceControl(ctrl) => eprintln!("Unhandled {:?}", ctrl),
            Action::OperatingSystemCommand(osc) => self.osc_dispatch(*osc),
            Action::Esc(esc) => self.esc_dispatch(esc),
            Action::CSI(csi) => self.csi_dispatch(csi),
        }
    }

    /// Draw a character to the screen
    fn print(&mut self, c: char) {
        // We buffer up the chars to increase the chances of correctly grouping graphemes into cells
        let size = self.screen().physical_cols;
        //let c = self.active_charset.map(c);
        self.print.get_or_insert_with(|| Vec::with_capacity(size)).push(c);
    }

    fn control(&mut self, control: ControlCode) {
        self.flush_print();
        trace!(target: "perform_control", "perform_control {:?}", control);
        match control {
            ControlCode::LineFeed | ControlCode::VerticalTab | ControlCode::FormFeed => {
                let lf_nl = self.lf_nl;
                self.new_line(lf_nl /* TODO: depend on terminal mode */)
            }
            ControlCode::CarriageReturn => {
                self.set_cursor_pos(&Position::Absolute(0), &Position::Relative(0));
            }
            ControlCode::Backspace => {
                self.set_cursor_pos(&Position::Relative(-1), &Position::Relative(0));
            }
            ControlCode::HorizontalTab => self.c0_horizontal_tab(),
            ControlCode::Bell => eprintln!("Ding! (this is the bell)"),
            ControlCode::ShiftOut => self.active_charset = Charset::SpecialCharacterAndLineDrawing,
            ControlCode::ShiftIn => dbg!(self.active_charset = Charset::USAscii),
            _ => println!("unhandled ControlCode {:?}", control),
        }
    }

    fn csi_dispatch(&mut self, csi: CSI) {
        self.flush_print();
        match csi {
            CSI::Sgr(sgr) => self.state.perform_csi_sgr(sgr),
            CSI::Cursor(cursor) => self.state.perform_csi_cursor(cursor, self.host),
            CSI::Edit(edit) => self.state.perform_csi_edit(edit),
            CSI::Mode(mode) => self.state.perform_csi_mode(mode),
            CSI::Device(dev) => self.state.perform_device(*dev, self.host),
            CSI::Mouse(mouse) => eprintln!("mouse report sent by app? {:?}", mouse),
            CSI::Unspecified(unspec) => {
                eprintln!("unknown unspecified CSI: {:?}", format!("{}", unspec))
            }
        };
    }

    fn esc_dispatch(&mut self, esc: Esc) {
        self.flush_print();
        match esc {
            Esc::Code(EscCode::StringTerminator) => {
                // String Terminator (ST); explicitly has nothing to do here, as its purpose is
                // handled by vte::Parser
            }
            Esc::Code(EscCode::DecApplicationKeyPad) => {
                trace!("DECKPAM on");
                self.application_keypad = true;
            }
            Esc::Code(EscCode::DecNormalKeyPad) => {
                trace!("DECKPAM off");
                self.application_keypad = false;
            }
            Esc::Code(EscCode::ReverseIndex) => self.c1_reverse_index(),
            Esc::Code(EscCode::Index) => self.c1_index(),
            Esc::Code(EscCode::NextLine) => self.c1_nel(),
            Esc::Code(EscCode::HorizontalTabSet) => self.c1_hts(),
            Esc::Code(EscCode::DecLineDrawing) => self.active_charset = Charset::SpecialCharacterAndLineDrawing,
            Esc::Code(EscCode::AsciiCharacterSet) => self.active_charset = Charset::USAscii,
            Esc::Code(EscCode::DecSaveCursorPosition) => self.save_cursor(),
            Esc::Code(EscCode::DecRestoreCursorPosition) => self.restore_cursor(),
            _ => println!("ESC: unhandled {:?}", esc),
        }
    }

    fn osc_dispatch(&mut self, osc: OperatingSystemCommand) {
        self.flush_print();
        match osc {
            OperatingSystemCommand::SetIconNameAndWindowTitle(title)
            | OperatingSystemCommand::SetWindowTitle(title) => {
                self.title = title.clone();
                self.host.set_title(&title);
            }
            OperatingSystemCommand::SetIconName(_) => {}
            OperatingSystemCommand::SetHyperlink(link) => {
                self.set_hyperlink(link);
            }
            OperatingSystemCommand::Unspecified(unspec) => {
                eprint!("Unhandled OSC ");
                for item in unspec {
                    eprint!(" {}", String::from_utf8_lossy(&item));
                }
                eprintln!("");
            }

            OperatingSystemCommand::ClearSelection(_) => {
                self.host.set_clipboard(None).ok();
            }
            OperatingSystemCommand::QuerySelection(_) => {}
            OperatingSystemCommand::SetSelection(_, selection_data) => match self
                .host
                .set_clipboard(Some(selection_data))
            {
                Ok(_) => (),
                Err(err) => eprintln!("failed to set clipboard in response to OSC 52: {:?}", err),
            },
            OperatingSystemCommand::ITermProprietary(iterm) => match iterm {
                ITermProprietary::File(image) => self.set_image(*image),
                _ => eprintln!("unhandled iterm2: {:?}", iterm),
            },
            OperatingSystemCommand::SystemNotification(message) => {
                eprintln!("Application sends SystemNotification: {}", message);
            }
        }
    }
}

impl<'a> vte::Perform for Performer<'a> {
    fn print(&mut self, c: char) {
        self.print(c);
    }

    fn execute(&mut self, byte: u8) {
        match num::FromPrimitive::from_u8(byte) {
            Some(code) => self.control(code),
            None => eprintln!("impossible C0/C1 control code {:?} was dropped", byte),
        }
    }

    fn hook(&mut self, params: &[i64], intermediates: &[u8], ignored_extra_intermediates: bool) {
        self.perform(Action::DeviceControl(Box::new(DeviceControlMode::Enter {
            params: params.to_vec(),
            intermediates: intermediates.to_vec(),
            ignored_extra_intermediates,
        })));
    }

    fn put(&mut self, data: u8) {
        self.perform(Action::DeviceControl(Box::new(DeviceControlMode::Data(
            data,
        ))));
    }

    fn unhook(&mut self) {
        self.perform(Action::DeviceControl(Box::new(DeviceControlMode::Exit)));
    }

    fn osc_dispatch(&mut self, osc: &[&[u8]]) {
        let osc = OperatingSystemCommand::parse(osc);
        self.perform(Action::OperatingSystemCommand(Box::new(osc)));
    }

    fn csi_dispatch(
        &mut self,
        params: &[i64],
        intermediates: &[u8],
        ignored_extra_intermediates: bool,
        control: char,
    ) {
        for action in CSI::parse(params, intermediates, ignored_extra_intermediates, control) {
            self.perform(Action::CSI(action));
        }
    }

    fn esc_dispatch(
        &mut self,
        _params: &[i64],
        intermediates: &[u8],
        _ignored_extra_intermediates: bool,
        control: u8,
    ) {
        // It doesn't appear to be possible for params.len() > 1 due to the way
        // that the state machine in vte functions.  As such, it also seems to
        // be impossible for ignored_extra_intermediates to be true too.
        self.perform(Action::Esc(Esc::parse(
            if intermediates.len() == 1 {
                Some(intermediates[0])
            } else {
                None
            },
            control,
        )));
    }
}
