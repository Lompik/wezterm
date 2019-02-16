use super::*;

pub struct ScreenOrAlt {
    /// The primary screen + scrollback
    screen: Screen,
    /// The alternate screen; no scrollback
    alt_screen: Screen,
    /// Tells us which screen is active
    alt_screen_is_active: bool,
}

impl Deref for ScreenOrAlt {
    type Target = Screen;

    fn deref(&self) -> &Screen {
        match self.alt_screen_is_active {
            true => &self.alt_screen,
            false => &self.screen,
        }
    }
}

impl DerefMut for ScreenOrAlt {
    fn deref_mut(&mut self) -> &mut Screen {
        match self.alt_screen_is_active {
            true => &mut self.alt_screen,
            false => &mut self.screen,
        }
    }
}

impl ScreenOrAlt {
    pub fn new(physical_rows: usize, physical_cols: usize, scrollback_size: usize) -> Self {
        let screen = Screen::new(physical_rows, physical_cols, scrollback_size, ScreenType::Primary);
        let alt_screen = Screen::new(physical_rows, physical_cols, 0, ScreenType::Alternate);

        Self {
            screen,
            alt_screen,
            alt_screen_is_active: false,
        }
    }

    pub fn resize(&mut self, physical_rows: usize, physical_cols: usize) {
        self.screen.resize(physical_rows, physical_cols);
        self.alt_screen.resize(physical_rows, physical_cols);
    }

    pub fn activate_alt_screen(&mut self) {
        self.alt_screen_is_active = true;
    }

    pub fn activate_primary_screen(&mut self) {
        for i in 0..self.screen.physical_rows{
            self.screen.line_at(i).set_dirty();
        }
        self.alt_screen_is_active = false;
    }

    pub fn is_alt_screen_active(&self) -> bool {
        self.alt_screen_is_active
    }

    pub fn lines_no_ansi(&self) -> Vec<String> {
        if self.is_alt_screen_active() {
            self.alt_screen.lines.iter().map(|l| l.clone().as_str()).collect::<Vec<_>>()
        } else {
            self.screen.hlines.iter().filter_map(|l| l.to_string_no_ansi()).collect::<Vec<_>>()
        }
    }
}
