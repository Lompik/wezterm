use super::*;
use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct LineHisto {
    pub chars: Vec<char>,
    pub attrs: CellAttributes,
    x: usize,
}

#[derive(Debug, Clone)]
pub struct LineH {
    pub is_dirty: bool,
    pub line: Vec<LineHisto>
}

impl Default for LineH {
    fn default() -> Self{
        LineH {is_dirty: true, line:Vec::new()}
    }
}

impl LineH {
    pub fn reset(&mut self) {
        self.set_dirty();
        self.line.clear();
    }

    pub fn set_dirty(&mut self){
        self.is_dirty = true;
    }

    pub fn push(&mut self, l: LineHisto){
        self.set_dirty();
        self.line.push(l)
    }

    pub fn is_empty(&self) -> bool{
        if self.line.len() == 0 {
            return true
        }
        if self.line.iter().map(|x| x.chars.len()).sum::<usize>() ==0 {
            return true
        }
        false
    }

    pub fn max_char_count(&self) -> usize {
        self.line.iter().fold(0, |res, line|{
            res.max(line.x+line.chars.len())
        })
    }

    pub fn get_x(&self, x: usize) -> Option<char> {
        self.line.iter()
            .filter_map(|lhi|
                        if x >= lhi.x {
                            lhi.chars.get(x-lhi.x)
                        } else {None}
            )
            .map(|x| *x)
            .last()
    }

    pub fn get_x_with_attr(&self, x: usize) -> Option<(char, CellAttributes)> {
        self.line.iter().enumerate()
            .filter_map(|(i, lhi)|
                        if x >= lhi.x && lhi.chars.get(x-lhi.x).is_some(){
                            Some(i)
                        } else {None}
            )
            .last().map(|i| {
                let lhix = self.line[i].x;
                (self.line[i].chars[x-lhix], self.line[i].attrs.clone())
            })
    }

    pub fn merge(&mut self, width: usize) {
        if self.line.len() <= width {
            return;
        }
        let mut newlineh = Vec::new();
        for i in 0..width{
            match self.get_x_with_attr(i) {
                Some(xa) => newlineh.push(LineHisto{chars: vec![xa.0], attrs: xa.1, x: i}),
                _ => {}
            }
        }
        if self.max_char_count() > width {
            for lhi in self.line.iter().skip_while(|lhi| lhi.x + lhi.chars.len() <= width || lhi.chars.is_empty()) {
                let new = LineHisto {
                    attrs: lhi.attrs.clone(),
                    chars: lhi.chars.clone().split_off(width.saturating_sub(lhi.x).min(lhi.chars.len())),
                    x: lhi.x.max(width)
                };
                newlineh.push(new);
            }
        }
        self.line = newlineh;
    }

    pub fn erase_char(&mut self, x:usize) {
        let mut found = false;
        let mut empty = Vec::new();
        let mut empty_idx = 0usize;
        self.merge(x+1);
        //trace!("hline_erase_char {}: {:?}", x, &self.line[..20]);
        for lhi in self.line.iter_mut(){
            let lhix = lhi.x;
            if lhix >= x {
                lhi.x = lhix.saturating_sub(1);
            }
            if x >= lhix && x-lhix < lhi.chars.len() {
                lhi.chars.remove(x-lhix);
                if lhi.chars.is_empty(){
                    empty.push(empty_idx);
                }
                found = true;
            }
            empty_idx += 1;
        }
        if found {
            for idx in empty.iter().rev(){
                self.line.remove(*idx);
            }
            self.set_dirty();
        }

    }

    pub fn to_string_no_ansi(&self) -> Option<String> {
        let mcc = self.max_char_count();
        if mcc == 0 {
            return None;
        }
        Some((0..self.max_char_count())
            .map(|idx| self.get_x(idx).unwrap_or(' '))
            .collect::<String>())
    }
}

impl LineHisto{
    pub fn new( chars: Vec<char>, attrs: CellAttributes, x: usize) -> Self{
        LineHisto{ chars,
                   attrs,
                   x,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_x_1() {
        let mut lineh = LineH::default();
        let test_c = LineHisto{
            chars: vec!['a', 'b', 'c'],
            attrs: CellAttributes::default(),
            x: 0
        };
        lineh.push(test_c);
        assert_eq!(Some('b'), lineh.get_x(1));
        assert_eq!(3, lineh.max_char_count());
        assert_eq!(Some("abc".to_string()), lineh.to_string_no_ansi());
    }

    #[test]
    fn test_get_x_2() {
        let mut lineh = LineH::default();
        let mut tests_c = vec![LineHisto{
            chars: vec!['a', 'b', 'c'],
            attrs: CellAttributes::default(),
            x: 0
        }, LineHisto{
            chars: vec!['e', 'f', 'g'],
            attrs: CellAttributes::default(),
            x: 1
        }];
        lineh.push(tests_c.remove(0));
        lineh.push(tests_c.remove(0));
        assert_eq!(Some('e'), lineh.get_x(1));
        assert_eq!(4, lineh.max_char_count());
        assert_eq!(Some("aefg".to_string()), lineh.to_string_no_ansi());
    }

    #[test]
    fn test_erase() {
        let mut lineh = LineH::default();
        let tests_c = vec![LineHisto{
            chars: vec!['a', 'b', 'c', 'e', 'f', 'g'],
            attrs: CellAttributes::default(),
            x: 0
        }, LineHisto{
            chars: vec!['h', 'i', 'j'],
            attrs: CellAttributes::default(),
            x: 1
        }, LineHisto{
            chars: vec!['k', 'l', 'o'],
            attrs: CellAttributes::default(),
            x: 9
        }];
        lineh.push(tests_c[0].clone());
        lineh.push(tests_c[1].clone());
        lineh.erase_char(2);
        assert_eq!(Some('j'), lineh.get_x(2));
        assert_eq!(5, lineh.max_char_count());
        assert_eq!(Some("ahjfg".to_string()), lineh.to_string_no_ansi());

        lineh.line.clear();
        lineh.push(tests_c[1].clone());
        lineh.push(tests_c[0].clone());
        lineh.erase_char(2);
        assert_eq!(Some("abefg".to_string()), lineh.to_string_no_ansi());
        assert_eq!(Some('e'), lineh.get_x(2));
        assert_eq!(5, lineh.max_char_count());

        lineh.line.clear();
        lineh.push(tests_c[2].clone());
        lineh.push(tests_c[0].clone());
        lineh.push(tests_c[1].clone());
        lineh.erase_char(2);
        assert_eq!(Some('l'), lineh.get_x(9));
        assert_eq!(Some("ahjfg   klo".to_string()), lineh.to_string_no_ansi());
        assert_eq!(11, lineh.max_char_count());

    }


    #[test]
    fn test_merge() {
        let mut lineh = LineH::default();
        let mut tests_c = vec![LineHisto{
            chars: vec!['a', 'b', 'c'],
            attrs: CellAttributes::default(),
            x: 0
        }, LineHisto{
            chars: vec!['e', 'f', 'g'],
            attrs: CellAttributes::default(),
            x: 1
        }];
        lineh.push(tests_c.remove(0));
        lineh.push(tests_c.remove(0));
        lineh.merge(1);
        eprintln!("{:?}", lineh.line);
        assert_eq!(Some('f'), lineh.get_x(2));
        assert_eq!(4, lineh.max_char_count());
        assert_eq!(Some("aefg".to_string()), lineh.to_string_no_ansi());
    }

}

#[derive(Debug, Clone, PartialEq)]
pub enum ScreenType {
    Primary, // with scrollback
    Alternate
}

/// Holds the model of a screen.  This can either be the primary screen
/// which includes lines of scrollback text, or the alternate screen
/// which holds no scrollback.  The intent is to have one instance of
/// Screen for each of these things.
#[derive(Debug, Clone)]
pub struct Screen {
    /// Holds the line data that comprises the screen contents.
    /// This is allocated with capacity for the entire scrollback.
    /// The last N lines are the visible lines, with those prior being
    /// the lines that have scrolled off the top of the screen.
    /// Index 0 is the topmost line of the screen/scrollback (depending
    /// on the current window size) and will be the first line to be
    /// popped off the front of the screen when a new line is added that
    /// would otherwise have exceeded the line capacity
    pub lines: VecDeque<Line>,
    pub hlines: VecDeque<LineH>,

    /// Maximum number of lines of scrollback
    pub scrollback_size: usize,

    /// Physical, visible height of the screen (not including scrollback)
    pub physical_rows: usize,
    /// Physical, visible width of the screen
    pub physical_cols: usize,

    pub kind: ScreenType
}

impl Screen {
    /// Create a new Screen with the specified dimensions.
    /// The Cells in the viewable portion of the screen are set to the
    /// default cell attributes.
    pub fn new(physical_rows: usize, physical_cols: usize, scrollback_size: usize, kind: ScreenType) -> Screen {
        let mut lines = VecDeque::with_capacity(physical_rows);
        let cap = match kind {
            ScreenType::Primary => physical_rows + scrollback_size,
            ScreenType::Alternate => scrollback_size,
        };
        let mut hlines =  VecDeque::with_capacity(cap);
        for _ in 0..physical_rows {
            lines.push_back(Line::with_width(physical_cols));
        }
        for _ in 0..physical_rows {
            hlines.push_back(LineH::default());
        }

        Screen {
            lines,
            hlines,
            scrollback_size,
            physical_rows,
            physical_cols,
            kind
        }
    }

    fn resize_line(&mut self, physical_rows: usize, physical_cols: usize){
        let capacity = physical_rows;
        let current_capacity = self.lines.capacity();
        if capacity > current_capacity {
            self.lines.reserve(capacity - current_capacity);
        }

        if physical_rows > self.physical_rows {
            // Enlarging the viewable portion?  Add more lines at the bottom
            for _ in self.physical_rows..physical_rows {
                self.lines.push_back(Line::with_width(physical_cols));
            }
        }
    }

    /// Resize the physical, viewable portion of the screen
    fn resize_hline(&mut self, physical_rows: usize, physical_cols: usize) {
        let capacity = physical_rows + self.scrollback_size;
        let current_capacity = self.hlines.capacity();
        if capacity > current_capacity {
            self.hlines.reserve(capacity - current_capacity);
        }

        if physical_rows > self.physical_rows {
            // Enlarging the viewable portion?  Add more lines at the bottom
            for _ in self.physical_rows..physical_rows {
                self.hlines.push_back(LineH::default());
            }
        } else if physical_rows < self.physical_rows {
            for idx in (self.hlines.len() - (self.physical_rows -  physical_rows)..self.hlines.len()).rev() {
                if self.hlines[idx].is_empty(){
                    self.hlines.remove(idx);
                }
            }
        }
    }
    pub fn resize(&mut self, physical_rows: usize, physical_cols:usize){
        self.resize_line(physical_rows, physical_cols);
        self.resize_hline(physical_rows, physical_cols);
        self.physical_rows = physical_rows;
        self.physical_cols = physical_cols;
    }

    /// Get mutable reference to a line, relative to start of scrollback.
    #[inline]
    pub fn line_mut(&mut self, idx: PhysRowIndex) -> &mut Line {
        &mut self.lines[idx]
    }

    #[inline]
    pub fn hline_mut(&mut self, idx: PhysRowIndex) -> &mut LineH {
        &mut self.hlines[idx]
    }

    /// Sets a line dirty.  The line is relative to the visible origin.
    #[inline]
    pub fn dirty_line(&mut self, idx: VisibleRowIndex) {
        //let line_idx = self.phys_row(idx);
        let idx = idx.abs() as usize;
        if idx < self.lines.len() {
            self.lines[idx].set_dirty();
        }
    }

    /// Returns a copy of the visible lines in the screen (no scrollback)
    #[cfg(test)]
    pub fn visible_lines(&self) -> Vec<Line> {
        let line_idx = self.lines.len() - self.physical_rows;
        let mut lines = Vec::new();
        for line in self.lines.iter().skip(line_idx) {
            if lines.len() >= self.physical_rows {
                break;
            }
            lines.push(line.clone());
        }
        lines
    }

    /// Returns a copy of the lines in the screen (including scrollback)
    #[cfg(test)]
    pub fn all_lines(&self) -> Vec<Line> {
        self.lines.iter().map(|l| l.clone()).collect()
    }

    pub fn insert_cell(&mut self, x: usize, y: VisibleRowIndex) {
        assert!(self.kind == ScreenType::Alternate); // [TODO]
        let line_idx = self.phys_row(y);
        let line = self.line_mut(line_idx);
        line.insert_cell(x, Cell::default());
    }

    pub fn erase_cell(&mut self, x: usize, y: VisibleRowIndex) {
        if self.kind == ScreenType::Alternate {
            let line_idx = self.phys_row(y);
            let line = self.line_mut(line_idx);
            line.erase_cell(x);
        } else {
            let line_idx = self.phys_row(y);
            let hline = self.hline_mut(line_idx);
            hline.erase_char(x);
        }
    }

    /// Set a cell.  the x and y coordinates are relative to the visible screeen
    /// origin.  0,0 is the top left.
    pub fn set_cell(&mut self, x: usize, y: VisibleRowIndex, cell: &Cell) -> &Cell {
        assert!(self.kind == ScreenType::Alternate); // [TODO]
        let line_idx = self.phys_row(y);
        debug!("set_cell x={} y={} phys={} {:?}", x, y, line_idx, cell);

        let line = self.line_mut(line_idx);
        line.set_cell(x, cell)
    }

    pub fn line_at(&mut self, y: usize) -> &mut Line {
        self.line_mut(y)
    }

    pub fn hline_at(&mut self, y: VisibleRowIndex) -> &mut LineH {
        let line_idx = self.phys_row(y);
        self.hline_mut(line_idx)
    }

    pub fn update_or_set_cell(&mut self, x: usize, y: VisibleRowIndex, text: char,  attr: CellAttributes) -> (&Cell, usize) {
        let line_idx = self.phys_row(y);
        //debug!("set_cell x={} y={} phys={} {:?}", x, y, line_idx, cell);

        let line = self.line_mut(line_idx);
        line.update_or_set_cell(x, text, attr)
    }

    pub fn clear_line(
        &mut self,
        y: VisibleRowIndex,
        cols: impl Iterator<Item = usize>,
        attr: &CellAttributes,
    ) {
        let line_idx = self.phys_row(y);
        let line = self.line_mut(y as usize);
        line.fill_range(cols, &Cell::new(' ', attr.clone()));
    }

    pub fn clear_line_hack(
        &mut self,
        y: VisibleRowIndex,
        cols: impl Iterator<Item = usize>,
        attr: &CellAttributes,
    ) {  // [FIXME] handle properly with unicode width
        let width= self.physical_cols;
        let line_idx = self.phys_row(y);
        let hline = self.hline_mut(line_idx);
        let mut found = false;
        for x in cols{
            if x > width {
                break;
            }
            for hl in hline.line.iter_mut(){
                if x >= hl.x && x < hl.x+hl.chars.len() {
                    hl.chars[x.saturating_sub(hl.x)] = ' ';
                    found = true;
                }
            }
        }
        if found {
            hline.set_dirty();
        }
        //line.fill_range(cols, &Cell::new(' ', attr.clone()));
    }

    /// Translate a VisibleRowIndex into a PhysRowIndex.  The resultant index
    /// will be invalidated by inserting or removing rows!
    #[inline]
    pub fn phys_row(&self, row: VisibleRowIndex) -> PhysRowIndex {
        assert!(row >= 0, "phys_row called with negative row {}", row);
        match self.kind {
            ScreenType::Primary => (self.hlines.len() - self.physical_rows) + row as usize,
            ScreenType::Alternate => {
                assert!(row < self.physical_rows as i64);
                row as usize
            }
        }
    }

    /// Given a possibly negative row number, return the corresponding physical
    /// row.  This is similar to phys_row() but allows indexing backwards into
    /// the scrollback.
    #[inline]
    pub fn scrollback_or_visible_row(&self, row: ScrollbackOrVisibleRowIndex) -> PhysRowIndex {
        match self.kind {
            ScreenType::Primary => ((self.hlines.len() - self.physical_rows) as ScrollbackOrVisibleRowIndex + row).max(0)
            as usize,
            ScreenType::Alternate => {
                assert!(row < self.physical_rows as i32);
                row.max(0) as usize
            }
        }
    }

    #[inline]
    pub fn scrollback_or_visible_range(
        &self,
        range: &Range<ScrollbackOrVisibleRowIndex>,
    ) -> Range<PhysRowIndex> {
        self.scrollback_or_visible_row(range.start)..self.scrollback_or_visible_row(range.end)
    }

    /// Translate a range of VisibleRowIndex to a range of PhysRowIndex.
    /// The resultant range will be invalidated by inserting or removing rows!
    #[inline]
    pub fn phys_range(&self, range: &Range<VisibleRowIndex>) -> Range<PhysRowIndex> {
        self.phys_row(range.start)..self.phys_row(range.end)
    }

    pub fn fill_line(&mut self, p: &[char], pen: &CellAttributes, y: usize, x: usize) -> usize {
        let mut it = p.iter();
        let width = self.physical_cols;
        let mut x = x;
        while let Some(mut g) = it.next()  //unicode_segmentation::UnicodeSegmentation::graphemes(s.as_str(), true).map(|x| x.chars()).flatten()
        {

            let mut print_width =0;
            {let line = self.line_at(y);
             if x == 0 {
                 line.resize_and_clear(width);
             }

            // Assign the cell and extract its printable width

             let mut has_next = true;
            while x+print_width < width && has_next{
                x+=print_width;
                let (_, pw) = line
                    .update_or_set_cell(x, *g, pen.clone());
                print_width = pw;
                if let Some(g1) = it.next(){
                    g = g1;
                } else {
                    has_next=false;
                };
            }
}                // the max(1) here is to ensure that we advance to the next cell
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
                x += print_width;
            }
        }
        x
    }

    pub fn fill_lines(&mut self, scrollback: ScrollbackOrVisibleRowIndex){
        if self.kind == ScreenType::Alternate {
            return;
        }
        let mut y = 0;
        let width = self.physical_cols;
        let vis = self.hlines.len().saturating_sub  (self.physical_rows + (scrollback.abs() as usize));
        for idx in vis as usize..vis+self.physical_rows {
            let is_dirty = self.hlines[idx].is_dirty;
            if is_dirty || self.line_at(y).is_dirty(){
                self.line_at(y).resize_and_clear(width);
                self.hline_mut(idx).merge(width);
                for h in self.hline_mut(idx).line.clone().iter() { // [FIXME] clone
                    self.fill_line(&h.chars, &h.attrs, y, h.x);
                }
                self.hlines[idx].is_dirty = false;
            }
            y += 1;
        }
    }

    /// ---------
    /// |
    /// |--- top
    /// |
    /// |--- bottom
    ///
    /// scroll the region up by num_rows.  Any rows that would be scrolled
    /// beyond the top get removed from the screen.
    /// In other words, we remove (top..top+num_rows) and then insert num_rows
    /// at bottom.
    /// If the top of the region is the top of the visible display, rather than
    /// removing the lines we let them go into the scrollback.
    pub fn scroll_up(&mut self, scroll_region: &Range<VisibleRowIndex>, num_rows: usize) {
        if self.kind == ScreenType::Alternate {
            self.scroll_lines_up(scroll_region, num_rows);
            return;
        }
        debug!("scroll_up {:?} {}", scroll_region, num_rows);
        let phys_scroll = self.phys_range(scroll_region);

        debug_assert!(num_rows <= phys_scroll.end - phys_scroll.start);

        // Invalidate the lines that will move before they move so that
        // the indices of the lines are stable (we may remove lines below)
        for (y, physy) in phys_scroll.clone().enumerate() {
            self.hlines[physy].set_dirty();
        }

        // if we're going to remove lines due to lack of scrollback capacity,
        // remember how many so that we can adjust our insertion point later.
        let lines_removed = if scroll_region.start > 0 {
            // No scrollback available for these;
            // Remove the scrolled lines
            num_rows
        } else {
            let max_allowed = self.physical_rows + self.scrollback_size;
            if self.hlines.len() + num_rows >= max_allowed {
                (self.hlines.len() + num_rows) - max_allowed
            } else {
                0
            }
        };

        let remove_idx = if scroll_region.start == 0 {
            0
        } else {
            phys_scroll.start
        };

        // To avoid thrashing the heap, prefer to move lines that were
        // scrolled off the top and re-use them at the bottom.
        let to_move = lines_removed.min(num_rows);
        //dbg!(to_move);
        let (to_remove, to_add) = {
             for _ in 0..to_move {
                let mut hline = self.hlines.remove(remove_idx).unwrap();
                 // Make the line like a new one of the appropriate width
                 hline.reset(); // resize_and_clear(self.physical_cols);
                if scroll_region.end as usize == self.physical_rows {
                    self.hlines.push_back(hline);
                } else {
                    self.hlines.insert(phys_scroll.end - 1, hline);
                }
            }
            // We may still have some lines to add at the bottom, so
            // return revised counts for remove/add
            (lines_removed - to_move, num_rows - to_move)
        };

        // Perform the removal
        for _ in 0..to_remove {
            self.hlines.remove(remove_idx);
        }

        if scroll_region.end as usize == self.physical_rows {
            // It's cheaper to push() than it is insert() at the end
            for _ in 0..to_add {
                self.hlines.push_back(LineH::default()); //self.hlines.push_back(Line::with_width(self.physical_cols));
            }
        } else {
            for _ in 0..to_add {
                self.hlines
                    .insert(phys_scroll.end - 1, LineH::default());
            }
        }
    }

    pub fn scroll_lines_up(&mut self, scroll_region: &Range<VisibleRowIndex>, num_rows: usize) {
        debug!("scroll_up {:?} {}", scroll_region, num_rows);
        let phys_scroll = self.phys_range(scroll_region);

        debug_assert!(num_rows <= phys_scroll.end - phys_scroll.start);

        // Invalidate the lines that will move before they move so that
        // the indices of the lines are stable (we may remove lines below)
        for (y, physy) in phys_scroll.clone().enumerate() {
            self.lines[physy].set_dirty();
        }

        // if we're going to remove lines due to lack of scrollback capacity,
        // remember how many so that we can adjust our insertion point later.
        let lines_removed = if scroll_region.start > 0 {
            // No scrollback available for these;
            // Remove the scrolled lines
            num_rows
        } else {
            let max_allowed = self.physical_rows + self.scrollback_size;
            if self.hlines.len() + num_rows >= max_allowed {
                (self.hlines.len() + num_rows) - max_allowed
            } else {
                0
            }
        };

        let remove_idx = if scroll_region.start == 0 {
            0
        } else {
            phys_scroll.start
        };

        // To avoid thrashing the heap, prefer to move lines that were
        // scrolled off the top and re-use them at the bottom.
        let to_move = lines_removed.min(num_rows);
        //dbg!(to_move);
        let (to_remove, to_add) = {
             for _ in 0..to_move {
                let mut line = self.lines.remove(remove_idx).unwrap();
                 // Make the line like a new one of the appropriate width
                line.resize_and_clear(self.physical_cols);
                if scroll_region.end as usize == self.physical_rows {
                    self.lines.push_back(line);
                } else {
                    self.lines.insert(phys_scroll.end - 1, line);
                }
            }
            // We may still have some lines to add at the bottom, so
            // return revised counts for remove/add
            (lines_removed - to_move, num_rows - to_move)
        };

        // Perform the removal
        for _ in 0..to_remove {
            self.lines.remove(remove_idx);
        }

        if scroll_region.end as usize == self.physical_rows {
            // It's cheaper to push() than it is insert() at the end
            for _ in 0..to_add {
                self.lines.push_back(Line::with_width(self.physical_cols)); //self.hlines.push_back(Line::with_width(self.physical_cols));
            }
        } else {
            for _ in 0..to_add {
                self.lines
                    .insert(phys_scroll.end - 1, Line::with_width(self.physical_cols));
            }
        }
    }

    /// ---------
    /// |
    /// |--- top
    /// |
    /// |--- bottom
    ///
    /// scroll the region down by num_rows.  Any rows that would be scrolled
    /// beyond the bottom get removed from the screen.
    /// In other words, we remove (bottom-num_rows..bottom) and then insert
    /// num_rows at scroll_top.
    pub fn scroll_down(&mut self, scroll_region: &Range<VisibleRowIndex>, num_rows: usize) {
        debug!("scroll_down {:?} {}", scroll_region, num_rows);
        let phys_scroll = self.phys_range(scroll_region);
        assert!(num_rows <= phys_scroll.end - phys_scroll.start);

        if self.kind == ScreenType::Primary {
            let middle = phys_scroll.end - num_rows;

            // dirty the rows in the region
            for y in phys_scroll.start..middle {
                self.hline_mut(y).set_dirty();
            }

            for _ in 0..num_rows {
                self.hlines.remove(middle);
            }

            for _ in 0..num_rows {
                self.hlines
                    .insert(phys_scroll.start, LineH::default());
            }
        } else {
            let middle = phys_scroll.end - num_rows;

            // dirty the rows in the region
            for y in phys_scroll.start..middle {
                self.line_mut(y).set_dirty();
            }

            for _ in 0..num_rows {
                self.lines.remove(middle);
            }

            for _ in 0..num_rows {
                self.lines
                    .insert(phys_scroll.start, Line::with_width(self.physical_cols));
            }
        }
    }
}
