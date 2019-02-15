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
}

impl LineHisto{
    pub fn new( chars: Vec<char>, attrs: CellAttributes, x: usize) -> Self{
        LineHisto{ chars,
                   attrs,
                   x,
        }
    }
}

#[derive(Debug, Clone)]
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
        let line_idx = self.phys_row(idx);
        if line_idx < self.lines.len() {
            self.lines[line_idx].set_dirty();
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
        let line_idx = self.phys_row(y);
        let line = self.line_mut(line_idx);
        line.insert_cell(x, Cell::default());
    }

    pub fn erase_cell(&mut self, x: usize, y: VisibleRowIndex) {
        let line_idx = self.phys_row(y);
        let line = self.line_mut(line_idx);
        line.erase_cell(x);
    }

    /// Set a cell.  the x and y coordinates are relative to the visible screeen
    /// origin.  0,0 is the top left.
    pub fn set_cell(&mut self, x: usize, y: VisibleRowIndex, cell: &Cell) -> &Cell {
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
        ((self.lines.len() - self.physical_rows) as ScrollbackOrVisibleRowIndex + row).max(0)
            as usize
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
        let mut y = 0;
        let width = self.physical_cols;
        let vis = self.hlines.len().saturating_sub  (self.physical_rows + (scrollback.abs() as usize));
        for idx in vis as usize..vis+self.physical_rows {
            let is_dirty = self.hlines[idx].is_dirty;
            if is_dirty || self.line_at(y).is_dirty(){
                self.line_at(y).resize_and_clear(width);
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
        debug!("scroll_up {:?} {}", scroll_region, num_rows);
        let phys_scroll = self.phys_range(scroll_region);

        debug_assert!(num_rows <= phys_scroll.end - phys_scroll.start);

        // Invalidate the lines that will move before they move so that
        // the indices of the lines are stable (we may remove lines below)
        for (y, physy) in phys_scroll.clone().enumerate() {
            //self.line_mut(y).resize_and_clear(width);
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
    }
}
