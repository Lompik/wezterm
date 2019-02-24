//! Model a cell in the terminal display
use crate::color::ColorAttribute;
pub use crate::escape::osc::Hyperlink;
use crate::image::ImageCell;
use smallvec::SmallVec;
use std;
use std::mem;
use std::rc::Rc;
use unicode_width::UnicodeWidthStr;

/// Holds the attributes for a cell.
/// Most style attributes are stored internally as part of a bitfield
/// to reduce per-cell overhead.
/// The setter methods return a mutable self reference so that they can
/// be chained together.
#[derive(Debug, Default, Clone, Eq, PartialEq)]
pub struct CellAttributes {
    attributes: u16,
    /// The foreground color
    pub foreground: ColorAttribute,
    /// The background color
    pub background: ColorAttribute,
    /// The hyperlink content, if any
    pub hyperlink: Option<Rc<Hyperlink>>,
    /// The image data, if any
    pub image: Option<Box<ImageCell>>,
}

/// Define getter and setter for the attributes bitfield.
/// The first form is for a simple boolean value stored in
/// a single bit.  The $bitnum parameter specifies which bit.
/// The second form is for an integer value that occupies a range
/// of bits.  The $bitmask and $bitshift parameters define how
/// to transform from the stored bit value to the consumable
/// value.
macro_rules! bitfield {
    ($getter:ident, $setter:ident, $bitnum:expr) => {
        #[inline]
        pub fn $getter(&self) -> bool {
            (self.attributes & (1 << $bitnum)) == (1 << $bitnum)
        }

        #[inline]
        pub fn $setter(&mut self, value: bool) -> &mut Self {
            let attr_value = if value { 1 << $bitnum } else { 0 };
            self.attributes = (self.attributes & !(1 << $bitnum)) | attr_value;
            self
        }
    };

    ($getter:ident, $setter:ident, $bitmask:expr, $bitshift:expr) => {
        #[inline]
        pub fn $getter(&self) -> u16 {
            (self.attributes >> $bitshift) & $bitmask
        }

        #[inline]
        pub fn $setter(&mut self, value: u16) -> &mut Self {
            let clear = !($bitmask << $bitshift);
            let attr_value = (value & $bitmask) << $bitshift;
            self.attributes = (self.attributes & clear) | attr_value;
            self
        }
    };

    ($getter:ident, $setter:ident, $enum:ident, $bitmask:expr, $bitshift:expr) => {
        #[inline]
        pub fn $getter(&self) -> $enum {
            unsafe { mem::transmute(((self.attributes >> $bitshift) & $bitmask) as u16)}
        }

        #[inline]
        pub fn $setter(&mut self, value: $enum) -> &mut Self {
            let value = value as u16;
            let clear = !($bitmask << $bitshift);
            let attr_value = (value & $bitmask) << $bitshift;
            self.attributes = (self.attributes & clear) | attr_value;
            self
        }
    };
}

/// The `Intensity` of a cell describes its boldness.  Most terminals
/// implement `Intensity::Bold` by either using a bold font or by simply
/// using an alternative color.  Some terminals implement `Intensity::Half`
/// as a dimmer color variant.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[repr(u16)]
pub enum Intensity {
    Normal = 0,
    Bold = 1,
    Half = 2,
}

/// Specify just how underlined you want your `Cell` to be
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[repr(u16)]
pub enum Underline {
    /// The cell is not underlined
    None = 0,
    /// The cell is underlined with a single line
    Single = 1,
    /// The cell is underlined with two lines
    Double = 2,
}

/// Allow converting to boolean; true means some kind of
/// underline, false means none.  This is used in some
/// generic code to determine whether to enable underline.
impl Into<bool> for Underline {
    fn into(self) -> bool {
        self != Underline::None
    }
}

/// Specify whether you want to slowly or rapidly annoy your users
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[repr(u16)]
pub enum Blink {
    None = 0,
    Slow = 1,
    Rapid = 2,
}

/// Allow converting to boolean; true means some kind of
/// blink, false means none.  This is used in some
/// generic code to determine whether to enable blink.
impl Into<bool> for Blink {
    fn into(self) -> bool {
        self != Blink::None
    }
}

impl CellAttributes {
    bitfield!(intensity, set_intensity, Intensity, 0b11, 0);
    bitfield!(underline, set_underline, Underline, 0b11, 2);
    bitfield!(blink, set_blink, Blink, 0b11, 4);
    bitfield!(italic, set_italic, 6);
    bitfield!(reverse, set_reverse, 7);
    bitfield!(strikethrough, set_strikethrough, 8);
    bitfield!(invisible, set_invisible, 9);

    /// Returns true if the attribute bits in both objects are equal.
    /// This can be used to cheaply test whether the styles of the two
    /// cells are the same, and is used by some `Renderer` implementations.
    pub fn attribute_bits_equal(&self, other: &Self) -> bool {
        self.attributes == other.attributes
    }

    /// Set the foreground color for the cell to that specified
    pub fn set_foreground<C: Into<ColorAttribute>>(&mut self, foreground: C) -> &mut Self {
        self.foreground = foreground.into();
        self
    }

    pub fn set_background<C: Into<ColorAttribute>>(&mut self, background: C) -> &mut Self {
        self.background = background.into();
        self
    }

    pub fn set_hyperlink(&mut self, link: Option<Rc<Hyperlink>>) -> &mut Self {
        self.hyperlink = link;
        self
    }

    pub fn set_image(&mut self, image: Option<Box<ImageCell>>) -> &mut Self {
        self.image = image;
        self
    }

    /// Clone the attributes, but exclude fancy extras such
    /// as hyperlinks or future sprite things
    pub fn clone_sgr_only(&self) -> Self {
        Self {
            attributes: self.attributes,
            foreground: self.foreground,
            background: self.background,
            hyperlink: None,
            image: None,
        }
    }
}

const BLANKATTRS: CellAttributes = CellAttributes {
    attributes: 0,
    /// The foreground color
    foreground: ColorAttribute::Default,
    /// The background color
    background: ColorAttribute::Default,
    /// The hyperlink content, if any
    hyperlink: None,
    /// The image data, if any
    image: None,
};

/// Models the contents of a cell on the terminal display
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct CellDataAll {
    pub text: SmallVec<[u8; 4]>,
    attrs: CellAttributes,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum CellData {
    Unicode(SmallVec<[u8; 4]>),
    Ascii(char),
}


const MAX_ONE_B: u32   =     0x80;
const MAX_TWO_B: u32   =    0x800;
const MAX_THREE_B: u32 = 0x10000;

impl CellData {
    fn new(text: char) -> CellData {
        let text_u32 = text as u32;
        let len = if text_u32 < MAX_ONE_B {
            let c = if text_u32 < 0x20 {
                0x20
            } else {
                text_u32 as u8
            };
            return  CellData::Ascii(text);
        } else if  text_u32 < MAX_TWO_B {
            2
        } else if text_u32 < MAX_THREE_B {
            3
        } else {4};

        let mut buf: [u8; 4] = [0; 4];
        //storage.resize(len, 0);
        text.encode_utf8(&mut buf);
        let storage = SmallVec::from_buf_and_len(buf, len);

        CellData::Unicode(storage)
    }
}

pub struct Cell2 {
    // pub text: SmallVec<[u8; 4]>,
    attrs: CellAttributes,
    cd: SmallVec<[u8; 4]>
}

/// Models the contents of a cell on the terminal display
#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Cell {
    // pub text: SmallVec<[u8; 4]>,
    attrs: CellAttributes,
    cd: SmallVec<[u8; 4]>
}

//pub const CELL_BLANK: Cell = Cell{cd: Smassmallvec![0x20u8], attrs: BLANKATTRS};

impl Default for Cell {
    fn default() -> Self {
        //CELL_BLANK  //Cell::new(' ', CellAttributes::default())
        let storage: SmallVec<[u8; 4]>=smallvec![0x20u8];
        Cell{
            attrs: BLANKATTRS,
            cd:(storage)
        }
    }
}

     //m0\n128|"\x%02d" // "%c"\n
const ASCII_TABLE: [&'static str; 128] = [
    "\x00", // " "
"\x01", // ""
"\x02", // ""
"\x03", // ""
"\x04", // ""
"\x05", // ""
"\x06", // ""
"\x07", // ""
"\x08", // ""
"\x09", // "	"
"\x0a", // ""
"\x0b", // ""
"\x0c", // ""
"\x0d", // "ecM"
"\x0e", // ""
"\x0f", // ""
"\x10", // ""
"\x11", // ""
"\x12", // ""
"\x13", // ""
"\x14", // ""
"\x15", // ""
"\x16", // ""
"\x17", // ""
"\x18", // ""
"\x19", // ""
"\x1a", // ""
"\x1b", // ""
"\x1c", // ""
"\x1d", // ""
"\x1e", // ""
"\x1f", // ""
"\x20", // " "
"\x21", // "!"
"\x22", // """
"\x23", // "#"
"\x24", // "$"
"\x25", // "%"
"\x26", // "&"
"\x27", // "'"
"\x28", // "("
"\x29", // ")"
"\x2a", // "*"
"\x2b", // "+"
"\x2c", // ","
"\x2d", // "-"
"\x2e", // "."
"\x2f", // "/"
"\x30", // "0"
"\x31", // "1"
"\x32", // "2"
"\x33", // "3"
"\x34", // "4"
"\x35", // "5"
"\x36", // "6"
"\x37", // "7"
"\x38", // "8"
"\x39", // "9"
"\x3a", // ":"
"\x3b", // ";"
"\x3c", // "<"
"\x3d", // "="
"\x3e", // ">"
"\x3f", // "?"
"\x40", // "@"
"\x41", // "A"
"\x42", // "B"
"\x43", // "C"
"\x44", // "D"
"\x45", // "E"
"\x46", // "F"
"\x47", // "G"
"\x48", // "H"
"\x49", // "I"
"\x4a", // "J"
"\x4b", // "K"
"\x4c", // "L"
"\x4d", // "M"
"\x4e", // "N"
"\x4f", // "O"
"\x50", // "P"
"\x51", // "Q"
"\x52", // "R"
"\x53", // "S"
"\x54", // "T"
"\x55", // "U"
"\x56", // "V"
"\x57", // "W"
"\x58", // "X"
"\x59", // "Y"
"\x5a", // "Z"
"\x5b", // "["
"\x5c", // "\"
"\x5d", // "]"
"\x5e", // "^"
"\x5f", // "_"
"\x60", // "`"
"\x61", // "a"
"\x62", // "b"
"\x63", // "c"
"\x64", // "d"
"\x65", // "e"
"\x66", // "f"
"\x67", // "g"
"\x68", // "h"
"\x69", // "i"
"\x6a", // "j"
"\x6b", // "k"
"\x6c", // "l"
"\x6d", // "m"
"\x6e", // "n"
"\x6f", // "o"
"\x70", // "p"
"\x71", // "q"
"\x72", // "r"
"\x73", // "s"
"\x74", // "t"
"\x75", // "u"
"\x76", // "v"
"\x77", // "w"
"\x78", // "x"
"\x79", // "y"
"\x7a", // "z"
"\x7b", // "{"
"\x7c", // "|"
"\x7d", // "}"
"\x7e", // "~"
"\x7f", // ""
];

impl Cell {
    /// De-fang the input character such that it has no special meaning
    /// to a terminal.  All control and movement characters are rewritten
    /// as a space.
    pub fn nerf_control_char(text: &mut SmallVec<[u8; 8]>) {
        if text.len() == 0 {
            text.push(b' ');
            return;
        }

        // if text.as_slice() == &[b'\r', b'\n'] {
        //     text.remove(1);
        //     text[0] = b' ';
        //     return;
        // }

        if text.len() != 1 {
            return;
        }

        if text[0] < 0x20 || text[0] == 0x7f {
            text[0] = b' ';
        }
    }

    pub fn blank(&mut self){
        self.cd = smallvec![0x20u8];
        self.attrs = CellAttributes::default();
    }

    pub fn is_blank(&self) -> bool {
        match &self.cd[0] {
            0x20 => true,
            _ => false
        }
    }

    pub fn blank_with_attrs(&mut self, attrs: CellAttributes){
        self.cd = smallvec![0x20u8];
        self.attrs = attrs;
    }

    pub fn set(&mut self, cell: &Cell){
        self.cd = cell.cd.clone();
        self.attrs = cell.attrs.clone();
    }

    #[inline]
    pub fn update(&mut self, text: char, attrs: CellAttributes){
        self.attrs = attrs;
        if (text as u32) < MAX_ONE_B && self.cd[0]  & (MAX_ONE_B as u8) == 0 {
            self.cd[0] = text as u8;
            return
        }
        self.cd.clear();
        let mut b = [0;4];
        text.encode_utf8(&mut b);
        self.cd.insert_from_slice(0, &b[..text.len_utf8()]);
        return;

        //self.cd = CellData::new(text);
    }

    pub fn append(&mut self, text: char) {
        let len = text.len_utf8();
        let mut b = [0; 4];
        text.encode_utf8(&mut b);
        self.cd.extend_from_slice(&b[..len]);
    }

    /// Create a new cell holding the specified character and with the
    /// specified cell attributes.
    /// All control and movement characters are rewritten as a space.
    pub fn new(text: char, attrs: CellAttributes) -> Self {

        let mut storage = SmallVec::default();
        storage.resize(text.len_utf8(), 0);

        text.encode_utf8(&mut storage);
        Self {
            cd: storage,
            attrs
        }
    }

    /// Create a new cell holding the specified grapheme.
    /// The grapheme is passed as a string slice and is intended to hold
    /// double-width characters, or combining unicode sequences, that need
    /// to be treated as a single logical "character" that can be cursored
    /// over.  This function technically allows for an arbitrary string to
    /// be passed but it should not be used to hold strings other than
    /// graphemes.
    pub fn new_grapheme(text: &str, attrs: CellAttributes) -> Self {
        let bytes = text.as_bytes();
        // let text = match text {
        //     "" => return Cell{attrs, cd: CellData::Ascii(' ')},
        //     "\n" | "\r\n" | "\r" => return Cell{attrs, cd: CellData::Ascii(' ')},
        //     c if c.len()==1 && bytes[0] < 0x20 => return Cell{attrs, cd: CellData::Ascii(' ')},
        //     c if c.len()==1 && bytes[0] < 0x1F  => return Cell{attrs, cd: CellData::Ascii(bytes[0] as char)},
        //     _ => text
        // };
        let mut storage = SmallVec::from_slice(bytes);
        //cargo nerf_cc!(&mut storage);

        Self {attrs,  cd: (storage)
        }
    }

    /// Returns the textual content of the cell
    pub fn str(&self) -> &str {
        // unsafety: this is safe because the constructor guarantees
        // that the storage is valid utf8
        unsafe { std::str::from_utf8_unchecked(&self.cd) }
        // match &self.cd {
        //     CellData::Unicode(a) => unsafe { std::str::from_utf8_unchecked(&a) },
        //     CellData::Ascii(a) => ASCII_TABLE.get(*a as usize).expect("Character unknown")
        // }
    }

    /// Returns the number of cells visually occupied by this grapheme
    pub fn width(&self) -> usize {
        UnicodeWidthStr::width(unsafe { std::str::from_utf8_unchecked(&self.cd) })
        // match &self.cd {
        //     CellData::Unicode(a) => UnicodeWidthStr::width(unsafe { std::str::from_utf8_unchecked(&a) }),
        //     CellData::Ascii(_) => 1
        // }
    }

    /// Returns the attributes of the cell
    pub fn attrs(&self) -> &CellAttributes {
        &self.attrs
    }
}



/// Models a change in the attributes of a cell in a stream of changes.
/// Each variant specifies one of the possible attributes; the corresponding
/// value holds the new value to be used for that attribute.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AttributeChange {
    Intensity(Intensity),
    Underline(Underline),
    Italic(bool),
    Blink(Blink),
    Reverse(bool),
    StrikeThrough(bool),
    Invisible(bool),
    Foreground(ColorAttribute),
    Background(ColorAttribute),
    Hyperlink(Option<Rc<Hyperlink>>),
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn nerf_special() {
        for c in " \n\r\t".chars() {
            let cell = Cell::new(c, CellAttributes::default());
            assert_eq!(cell.str(), " ");
        }

        for g in &["", " ", "\n", "\r", "\t", "\r\n"] {
            let cell = Cell::new_grapheme(g, CellAttributes::default());
            assert_eq!(cell.str(), " ");
        }
    }
}
