//! This module is responsible for rendering a terminal to an OpenGL context

use super::textureatlas::{Atlas, Sprite, SpriteSlice, TEX_SIZE};
use crate::config::TextStyle;
use crate::font::{FontConfiguration, GlyphInfo};
use euclid;
use failure::{err_msg, Error};
use glium::backend::Facade;
use glium::texture::SrgbTexture2d;
use glium::{self, IndexBuffer, Surface, VertexBuffer};
use glium::{implement_vertex, uniform};
use std::cell::RefCell;
use std::collections::HashMap;
use std::mem;
use std::ops::{Deref, Range};
use std::rc::Rc;
use term::color::RgbaTuple;
use term::{self, CursorPosition, Line, Underline};

type Transform3D = euclid::Transform3D<f32>;

#[derive(Copy, Clone, Debug)]
struct Point(euclid::Point2D<f32>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct GlyphKey {
    font_idx: usize,
    glyph_pos: u32,
    style: TextStyle,
}

/// Caches a rendered glyph.
/// The image data may be None for whitespace glyphs.
#[derive(Debug)]
struct CachedGlyph {
    has_color: bool,
    x_offset: isize,
    y_offset: isize,
    bearing_x: isize,
    bearing_y: isize,
    texture: Option<Sprite>,
    scale: f32,
}

impl Default for Point {
    fn default() -> Point {
        Point::new(0.0, 0.0)
    }
}

impl Deref for Point {
    type Target = euclid::Point2D<f32>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

unsafe impl glium::vertex::Attribute for Point {
    #[inline]
    fn get_type() -> glium::vertex::AttributeType {
        glium::vertex::AttributeType::F32F32
    }
}

impl Point {
    fn new(x: f32, y: f32) -> Self {
        Self {
            0: euclid::point2(x, y),
        }
    }
}

/// Each cell is composed of two triangles built from 4 vertices.
/// The buffer is organized row by row.
const VERTICES_PER_CELL: usize = 4;
const V_TOP_LEFT: usize = 0;
const V_TOP_RIGHT: usize = 1;
const V_BOT_LEFT: usize = 2;
const V_BOT_RIGHT: usize = 3;

#[derive(Copy, Clone, Debug, Default)]
struct Vertex {
    // pre-computed by compute_vertices and changed only on resize
    position: Point,
    // adjustment for glyph size, recomputed each time the cell changes
    adjust: Point,
    // texture coords are updated as the screen contents change
    tex: (f32, f32),
    // cell foreground and background color
    fg_color: (f32, f32, f32, f32),
    bg_color: (f32, f32, f32, f32),
    /// Nominally a boolean, but the shader compiler hated it
    has_color: f32,
    /// Count of how many underlines there are
    underline: f32,
    strikethrough: f32,
    v_idx: f32,
}

implement_vertex!(
    Vertex,
    position,
    adjust,
    tex,
    fg_color,
    bg_color,
    has_color,
    underline,
    strikethrough,
    v_idx,
);

#[cfg(any(windows, feature = "force-glutin", target_os = "macos"))]
const VERTEX_SHADER: &str = r#"
#version 330
in vec2 position;
in vec2 adjust;
in vec2 tex;
in vec4 fg_color;
in vec4 bg_color;
in float has_color;
in float underline;
in float v_idx;

uniform mat4 projection;
uniform mat4 translation;
uniform bool bg_and_line_layer;

out vec2 tex_coords;
out vec2 underline_coords;
out vec4 o_fg_color;
out vec4 o_bg_color;
out float o_has_color;
out float o_underline;

// Offset from the RHS texture coordinate to the LHS.
// This is an underestimation to avoid the shader interpolating
// the underline gylph into its neighbor.
const float underline_offset = (1.0 / 5.0);

void main() {
    o_fg_color = fg_color;
    o_bg_color = bg_color;
    o_has_color = has_color;
    o_underline = underline;

    if (bg_and_line_layer) {
        gl_Position = projection * vec4(position, 0.0, 1.0);

        if (underline != 0.0) {
            // Populate the underline texture coordinates based on the
            // v_idx (which tells us which corner of the cell we're
            // looking at) and o_underline which corresponds to one
            // of the U_XXX constants defined in the rust code below
            // and which holds the RHS position in the texture coordinate
            // space for the underline texture layer.
            if (v_idx == 0.0) { // top left
                underline_coords = vec2(o_underline - underline_offset, -1.0);
            } else if (v_idx == 1.0) { // top right
                underline_coords = vec2(o_underline, -1.0);
            } else if (v_idx == 2.0) { // bot left
                underline_coords = vec2(o_underline- underline_offset, 0.0);
            } else { // bot right
                underline_coords = vec2(o_underline, 0.0);
            }
        }

    } else {
        gl_Position = projection * vec4(position + adjust, 0.0, 1.0);
        tex_coords = tex;
    }
}
"#;

#[cfg(not(any(windows, feature = "force-glutin", target_os = "macos")))]
const VERTEX_SHADER: &str = r#"
#version 300 es
in vec2 position;
in vec2 adjust;
in vec2 tex;
in vec4 fg_color;
in vec4 bg_color;
in float has_color;
in float underline;
in float v_idx;

uniform mat4 projection;
uniform mat4 translation;
uniform bool bg_and_line_layer;

out vec2 tex_coords;
out vec2 underline_coords;
out vec4 o_fg_color;
out vec4 o_bg_color;
out float o_has_color;
out float o_underline;

// Offset from the RHS texture coordinate to the LHS.
// This is an underestimation to avoid the shader interpolating
// the underline gylph into its neighbor.
const float underline_offset = (1.0 / 5.0);

void main() {
    o_fg_color = fg_color;
    o_bg_color = bg_color;
    o_has_color = has_color;
    o_underline = underline;

    if (bg_and_line_layer) {
        gl_Position = projection * vec4(position, 0.0, 1.0);

        if (underline != 0.0) {
            // Populate the underline texture coordinates based on the
            // v_idx (which tells us which corner of the cell we're
            // looking at) and o_underline which corresponds to one
            // of the U_XXX constants defined in the rust code below
            // and which holds the RHS position in the texture coordinate
            // space for the underline texture layer.
            if (v_idx == 0.0) { // top left
                underline_coords = vec2(o_underline - underline_offset, -1.0);
            } else if (v_idx == 1.0) { // top right
                underline_coords = vec2(o_underline, -1.0);
            } else if (v_idx == 2.0) { // bot left
                underline_coords = vec2(o_underline- underline_offset, 0.0);
            } else { // bot right
                underline_coords = vec2(o_underline, 0.0);
            }
        }

    } else {
        gl_Position = projection * vec4(position + adjust, 0.0, 1.0);
        tex_coords = tex;
    }
}
"#;

/// How many columns the underline texture has
const U_COLS: f32 = 5.0;
/// The glyph has no underline or strikethrough
const U_NONE: f32 = 0.0;
/// The glyph has a single underline.  This value is actually the texture
/// coordinate for the right hand side of the underline.
const U_ONE: f32 = 1.0 / U_COLS;
/// Texture coord for the RHS of the double underline glyph
const U_TWO: f32 = 2.0 / U_COLS;
/// Texture coord for the RHS of the strikethrough glyph
const U_STRIKE: f32 = 3.0 / U_COLS;
/// Texture coord for the RHS of the strikethrough + single underline glyph
const U_STRIKE_ONE: f32 = 4.0 / U_COLS;
/// Texture coord for the RHS of the strikethrough + double underline glyph
const U_STRIKE_TWO: f32 = 5.0 / U_COLS;

#[cfg(any(windows, feature = "force-glutin", target_os = "macos"))]
const FRAGMENT_SHADER: &str = r#"
#version 330
precision mediump float;
in vec2 tex_coords;
in vec2 underline_coords;
in vec4 o_fg_color;
in vec4 o_bg_color;
in float o_has_color;
in float o_underline;

out vec4 color;
uniform sampler2D glyph_tex;
uniform sampler2D underline_tex;
uniform bool bg_and_line_layer;

float multiply_one(float src, float dst, float inv_dst_alpha, float inv_src_alpha) {
    return (src * ((1.0 - inv_src_alpha))) + (dst * (inv_src_alpha)); //+ (src * (inv_dst_alpha)) ;
}

// Alpha-regulated multiply to colorize the glyph bitmap.
// The texture data is pre-multiplied by the alpha, so we need to divide
// by the alpha after multiplying to avoid having the colors be too dark.
vec4 multiply(vec4 src, vec4 dst) {
    float inv_src_alpha = 1.0 - src.a;
    float inv_dst_alpha = 1.0 - dst.a;

    float a = max(0.0, min(1.0,dst.a));
    return vec4(
        max(0.0, min(multiply_one(src.r, dst.r, inv_dst_alpha, inv_src_alpha) , 1.0)),
        max(0.0, min(multiply_one(src.g, dst.g, inv_dst_alpha, inv_src_alpha) , 1.0)),
        max(0.0, min(multiply_one(src.b, dst.b, inv_dst_alpha, inv_src_alpha) , 1.0)),
        a*1.35);
}

void main() {
    if (bg_and_line_layer) {
        color = o_bg_color;
        // If there's an underline/strike glyph, extract the pixel color
        // from the texture.  If the alpha value is non-zero then we'll
        // take that pixel, otherwise we'll use the background color.
        if (o_underline != 0.0) {
            // Compute the pixel color for this location
            vec4 under_color = multiply(o_fg_color, texture(underline_tex, underline_coords));
            if (under_color.a != 0.0) {
                // if the line glyph isn't transparent in this position then
                // we take this pixel color, otherwise we'll leave the color
                // at the background color.
                color = under_color;
            }
        }
    } else {
        color = texture(glyph_tex, tex_coords);
        if (o_has_color == 0.0) {
            // if it's not a color emoji, tint with the fg_color
            //color = multiply(o_fg_color, color);
            color.rgb = o_fg_color.rgb;
        }
    }
}
"#;

#[cfg(not(any(windows, feature = "force-glutin", target_os = "macos")))]
const FRAGMENT_SHADER: &str = r#"
#version 300 es
precision mediump float;
in vec2 tex_coords;
in vec2 underline_coords;
in vec4 o_fg_color;
in vec4 o_bg_color;
in float o_has_color;
in float o_underline;

out vec4 color;
uniform sampler2D glyph_tex;
uniform sampler2D underline_tex;
uniform bool bg_and_line_layer;

float multiply_one(float src, float dst, float inv_dst_alpha, float inv_src_alpha) {
    return (src * dst) + (src * (inv_dst_alpha)) + (dst * (inv_src_alpha));
}

// Alpha-regulated multiply to colorize the glyph bitmap.
// The texture data is pre-multiplied by the alpha, so we need to divide
// by the alpha after multiplying to avoid having the colors be too dark.
vec4 multiply(vec4 src, vec4 dst) {
    float inv_src_alpha = 1.0 - src.a;
    float inv_dst_alpha = 1.0 - dst.a;

    return vec4(
        multiply_one(src.r, dst.r, inv_dst_alpha, inv_src_alpha) / dst.a,
        multiply_one(src.g, dst.g, inv_dst_alpha, inv_src_alpha) / dst.a,
        multiply_one(src.b, dst.b, inv_dst_alpha, inv_src_alpha) / dst.a,
        dst.a);
}

void main() {
    if (bg_and_line_layer) {
        color = o_bg_color;
        // If there's an underline/strike glyph, extract the pixel color
        // from the texture.  If the alpha value is non-zero then we'll
        // take that pixel, otherwise we'll use the background color.
        if (o_underline != 0.0) {
            // Compute the pixel color for this location
            vec4 under_color = multiply(o_fg_color, texture(underline_tex, underline_coords));
            if (under_color.a != 0.0) {
                // if the line glyph isn't transparent in this position then
                // we take this pixel color, otherwise we'll leave the color
                // at the background color.
                color = under_color;
            }
        }
    } else {
        color = texture(glyph_tex, tex_coords);
        if (o_has_color == 0.0) {
            // if it's not a color emoji, tint with the fg_color
            //color = multiply(o_fg_color, color);
            color.rgb = o_fg_color.rgb;
        }
    }
}
"#;

pub struct Renderer {
    width: u16,
    height: u16,
    pub fonts: Rc<FontConfiguration>,
    cell_height: usize,
    cell_width: usize,
    descender: isize,
    glyph_cache: RefCell<HashMap<GlyphKey, Rc<CachedGlyph>>>,
    program: glium::Program,
    glyph_vertex_buffer: RefCell<VertexBuffer<Vertex>>,
    glyph_index_buffer: IndexBuffer<u32>,
    projection: Transform3D,
    atlas: RefCell<Atlas>,
    underline_tex: SrgbTexture2d,
    palette: term::color::ColorPalette,
}

impl Renderer {
    pub fn new<F: Facade>(
        facade: &F,
        width: u16,
        height: u16,
        fonts: &Rc<FontConfiguration>,
        palette: term::color::ColorPalette,
    ) -> Result<Self, Error> {
        let metrics = fonts.default_font_metrics()?;
        let (cell_height, cell_width, descender) =
            (metrics.cell_height, metrics.cell_width, metrics.descender);
        let descender = if descender.is_positive() {
            ((f64::from(descender)) / 64.0).ceil() as isize
        } else {
            ((f64::from(descender)) / 64.0).floor() as isize
        };
        debug!(
            "METRICS: h={} w={} d={}",
            cell_height, cell_width, descender
        );

        let cell_height = cell_height.ceil() as usize;
        let cell_width = cell_width.ceil() as usize;

        let underline_tex = Self::compute_underlines(facade, cell_width, cell_height, descender)?;

        let (glyph_vertex_buffer, glyph_index_buffer) = Self::compute_vertices(
            facade,
            cell_width as f32,
            cell_height as f32,
            f32::from(width),
            f32::from(height),
        )?;

        let source = glium::program::ProgramCreationInput::SourceCode {
            vertex_shader: VERTEX_SHADER,
            fragment_shader: FRAGMENT_SHADER,
            outputs_srgb: true,
            tessellation_control_shader: None,
            tessellation_evaluation_shader: None,
            transform_feedback_varyings: None,
            uses_point_size: false,
            geometry_shader: None,
        };
        let program = glium::Program::new(facade, source)?;

        let atlas = RefCell::new(Atlas::new(facade, TEX_SIZE)?);

        Ok(Self {
            atlas,
            program,
            palette,
            glyph_vertex_buffer: RefCell::new(glyph_vertex_buffer),
            glyph_index_buffer,
            width,
            height,
            fonts: Rc::clone(fonts),
            cell_height,
            cell_width,
            descender,
            glyph_cache: RefCell::new(HashMap::new()),
            projection: Self::compute_projection(f32::from(width), f32::from(height)),
            underline_tex,
        })
    }

    /// Create the texture atlas for the line decoration layer.
    /// This is a bitmap with columns to accomodate the U_XXX
    /// constants defined above.
    fn compute_underlines<F: Facade>(
        facade: &F,
        cell_width: usize,
        cell_height: usize,
        descender: isize,
    ) -> Result<SrgbTexture2d, glium::texture::TextureCreationError> {
        let width = 5 * cell_width;
        let mut underline_data = Vec::with_capacity(width * cell_height * 4);
        underline_data.resize(width * cell_height * 4, 0u8);

        let descender_row = (cell_height as isize + descender) as usize;
        let descender_plus_one = (1 + descender_row).min(cell_height - 1);
        let descender_plus_two = (2 + descender_row).min(cell_height - 1);
        let strike_row = descender_row / 2;

        // First, the single underline.
        // We place this just under the descender position.
        {
            let col = 0;
            let offset = ((width * 4) * descender_plus_one) + (col * 4 * cell_width);
            for i in 0..4 * cell_width {
                underline_data[offset + i] = 0xff;
            }
        }
        // Double underline,
        // We place this at and just below the descender
        {
            let col = 1;
            let offset_one = ((width * 4) * (descender_row)) + (col * 4 * cell_width);
            let offset_two = ((width * 4) * (descender_plus_two)) + (col * 4 * cell_width);
            for i in 0..4 * cell_width {
                underline_data[offset_one + i] = 0xff;
                underline_data[offset_two + i] = 0xff;
            }
        }
        // Strikethrough
        {
            let col = 2;
            let offset = (width * 4) * strike_row + (col * 4 * cell_width);
            for i in 0..4 * cell_width {
                underline_data[offset + i] = 0xff;
            }
        }
        // Strikethrough and single underline
        {
            let col = 3;
            let offset_one = ((width * 4) * descender_plus_one) + (col * 4 * cell_width);
            let offset_two = ((width * 4) * strike_row) + (col * 4 * cell_width);
            for i in 0..4 * cell_width {
                underline_data[offset_one + i] = 0xff;
                underline_data[offset_two + i] = 0xff;
            }
        }
        // Strikethrough and double underline
        {
            let col = 4;
            let offset_one = ((width * 4) * (descender_row)) + (col * 4 * cell_width);
            let offset_two = ((width * 4) * strike_row) + (col * 4 * cell_width);
            let offset_three = ((width * 4) * (descender_plus_two)) + (col * 4 * cell_width);
            for i in 0..4 * cell_width {
                underline_data[offset_one + i] = 0xff;
                underline_data[offset_two + i] = 0xff;
                underline_data[offset_three + i] = 0xff;
            }
        }

        glium::texture::SrgbTexture2d::new(
            facade,
            glium::texture::RawImage2d::from_raw_rgba(
                underline_data,
                (width as u32, cell_height as u32),
            ),
        )
    }

    pub fn scaling_changed<F: Facade>(&mut self, facade: &F) -> Result<(), Error> {
        let metrics = self.fonts.default_font_metrics()?;
        self.cell_height = metrics.cell_height.ceil() as usize;
        self.cell_width = metrics.cell_width.ceil() as usize;
        self.descender = if metrics.descender.is_positive() {
            ((f64::from(metrics.descender)) / 64.0).ceil() as isize
        } else {
            ((f64::from(metrics.descender)) / 64.0).floor() as isize
        };

        self.glyph_cache.borrow_mut().clear();
        self.atlas = RefCell::new(Atlas::new(facade, TEX_SIZE)?);
        self.underline_tex =
            Self::compute_underlines(facade, self.cell_width, self.cell_height, self.descender)?;
        Ok(())
    }

    pub fn recreate_atlas<F: Facade>(&mut self, facade: &F, size: u32) -> Result<(), Error> {
        let atlas = RefCell::new(Atlas::new(facade, size)?);
        self.atlas = atlas;
        self.glyph_cache.borrow_mut().clear();
        Ok(())
    }

    pub fn resize<F: Facade>(&mut self, facade: &F, width: u16, height: u16) -> Result<(), Error> {
        debug!("Renderer resize {},{}", width, height);

        self.width = width;
        self.height = height;
        self.projection = Self::compute_projection(f32::from(width), f32::from(height));

        let (glyph_vertex_buffer, glyph_index_buffer) = Self::compute_vertices(
            facade,
            self.cell_width as f32,
            self.cell_height as f32,
            f32::from(width),
            f32::from(height),
        )?;
        self.glyph_vertex_buffer = RefCell::new(glyph_vertex_buffer);
        self.glyph_index_buffer = glyph_index_buffer;

        Ok(())
    }

    /// Resolve a glyph from the cache, rendering the glyph on-demand if
    /// the cache doesn't already hold the desired glyph.
    fn cached_glyph(&self, info: &GlyphInfo, style: &TextStyle) -> Result<Rc<CachedGlyph>, Error> {
        let key = GlyphKey {
            font_idx: info.font_idx,
            glyph_pos: info.glyph_pos,
            style: style.clone(),
        };

        let mut cache = self.glyph_cache.borrow_mut();

        if let Some(entry) = cache.get(&key) {
            return Ok(Rc::clone(entry));
        }

        let glyph = self.load_glyph(info, style)?;
        cache.insert(key, Rc::clone(&glyph));
        Ok(glyph)
    }

    /// Perform the load and render of a glyph
    fn load_glyph(&self, info: &GlyphInfo, style: &TextStyle) -> Result<Rc<CachedGlyph>, Error> {
        let (has_color, glyph, cell_width, cell_height) = {
            let font = self.fonts.cached_font(style)?;
            let mut font = font.borrow_mut();
            let metrics = font.get_fallback(0)?.metrics();
            let active_font = font.get_fallback(info.font_idx)?;
            let has_color = active_font.has_color();
            let glyph = active_font.rasterize_glyph(info.glyph_pos)?;
            (has_color, glyph, metrics.cell_width, metrics.cell_height)
        };

        let effective_height = cell_height;

        let scale = if (effective_height - (glyph.height as f64)) < -0.99 {
            effective_height / glyph.height as f64
        } else {
            1.0f64
        };

        //let scale = 1.0f64;

        #[cfg_attr(feature = "cargo-clippy", allow(float_cmp))]
        let (x_offset, y_offset) = if scale != 1.0 {
            (info.x_offset * scale, info.y_offset * scale)
        } else {
            (info.x_offset, info.y_offset)
        };

        let glyph = if glyph.width == 0 || glyph.height == 0 {
            // a whitespace glyph
            CachedGlyph {
                texture: None,
                has_color,
                x_offset: x_offset as isize,
                y_offset: y_offset as isize,
                bearing_x: 0,
                bearing_y: 0,
                scale: scale as f32,
            }
        } else {
            let raw_im = glium::texture::RawImage2d::from_raw_rgba(
                glyph.data,
                (glyph.width as u32, glyph.height as u32),
            );

            let tex = self
                .atlas
                .borrow_mut()
                .allocate(raw_im.width, raw_im.height, raw_im)?;

            let bearing_x = (f64::from(glyph.bearing_x) * scale) as isize;
            let bearing_y = (f64::from(glyph.bearing_y) * scale) as isize;

            if glyph.width > 40 || scale < 1.0 {
                eprintln!("cw/(gw)/(xof)/bx: {} ({}, {}) ({}, {}) {}", cell_width, glyph.height, glyph.width, x_offset, y_offset, bearing_x);
            }
            CachedGlyph {
                texture: Some(tex),
                has_color,
                x_offset: x_offset as isize,
                y_offset: y_offset as isize,
                bearing_x,
                bearing_y,
                scale: scale as f32,
            }
        };

        Ok(Rc::new(glyph))
    }

    /// Compute a vertex buffer to hold the quads that comprise the visible
    /// portion of the screen.   We recreate this when the screen is resized.
    /// The idea is that we want to minimize and heavy lifting and computation
    /// and instead just poke some attributes into the offset that corresponds
    /// to a changed cell when we need to repaint the screen, and then just
    /// let the GPU figure out the rest.
    fn compute_vertices<F: Facade>(
        facade: &F,
        cell_width: f32,
        cell_height: f32,
        width: f32,
        height: f32,
    ) -> Result<(VertexBuffer<Vertex>, IndexBuffer<u32>), Error> {
        let mut verts = Vec::new();
        let mut indices = Vec::new();

        let num_cols = (width as usize + 1) / cell_width as usize;
        let num_rows = (height as usize + 1) / cell_height as usize;

        for y in 0..num_rows {
            for x in 0..num_cols {
                let y_pos = (height / -2.0) + (y as f32 * cell_height);
                let x_pos = (width / -2.0) + (x as f32 * cell_width);
                // Remember starting index for this position
                let idx = verts.len() as u32;
                verts.push(Vertex {
                    // Top left
                    position: Point::new(x_pos, y_pos),
                    v_idx: V_TOP_LEFT as f32,
                    ..Default::default()
                });
                verts.push(Vertex {
                    // Top Right
                    position: Point::new(x_pos + cell_width, y_pos),
                    v_idx: V_TOP_RIGHT as f32,
                    ..Default::default()
                });
                verts.push(Vertex {
                    // Bottom Left
                    position: Point::new(x_pos, y_pos + cell_height),
                    v_idx: V_BOT_LEFT as f32,
                    ..Default::default()
                });
                verts.push(Vertex {
                    // Bottom Right
                    position: Point::new(x_pos + cell_width, y_pos + cell_height),
                    v_idx: V_BOT_RIGHT as f32,
                    ..Default::default()
                });

                // Emit two triangles to form the glyph quad
                indices.push(idx);
                indices.push(idx + 1);
                indices.push(idx + 2);
                indices.push(idx + 1);
                indices.push(idx + 2);
                indices.push(idx + 3);
            }
        }
        eprintln!("vertex buffer {}x{} needs {}", num_cols, num_rows, verts.len() * std::mem::size_of_val(&verts[0]));

        Ok((
            VertexBuffer::dynamic(facade, &verts)?,
            IndexBuffer::new(facade, glium::index::PrimitiveType::TrianglesList, &indices)?,
        ))
    }

    /// The projection corrects for the aspect ratio and flips the y-axis
    fn compute_projection(width: f32, height: f32) -> Transform3D {
        Transform3D::ortho(
            -width / 2.0,
            width / 2.0,
            height / 2.0,
            -height / 2.0,
            -1.0,
            1.0,
        )
    }

    /// "Render" a line of the terminal screen into the vertex buffer.
    /// This is nominally a matter of setting the fg/bg color and the
    /// texture coordinates for a given glyph.  There's a little bit
    /// of extra complexity to deal with multi-cell glyphs.
    fn render_screen_line(
        &self,
        line_idx: usize,
        line: &Line,
        selection: Range<usize>,
        cursor: &CursorPosition,
        terminal: &term::Terminal,
    ) -> Result<(), Error> {
        let num_cols = terminal.screen().physical_cols;
        let mut vb = self.glyph_vertex_buffer.borrow_mut();
        let mut vertices = {
            let per_line = num_cols * VERTICES_PER_CELL;
            let start_pos = line_idx * per_line;
            vb.slice_mut(start_pos..start_pos + per_line)
                .ok_or_else(|| err_msg("we're confused about the screen size"))?
                .map()
        };

        let current_highlight = terminal.current_highlight();
        let cell_width = self.cell_width as f32;
        let cell_height = self.cell_height as f32;

        // Break the line into clusters of cells with the same attributes
        let cell_clusters = line.cluster();
        let mut last_cell_idx = 0;
        for cluster in cell_clusters {
            let attrs = &cluster.attrs;
            let is_highlited_hyperlink = match (&attrs.hyperlink, &current_highlight) {
                (&Some(ref this), &Some(ref highlight)) => this == highlight,
                _ => false,
            };
            let style = self.fonts.match_style(attrs);

            let bg_color = self.palette.resolve_bg(&attrs.background);
            let fg_color = match attrs.foreground {
                term::color::ColorAttribute::Default => {
                    if let Some(fg) = style.foreground {
                        fg
                    } else {
                        self.palette.resolve_fg(&attrs.foreground)
                    }
                }
                term::color::ColorAttribute::PaletteIndex(idx) if idx < 8 => {
                    // For compatibility purposes, switch to a brighter version
                    // of one of the standard ANSI colors when Bold is enabled.
                    // This lifts black to dark grey.
                    let idx = if attrs.intensity() == term::Intensity::Bold {
                        idx + 8
                    } else {
                        idx
                    };
                    self.palette
                        .resolve_fg(&term::color::ColorAttribute::PaletteIndex(idx))
                }
                _ => self.palette.resolve_fg(&attrs.foreground),
            };

            let (fg_color, bg_color) = {
                let mut fg = fg_color;
                let mut bg = bg_color;

                if attrs.reverse() {
                    mem::swap(&mut fg, &mut bg);
                }

                (fg, bg)
            };

            let glyph_color = fg_color.to_tuple_rgba();
            let bg_color = bg_color.to_tuple_rgba();

            // Shape the printable text from this cluster
            let glyph_info = {
                let font = self.fonts.cached_font(style)?;
                let mut font = font.borrow_mut();
                font.shape(&cluster.text)?
            };

            for info in &glyph_info {
                let cell_idx = cluster.byte_to_cell_idx[info.cluster as usize];
                let glyph = self.cached_glyph(info, style)?;

                let left: f32 = glyph.x_offset as f32 + glyph.bearing_x as f32 + info.x_offset as f32;
                let top = (self.cell_height as f32 + self.descender as f32)
                    - (glyph.y_offset as f32 + glyph.bearing_y as f32);

                // underline and strikethrough
                // Figure out what we're going to draw for the underline.
                // If the current cell is part of the current URL highlight
                // then we want to show the underline.
                #[cfg_attr(feature = "cargo-clippy", allow(match_same_arms))]
                let underline: f32 = match (
                    is_highlited_hyperlink,
                    attrs.strikethrough(),
                    attrs.underline(),
                ) {
                    (true, false, Underline::None) => U_ONE,
                    (true, false, Underline::Single) => U_TWO,
                    (true, false, Underline::Double) => U_ONE,
                    (true, true, Underline::None) => U_STRIKE_ONE,
                    (true, true, Underline::Single) => U_STRIKE_TWO,
                    (true, true, Underline::Double) => U_STRIKE_ONE,
                    (false, false, Underline::None) => U_NONE,
                    (false, false, Underline::Single) => U_ONE,
                    (false, false, Underline::Double) => U_TWO,
                    (false, true, Underline::None) => U_STRIKE,
                    (false, true, Underline::Single) => U_STRIKE_ONE,
                    (false, true, Underline::Double) => U_STRIKE_TWO,
                };

                // Iterate each cell that comprises this glyph.  There is usually
                // a single cell per glyph but combining characters, ligatures
                // and emoji can be 2 or more cells wide.
                for glyph_idx in 0..info.num_cells as usize {
                    let cell_idx = cell_idx + glyph_idx;

                    if cell_idx >= num_cols {
                        // terminal line data is wider than the window.
                        // This happens for example while live resizing the window
                        // smaller than the terminal.
                        break;
                    }
                    last_cell_idx = cell_idx;

                    let (glyph_color, bg_color) = self.compute_cell_fg_bg(
                        line_idx,
                        cell_idx,
                        cursor,
                        &selection,
                        glyph_color,
                        bg_color,
                    );

                    let vert_idx = cell_idx * VERTICES_PER_CELL;
                    let vert = &mut vertices[vert_idx..vert_idx + VERTICES_PER_CELL];

                    vert[V_TOP_LEFT].fg_color = glyph_color;
                    vert[V_TOP_RIGHT].fg_color = glyph_color;
                    vert[V_BOT_LEFT].fg_color = glyph_color;
                    vert[V_BOT_RIGHT].fg_color = glyph_color;

                    vert[V_TOP_LEFT].bg_color = bg_color;
                    vert[V_TOP_RIGHT].bg_color = bg_color;
                    vert[V_BOT_LEFT].bg_color = bg_color;
                    vert[V_BOT_RIGHT].bg_color = bg_color;

                    vert[V_TOP_LEFT].underline = underline;
                    vert[V_TOP_RIGHT].underline = underline;
                    vert[V_BOT_LEFT].underline = underline;
                    vert[V_BOT_RIGHT].underline = underline;

                    match glyph.texture {
                        Some(ref texture) => {
                            let slice = SpriteSlice {
                                cell_idx: glyph_idx,
                                num_cells: info.num_cells as usize,
                                cell_width: self.cell_width,
                                scale: glyph.scale,
                                left_offset: left as i32,
                            };
                            // How much of the width of this glyph we can use here
                            let slice_width = texture.slice_width(&slice);

                            let left = if glyph_idx == 0 { left } else { 0.0 };
                            let right = (slice_width as f32 + left) - cell_width;

                            let bottom =
                                ((texture.coords.height as f32) * glyph.scale + top) - cell_height;

                            vert[V_TOP_LEFT].tex = texture.top_left(&slice);
                            vert[V_TOP_LEFT].adjust = Point::new(left, top);

                            vert[V_TOP_RIGHT].tex = texture.top_right(&slice);
                            vert[V_TOP_RIGHT].adjust = Point::new(right, top);

                            vert[V_BOT_LEFT].tex = texture.bottom_left(&slice);
                            vert[V_BOT_LEFT].adjust = Point::new(left, bottom);

                            vert[V_BOT_RIGHT].tex = texture.bottom_right(&slice);
                            vert[V_BOT_RIGHT].adjust = Point::new(right, bottom);

                            let has_color = if glyph.has_color { 1.0 } else { 0.0 };
                            vert[V_TOP_LEFT].has_color = has_color;
                            vert[V_TOP_RIGHT].has_color = has_color;
                            vert[V_BOT_LEFT].has_color = has_color;
                            vert[V_BOT_RIGHT].has_color = has_color;
                        }
                        None => {
                            // Whitespace; no texture to render
                            let zero = (0.0, 0.0f32);

                            // Note: these 0 coords refer to the blank pixel
                            // in the bottom left of the underline texture!
                            vert[V_TOP_LEFT].tex = zero;
                            vert[V_TOP_RIGHT].tex = zero;
                            vert[V_BOT_LEFT].tex = zero;
                            vert[V_BOT_RIGHT].tex = zero;

                            vert[V_TOP_LEFT].adjust = Default::default();
                            vert[V_TOP_RIGHT].adjust = Default::default();
                            vert[V_BOT_LEFT].adjust = Default::default();
                            vert[V_BOT_RIGHT].adjust = Default::default();

                            vert[V_TOP_LEFT].has_color = 0.0;
                            vert[V_TOP_RIGHT].has_color = 0.0;
                            vert[V_BOT_LEFT].has_color = 0.0;
                            vert[V_BOT_RIGHT].has_color = 0.0;
                        }
                    }
                }
            }
        }

        // Clear any remaining cells to the right of the clusters we
        // found above, otherwise we leave artifacts behind.  The easiest
        // reproduction for the artifacts is to maximize the window and
        // open a vim split horizontally.  Backgrounding vim would leave
        // the right pane with its prior contents instead of showing the
        // cleared lines from the shell in the main screen.

        for cell_idx in last_cell_idx + 1..num_cols {
            let vert_idx = cell_idx * VERTICES_PER_CELL;
            let vert_slice = &mut vertices[vert_idx..vert_idx + 4];

            // Even though we don't have a cell for these, they still
            // hold the cursor or the selection so we need to compute
            // the colors in the usual way.
            let (glyph_color, bg_color) = self.compute_cell_fg_bg(
                line_idx,
                cell_idx,
                cursor,
                &selection,
                self.palette.foreground.to_tuple_rgba(),
                self.palette.background.to_tuple_rgba(),
            );

            for vert in vert_slice.iter_mut() {
                vert.bg_color = bg_color;
                vert.fg_color = glyph_color;
                vert.underline = U_NONE;
                // Note: these 0 coords refer to the blank pixel
                // in the bottom left of the underline texture!
                vert.tex = (0.0, 0.0);
                vert.adjust = Default::default();
                vert.has_color = 0.0;
            }
        }

        Ok(())
    }

    fn compute_cell_fg_bg(
        &self,
        line_idx: usize,
        cell_idx: usize,
        cursor: &CursorPosition,
        selection: &Range<usize>,
        fg_color: RgbaTuple,
        bg_color: RgbaTuple,
    ) -> (RgbaTuple, RgbaTuple) {
        let selected = term::in_range(cell_idx, selection);
        let is_cursor = line_idx as i64 == cursor.y && cursor.x == cell_idx;
        //trace!("compute_cell_ggf_bg: {}, {} @ {:?}  isc:{}", line_idx, cell_idx, (cursor.x, cursor.y), is_cursor);

        let (fg_color, bg_color) = match (selected, is_cursor) {
            // Normally, render the cell as configured
            (false, false) => (fg_color, bg_color),
            // Cursor cell overrides colors
            (_, true) => (
                self.palette.cursor_fg.to_tuple_rgba(),
                self.palette.cursor_bg.to_tuple_rgba(),
            ),
            // Selected text overrides colors
            (true, false) => (
                self.palette.selection_fg.to_tuple_rgba(),
                self.palette.selection_bg.to_tuple_rgba(),
            ),
        };

        (fg_color, bg_color)
    }

    pub fn paint(
        &mut self,
        target: &mut glium::Frame,
        term: &mut term::Terminal,
    ) -> Result<(), Error> {
        let background_color = self
            .palette
            .resolve_bg(&term::color::ColorAttribute::Default);
        //eprintln!("{:?}", background_color);
        let (r, g, b, a) = background_color.to_tuple_rgba();
        target.clear_color(r, g, b, a);

        let cursor = term.cursor_pos();
        {
            let dirty_lines = term.get_dirty_lines();

            for (line_idx, line, selrange) in dirty_lines {
                self.render_screen_line(line_idx, line, selrange, &cursor, term)?;
            }
        }

        let tex = self.atlas.borrow().texture();

        //eprintln!("Draw: {:?}", std::time::Instant::now());
        // Pass 1: Draw backgrounds, strikethrough and underline
        target.draw(
            &*self.glyph_vertex_buffer.borrow(),
            &self.glyph_index_buffer,
            &self.program,
            &uniform! {
                projection: self.projection.to_column_arrays(),
                glyph_tex: &*tex,
                bg_and_line_layer: true,
                underline_tex: &self.underline_tex,
            },
            &glium::DrawParameters {
                blend: glium::Blend::alpha_blending(),
                //color_mask: (true, false, true, true),
                ..Default::default()
            },
        )?;

        // Pass 2: Draw glyphs
        target.draw(
            &*self.glyph_vertex_buffer.borrow(),
            &self.glyph_index_buffer,
            &self.program,
            &uniform! {
                projection: self.projection.to_column_arrays(),
                glyph_tex: &*tex,
                bg_and_line_layer: false,
            },
            &glium::DrawParameters {
                blend: glium::Blend::alpha_blending(),
                //color_mask: (true, false, true, false),
                ..Default::default()
            },
        )?;

        term.clean_dirty_lines();
        Ok(())
    }
}
