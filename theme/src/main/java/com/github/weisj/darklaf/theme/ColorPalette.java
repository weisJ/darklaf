/*
 * MIT License
 *
 * Copyright (c) 2021-2024 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
package com.github.weisj.darklaf.theme;

import java.awt.Color;

import com.github.weisj.darklaf.properties.color.DynamicColor;

/**
 * Convenience accessor for the palette colors defined in all themes. The color references obtained
 * from this class are always valid and don't need to be renewed when the theme changes.
 * <p>
 * Note that the names for all the colors are only approximating the true color as themes are free
 * to colors different to what the names imply. However the general hue of the color will match the
 * names.
 */
public final class ColorPalette {
    private ColorPalette() {
        throw new IllegalStateException("Utility class constructor");
    }

    public static final Color YELLOW = new PaletteColor("palette.yellow");
    public static final Color ORANGE = new PaletteColor("palette.orange");
    public static final Color RED = new PaletteColor("palette.red");
    public static final Color PINK = new PaletteColor("palette.pink");
    public static final Color PURPLE = new PaletteColor("palette.purple");
    public static final Color INDIGO = new PaletteColor("palette.indigo");
    public static final Color BLUE = new PaletteColor("palette.blue");
    public static final Color TEAL = new PaletteColor("palette.teal");
    public static final Color CYAN = new PaletteColor("palette.cyan");
    public static final Color GREEN = new PaletteColor("palette.green");
    public static final Color LIME = new PaletteColor("palette.lime");
    public static final Color FOREST = new PaletteColor("palette.forest");
    public static final Color BROWN = new PaletteColor("palette.brown");
    public static final Color GRAY = new PaletteColor("palette.gray");

    public static void invalidatePalette() {
        PaletteColor.currentThemeKey = new Object();
    }

    private static class PaletteColor extends DynamicColor {
        private static Object currentThemeKey;

        public PaletteColor(final String key) {
            super(key);
        }

        @Override
        protected Object getCurrentDynamicKey() {
            return currentThemeKey;
        }
    }
}
