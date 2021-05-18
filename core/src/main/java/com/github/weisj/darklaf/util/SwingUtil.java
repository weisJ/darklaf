/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.util;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;

import javax.swing.JComponent;

import sun.swing.SwingUtilities2;

public final class SwingUtil {

    private SwingUtil() {}

    public static FontMetrics getFontMetrics(final JComponent c, final Graphics g) {
        if (g == null) throw new IllegalArgumentException("Graphics must not be null");
        return getFontMetrics(c, g.getFont());
    }

    public static FontMetrics getFontMetrics(final JComponent c, final Font font) {
        if (c == null) throw new IllegalArgumentException("Component must not be null");
        if (font == null) throw new IllegalArgumentException("Font must not be null");
        // Note: We assume that we're using the FontMetrics
        // from the widget to layout out text, otherwise we can get
        // mismatches when printing.
        return c.getFontMetrics(font);
    }

    public static void drawStringUnderlineCharAt(JComponent c, Graphics g,
            String text, int underlinedIndex, int x, int y) {
        SwingUtilities2.drawStringUnderlineCharAt(c, g, text, underlinedIndex, x, y);
    }

    public static void drawString(final JComponent c, final Graphics g, final String text, int x, int y) {
        SwingUtilities2.drawString(c, g, text, x, y);
    }

    public static int stringWidth(final JComponent c, final FontMetrics fm, final String string) {
        return SwingUtilities2.stringWidth(c, fm, string);
    }
}
