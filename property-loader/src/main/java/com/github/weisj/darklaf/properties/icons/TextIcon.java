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
package com.github.weisj.darklaf.properties.icons;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.util.FontUtil;

public class TextIcon implements Icon, DerivableIcon<TextIcon> {

    private final String text;
    private final Color color;
    private final Font font;
    private final int width;
    private final int height;
    private final int textOffset;

    public TextIcon(final String text, final Color color, final Font font, final int width, final int height) {
        this(text, color, font, width, height, 0);
    }

    public TextIcon(final String text, final Color color, final Font font, final int width, final int height,
            final int textOffset) {
        this.text = text;
        this.color = color;
        this.font = font;
        this.width = width;
        this.height = height;
        this.textOffset = textOffset;
    }

    @Override
    public void paintIcon(final Component c, final Graphics gg, final int x, final int y) {
        Graphics2D g = (Graphics2D) gg.create();
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_DEFAULT);
        g.setFont(font);
        FontMetrics fm = g.getFontMetrics(font);
        int w = fm.stringWidth(text);
        int textX = x + (getIconWidth() - w) / 2;
        int textY = y + FontUtil.getCenteredFontPosition(height, fm) + fm.getAscent();
        textY += textOffset;
        g.setColor(color);
        g.drawString(text, textX, textY);
        g.dispose();
    }

    @Override
    public int getIconWidth() {
        return width;
    }

    @Override
    public int getIconHeight() {
        return height;
    }

    @Override
    public TextIcon derive(final int width, final int height) {
        int off = Math.round(height * ((float) this.textOffset / this.height));
        Font f = font.deriveFont(height * (font.getSize2D() / this.height));
        return new TextIcon(text, color, f, width, height, off);
    }
}
