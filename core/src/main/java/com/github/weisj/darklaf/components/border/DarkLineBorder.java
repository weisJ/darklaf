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
package com.github.weisj.darklaf.components.border;

import java.awt.*;
import java.util.function.Function;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.Theme;

public class DarkLineBorder extends MutableLineBorder {

    private final String key;
    private final Function<Color, Color> fallbackSupplier;
    protected Theme currentTheme;

    public DarkLineBorder(final int top, final int left, final int bottom, final int right, final String key,
            final Function<Color, Color> fallbackSupplier) {
        super(top, left, bottom, right, null);
        this.key = key;
        this.fallbackSupplier = fallbackSupplier;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        g.setColor(getColor(c));
        g.fillRect(x, y, width - right, top);
        g.fillRect(x, y + top, left, height - top);
        g.fillRect(x + left, y + height - bottom, width - left, bottom);
        g.fillRect(x + width - right, y, right, height - bottom);
    }

    protected Color getColor(final Component c) {
        if (!LafManager.isInstalled()) {
            currentTheme = null;
            Color bg = c.getBackground();
            return bg != null ? fallbackSupplier.apply(bg) : null;
        }
        Color color = getColor();
        Theme installed = LafManager.getInstalledTheme();
        if (color == null || currentTheme != installed) {
            currentTheme = installed;
            color = UIManager.getColor(key);
            setColor(color);
        }
        return color;
    }
}
