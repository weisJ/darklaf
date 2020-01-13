/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.github.weisj.darklaf.icons;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.Theme;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.net.URI;

/**
 * @author Jannis Weis
 */
public class ThemedSVGIcon extends DarkSVGIcon {

    private Theme currentTheme;

    public ThemedSVGIcon(@NotNull final URI uri, final int displayWidth, final int displayHeight) {
        super(uri, displayWidth, displayHeight);
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureTheme();
        super.paintIcon(c, g, x, y);
    }

    private void ensureTheme() {
        Theme theme = LafManager.getTheme();
        if (currentTheme != theme) {
            IconColorMapper.patchColors(getSVGIcon());
            currentTheme = theme;
        }
    }
}
