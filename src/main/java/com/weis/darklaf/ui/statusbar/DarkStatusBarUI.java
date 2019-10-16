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
package com.weis.darklaf.ui.statusbar;

import org.jdesktop.swingx.JXStatusBar;
import org.jdesktop.swingx.plaf.basic.BasicStatusBarUI;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkStatusBarUI extends BasicStatusBarUI {


    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkStatusBarUI();
    }

    protected Color background;
    protected Color borderColor;

    @Override
    protected void installDefaults(final JXStatusBar sb) {
        super.installDefaults(sb);
        background = UIManager.getColor("StatusBar.background");
        borderColor = UIManager.getColor("StatusBar.topColor");
    }

    @Override
    protected void paintBackground(final Graphics2D g, @NotNull final JXStatusBar bar) {
        if (bar.isOpaque()) {
            g.setColor(background);
            g.fillRect(0, 0, bar.getWidth(), bar.getHeight());
        }
        g.setColor(borderColor);
        g.fillRect(0, 0, bar.getWidth(), 1);
    }
}
