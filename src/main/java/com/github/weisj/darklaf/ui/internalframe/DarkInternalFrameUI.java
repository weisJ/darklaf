/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.internalframe;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicInternalFrameUI;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkInternalFrameUI extends BasicInternalFrameUI {

    public DarkInternalFrameUI(final JInternalFrame b) {
        super(b);
    }

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent b) {
        return new DarkInternalFrameUI((JInternalFrame) b);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installProperty(frame, "opaque", false);
    }

    @Override
    protected JComponent createNorthPane(final JInternalFrame w) {
        this.titlePane = new DarkInternalFrameTitlePane(w);
        this.titlePane.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 0));
        return this.titlePane;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        super.paint(g, c);
    }
}
