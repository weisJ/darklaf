/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.components.text;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

/**
 * JTextPane that keeps the original width of text.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class NonWrappingTextArea extends JTextArea {

    @Override
    public Dimension getPreferredSize() {
        // Avoid substituting the minimum width for the preferred width
        // when the viewport is too narrow.
        return getUI().getPreferredSize(this);
    }

    @Override
    public boolean getScrollableTracksViewportWidth() {
        final Component parent = getParent();
        final ComponentUI ui = getUI();
        return parent == null || (ui.getPreferredSize(this).width <= parent.getSize().width);
    }
}
