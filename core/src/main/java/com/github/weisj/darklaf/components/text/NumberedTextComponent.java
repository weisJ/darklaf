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
package com.github.weisj.darklaf.components.text;

import java.awt.*;

import javax.swing.*;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.components.OverlayScrollPane;

public class NumberedTextComponent extends JPanel {

    protected final OverlayScrollPane overlayScrollPane;
    protected final NumberingPane numberingPane;
    protected final JTextComponent textComponent;

    public NumberedTextComponent(final JTextComponent textComponent) {
        super(new BorderLayout());
        this.textComponent = textComponent;
        overlayScrollPane = new OverlayScrollPane(textComponent);
        numberingPane = new NumberingPane();
        numberingPane.setTextComponent(textComponent);
        overlayScrollPane.getVerticalScrollBar().setBlockIncrement(textComponent.getFont().getSize());
        overlayScrollPane.getScrollPane().setRowHeaderView(numberingPane);
        add(overlayScrollPane, BorderLayout.CENTER);
    }

    public NumberingPane getNumberingPane() {
        return numberingPane;
    }

    public JTextComponent getTextComponent() {
        return textComponent;
    }
}
