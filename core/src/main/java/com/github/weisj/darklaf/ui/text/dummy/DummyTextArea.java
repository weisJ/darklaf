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
package com.github.weisj.darklaf.ui.text.dummy;

import java.awt.*;
import java.awt.event.InputMethodListener;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelListener;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TextUI;
import javax.swing.text.Caret;
import javax.swing.text.Document;
import javax.swing.text.Highlighter;

public class DummyTextArea extends JTextArea {

    @Override
    protected void setUI(final ComponentUI newUI) {}

    @Override
    public void setUI(final TextUI ui) {}

    @Override
    public void updateUI() {}

    @Override
    public void addPropertyChangeListener(final PropertyChangeListener listener) {}

    @Override
    public synchronized void addMouseListener(final MouseListener l) {}

    @Override
    public synchronized void addMouseMotionListener(final MouseMotionListener l) {}

    @Override
    public synchronized void addMouseWheelListener(final MouseWheelListener l) {}

    @Override
    public void addInputMethodListener(final InputMethodListener l) {}

    @Override
    public void setHighlighter(final Highlighter h) {}

    @Override
    public void setTransferHandler(final TransferHandler newHandler) {}

    @Override
    public void setCaret(final Caret c) {}

    @Override
    public void setDocument(final Document doc) {}

    @Override
    public void setDisabledTextColor(final Color c) {}

    @Override
    public void setDragEnabled(final boolean b) {}

    @Override
    public LayoutManager getLayout() {
        return null;
    }

    @Override
    public void setLayout(final LayoutManager mgr) {
        if (mgr instanceof DocumentListener) {
            Document doc = getDocument();
            if (doc != null) doc.removeDocumentListener((DocumentListener) mgr);
        }
    }
}
