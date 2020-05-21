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
 *
 */
package com.github.weisj.darklaf.ui.text.bridge;

import java.awt.*;
import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.text.Element;
import javax.swing.text.JTextComponent;
import javax.swing.text.View;

import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.text.dummy.DummyTextArea;
import com.github.weisj.darklaf.ui.text.dummy.DummyTextAreaUI;
import com.github.weisj.darklaf.util.PropertyKey;

/**
 * @author Jannis Weis
 */
public abstract class DarkTextAreaUIBridge extends DarkTextUI {

    private static final JTextArea area = new DummyTextArea();
    private static final DummyTextAreaUI basicTextAreaUI = new DummyTextAreaUI();

    /*
     * Implementation of BasicTextAreaUI
     */
    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        if (evt.getPropertyName().equals("lineWrap") ||
            evt.getPropertyName().equals("wrapStyleWord") ||
            evt.getPropertyName().equals("tabSize")) {
            // rebuild the view
            modelChanged();
        } else if (PropertyKey.EDITABLE.equals(evt.getPropertyName())) {
            basicTextAreaUI.propertyChange(evt);
        }
    }

    @Override
    public View create(final Element elem) {
        JTextComponent editor = getComponent();
        if (editor instanceof JTextArea) {
            JTextArea c = (JTextArea) editor;
            area.setLineWrap(c.getLineWrap());
            area.setWrapStyleWord(c.getWrapStyleWord());
            basicTextAreaUI.installUI(area);
        }
        return basicTextAreaUI.create(elem);
    }

    @Override
    public int getBaseline(final JComponent c, final int width, final int height) {
        return basicTextAreaUI.getBaseline(c, width, height);
    }

    @Override
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(final JComponent c) {
        return basicTextAreaUI.getBaselineResizeBehavior(c);
    }
}
