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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;
import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TextUI;
import javax.swing.plaf.basic.BasicTextAreaUI;
import javax.swing.text.*;

import com.github.weisj.darklaf.util.PropertyKey;

/**
 * @author Jannis Weis
 */
public class DarkTextAreaUI extends DarkTextUI {

    private static final JTextArea area = new JTextArea() {
        @Override
        public void setUI(final TextUI ui) {}
    };
    private static final BasicTextAreaUI basicTextAreaUI = new BasicTextAreaUIWrapper();

    public static ComponentUI createUI(final JComponent ta) {
        return new DarkTextAreaUI();
    }

    /*
     * Implementation of BasicTextAreaUI
     */
    /**
     * This method gets called when a bound property is changed on the associated JTextComponent. This is a hook which
     * UI implementations may change to reflect how the UI displays bound properties of JTextComponent subclasses. This
     * is implemented to rebuild the View when the
     * <em>WrapLine</em> or the <em>WrapStyleWord</em> property changes.
     *
     * @param evt the property change event
     */
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        if (evt.getPropertyName().equals("lineWrap") ||
            evt.getPropertyName().equals("wrapStyleWord") ||
            evt.getPropertyName().equals("tabSize")) {
            // rebuild the view
            modelChanged();
        } else if (PropertyKey.EDITABLE.equals(evt.getPropertyName())) {
            updateFocusTraversalKeys();
        }
    }

    protected String getPropertyPrefix() {
        return "TextArea";
    }

    /**
     * Creates the view for an element. Returns a WrappedPlainView or PlainView.
     *
     * @param  elem the element
     * @return      the view
     */
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

    /**
     * Returns the baseline.
     *
     * @see   javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public int getBaseline(final JComponent c, final int width, final int height) {
        return basicTextAreaUI.getBaseline(c, width, height);
    }

    /**
     * Returns an enum indicating how the baseline of the component changes as the size changes.
     *
     * @see   javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(final JComponent c) {
        return basicTextAreaUI.getBaselineResizeBehavior(c);
    }

    @Override
    protected DarkCaret.CaretStyle getDefaultCaretStyle() {
        return DarkCaret.CaretStyle.VERTICAL_LINE_STYLE;
    }

    protected static class BasicTextAreaUIWrapper extends BasicTextAreaUI {

        @Override
        public void installUI(final JComponent c) {
            super.installUI(c);
        }

        @Override
        protected void installDefaults() {}

        @Override
        protected void installKeyboardActions() {}

        @Override
        protected void installListeners() {}
    }
}
