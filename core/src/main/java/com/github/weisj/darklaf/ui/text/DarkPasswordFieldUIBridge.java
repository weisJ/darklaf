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

import javax.swing.*;
import javax.swing.plaf.basic.BasicPasswordFieldUI;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Element;
import javax.swing.text.PasswordView;
import javax.swing.text.View;

/**
 * This class is an exact copy of the implementation of {@link BasicPasswordFieldUI}. In this way it is possible to
 * contain all Laf specific methods in {@link DarkPasswordFieldUI}, without having to extends {@link
 * BasicPasswordFieldUI} directly and instead extend the {@link DarkTextFieldUI} base class.
 *
 * @author Jannis Weis
 */
public abstract class DarkPasswordFieldUIBridge extends DarkTextFieldUI {

    /**
     * Installs the necessary properties on the JPasswordField.
     *
     * @since 1.6
     */
    protected void installDefaults() {
        super.installDefaults();
        String prefix = getPropertyPrefix();
        Character echoChar = (Character) UIManager.getDefaults().get(prefix + ".echoChar");
        if (echoChar != null) {
            LookAndFeel.installProperty(getComponent(), "echoChar", echoChar);
        }
    }

    /**
     * Fetches the name used as a key to look up properties through the UIManager. This is used as a prefix to all the
     * standard text properties.
     *
     * @return the name ("PasswordField")
     */
    protected String getPropertyPrefix() {
        return "PasswordField";
    }

    /**
     * Creates a view (PasswordView) for an element.
     *
     * @param  elem the element
     * @return      the view
     */
    public View create(final Element elem) {
        return new PasswordView(elem);
    }

    /**
     * Create the action map for Password Field. This map provides same actions for double mouse click and and for
     * triple mouse click (see bug 4231444).
     */
    public ActionMap createActionMap() {
        ActionMap map = super.createActionMap();
        if (map.get(DefaultEditorKit.selectWordAction) != null) {
            Action a = map.get(DefaultEditorKit.selectLineAction);
            if (a != null) {
                map.remove(DefaultEditorKit.selectWordAction);
                map.put(DefaultEditorKit.selectWordAction, a);
            }
        }
        return map;
    }
}
