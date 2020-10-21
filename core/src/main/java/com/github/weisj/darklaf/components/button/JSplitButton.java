/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.components.button;

import java.awt.event.ActionListener;

import javax.swing.*;

public class JSplitButton extends JButton {

    public static final String KEY_ACTION_ADDED = "addedAction";
    public static final String KEY_ACTION_REMOVED = "removedAction";

    private JPopupMenu actionMenu;

    /**
     * Creates a button with no set text or icon.
     */
    public JSplitButton() {
        super();
    }

    /**
     * Creates a button with an icon.
     *
     * @param icon the Icon image to display on the button
     */
    public JSplitButton(final Icon icon) {
        super(icon);
    }

    /**
     * Creates a button with text.
     *
     * @param text the text of the button
     */
    public JSplitButton(final String text) {
        super(text);
    }

    /**
     * Creates a button where properties are taken from the <code>Action</code> supplied.
     *
     * @param a the <code>Action</code> used to specify the new button
     *
     * @since 1.3
     */
    public JSplitButton(final Action a) {
        super(a);
    }

    /**
     * Creates a button with initial text and an icon.
     *
     * @param text the text of the button
     * @param icon the Icon image to display on the button
     */
    public JSplitButton(final String text, final Icon icon) {
        super(text, icon);
    }

    public int getActionCount() {
        return listenerList.getListenerCount(ActionListener.class);
    }

    @Override
    public String getUIClassID() {
        return "SplitButtonUI";
    }

    @Override
    public void addActionListener(final ActionListener l) {
        super.addActionListener(l);
        firePropertyChange(KEY_ACTION_ADDED, null, l);
    }

    @Override
    public void removeActionListener(final ActionListener l) {
        super.removeActionListener(l);
        firePropertyChange(KEY_ACTION_REMOVED, l, null);
    }

    public JPopupMenu getActionMenu() {
        if (actionMenu == null) {
            actionMenu = new JPopupMenu();
        }
        return actionMenu;
    }

    @Override
    public void updateUI() {
        super.updateUI();
        if (actionMenu != null) {
            SwingUtilities.updateComponentTreeUI(actionMenu);
        }
    }

    public void setActionMenu(final JPopupMenu actionMenu) {
        this.actionMenu = actionMenu;
    }
}
