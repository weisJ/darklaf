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
package com.github.weisj.darklaf.components.tristate;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.*;

import com.github.weisj.darklaf.theme.laf.ThemedLookAndFeel;

public class TristateCheckBoxMenuItem extends JCheckBoxMenuItem {

    /**
     * Creates an initially unselected tristate check box menu item with no set text or icon.
     */
    public TristateCheckBoxMenuItem() {
        this(null, null, TristateState.DESELECTED);
    }

    /**
     * Creates an initially unselected tristate check box menu item with an icon.
     *
     * @param icon the icon of the {@code JCheckBoxMenuItem}.
     */
    public TristateCheckBoxMenuItem(final Icon icon) {
        this(null, icon, TristateState.DESELECTED);
    }

    /**
     * Creates an initially unselected tristate check box menu item with text.
     *
     * @param text the text of the {@code JCheckBoxMenuItem}
     */
    public TristateCheckBoxMenuItem(final String text) {
        this(text, null, TristateState.DESELECTED);
    }

    /**
     * Creates a menu item whose properties are taken from the Action supplied.
     *
     * @param a the action of the {@code JCheckBoxMenuItem}
     * @since   1.3
     */
    public TristateCheckBoxMenuItem(final Action a) {
        this();
        setAction(a);
    }

    /**
     * Creates an initially unselected check box menu item with the specified text and icon.
     *
     * @param text the text of the {@code JCheckBoxMenuItem}
     * @param icon the icon of the {@code JCheckBoxMenuItem}
     */
    public TristateCheckBoxMenuItem(final String text, final Icon icon) {
        this(text, icon, TristateState.DESELECTED);
    }

    /**
     * Creates a tristate check box menu item with the specified text and selection state.
     *
     * @param text  the text of the check box menu item.
     * @param state the selected state of the check box menu item.
     */
    public TristateCheckBoxMenuItem(final String text, final TristateState state) {
        this(text, null, state);
    }

    /**
     * Creates a tristate check box menu item with the specified text, icon, and selection state.
     *
     * @param text  the text of the check box menu item.
     * @param icon  the icon of the check box menu item.
     * @param state the selected state of the check box menu item.
     */
    public TristateCheckBoxMenuItem(final String text, final Icon icon, final TristateState state) {
        super(text, icon);
        setModel(new TristateButtonModel(state));
        // override action behaviour
        super.addMouseListener(new MouseAdapter() {
            public void mousePressed(final MouseEvent e) {
                TristateCheckBoxMenuItem.this.iterateState();
            }
        });
        setFocusable(false);
    }

    public String getUIClassID() {
        if (UIManager.getLookAndFeel() instanceof ThemedLookAndFeel) {
            return "TristateCheckBoxMenuItemUI";
        } else {
            return super.getUIClassID();
        }
    }

    private void iterateState() {
        if (!getModel().isEnabled()) return;

        grabFocus();
        getTristateModel().iterateState();
        repaint();

        int modifiers = 0;
        AWTEvent currentEvent = EventQueue.getCurrentEvent();
        if (currentEvent instanceof InputEvent) {
            modifiers = ((InputEvent) currentEvent).getModifiersEx();
        } else if (currentEvent instanceof ActionEvent) {
            modifiers = ((ActionEvent) currentEvent).getModifiers();
        }
        fireActionPerformed(new ActionEvent(this,
                                            ActionEvent.ACTION_PERFORMED, getText(),
                                            System.currentTimeMillis(), modifiers));
    }

    public TristateButtonModel getTristateModel() {
        return (TristateButtonModel) super.getModel();
    }

    @Override
    public void setSelected(final boolean b) {
        if (getModel() instanceof TristateButtonModel) {
            setState(b ? TristateState.SELECTED : TristateState.DESELECTED);
        } else {
            super.setSelected(b);
        }
    }

    public void setIndeterminate() {
        getTristateModel().setIndeterminate();
    }

    public boolean isIndeterminate() {
        return getTristateModel().isIndeterminate();
    }

    @Override
    public boolean getState() {
        return getTristateModel().getState() == TristateState.SELECTED;
    }

    public void setState(final TristateState state) {
        getTristateModel().setState(state);
    }

    public TristateState getTristateState() {
        return getTristateModel().getState();
    }
}
