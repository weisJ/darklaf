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
package com.github.weisj.darklaf.components.tristate;

import com.github.weisj.darklaf.DarkLaf;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public class TristateCheckBox extends JCheckBox {
    private final ChangeListener enableListener = e -> TristateCheckBox.this.setFocusable(getModel().isEnabled());

    public TristateCheckBox(final String text) {
        this(text, null, TristateState.DESELECTED);
    }

    public TristateCheckBox(final String text, final Icon icon, final TristateState initial) {
        super(text, icon);
        setModel(new TristateButtonModel(initial));
        // override action behaviour
        super.addMouseListener(new MouseAdapter() {
            public void mousePressed(final MouseEvent e) {
                TristateCheckBox.this.iterateState();
            }
        });
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


    public String getUIClassID() {
        if (UIManager.getLookAndFeel() instanceof DarkLaf) {
            return "TristateCheckBoxUI";
        } else {
            return super.getUIClassID();
        }
    }

    @Override
    public void setSelected(final boolean b) {
        setState(b ? TristateState.SELECTED : TristateState.DESELECTED);
    }

    @Override
    public void setModel(final ButtonModel newModel) {
        super.setModel(newModel);

        if (model instanceof TristateButtonModel) {
            model.addChangeListener(enableListener);
        }
    }

    public void setIndeterminate() {
        getTristateModel().setIndeterminate();
    }

    public boolean isIndeterminate() {
        return getTristateModel().isIndeterminate();
    }

    public TristateState getState() {
        return getTristateModel().getState();
    }

    public void setState(final TristateState state) {
        getTristateModel().setState(state);
    }
}
