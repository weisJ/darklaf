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
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.ChangeListener;

import com.github.weisj.darklaf.LafManager;

public class TristateCheckBox extends JCheckBox {
    private final ChangeListener enableListener = e -> TristateCheckBox.this.setFocusable(getModel().isEnabled());

    public TristateCheckBox() {
        this(null);
    }

    public TristateCheckBox(final String text) {
        this(text, null, TristateState.DESELECTED);
    }

    public TristateCheckBox(final String text, final Icon icon, final TristateState initial) {
        super(text, icon);
        setModel(new TristateButtonModel(initial));
        addActionListener(e -> iterateState());
    }

    private void iterateState() {
        if (!getModel().isEnabled()) return;
        getTristateModel().iterateState();
    }

    public TristateButtonModel getTristateModel() {
        return (TristateButtonModel) super.getModel();
    }

    public String getUIClassID() {
        if (LafManager.isInstalled()) {
            return "TristateCheckBoxUI";
        } else {
            return super.getUIClassID();
        }
    }

    public boolean allowsIndeterminate() {
        return getTristateModel().allowsIndeterminate();
    }

    public void setAllowsIndeterminate(final boolean allowsIndeterminate) {
        getTristateModel().setAllowsIndeterminate(allowsIndeterminate);
    }

    @Override
    public void setSelected(final boolean b) {
        setState(b ? TristateState.SELECTED : TristateState.DESELECTED);
    }

    @Override
    public void setModel(final ButtonModel newModel) {
        ButtonModel model = getModel();
        if (model instanceof TristateButtonModel) {
            model.removeChangeListener(enableListener);
        }
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
