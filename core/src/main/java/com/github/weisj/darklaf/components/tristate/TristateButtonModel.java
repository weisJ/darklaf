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
package com.github.weisj.darklaf.components.tristate;

import java.awt.event.ItemEvent;

import javax.swing.*;

public class TristateButtonModel extends JToggleButton.ToggleButtonModel {
    private TristateState state = TristateState.DESELECTED;
    private boolean allowsIndeterminate = true;

    public TristateButtonModel() {
        this(TristateState.DESELECTED);
    }

    public TristateButtonModel(final TristateState state) {
        setState(state);
    }

    protected void displayState() {
        super.setSelected(!isDeselected());
    }

    public void setIndeterminate() {
        if (!allowsIndeterminate || isIndeterminate())
            return;
        setState(isSelected() ? TristateState.INDETERMINATE_DES : TristateState.INDETERMINATE_SEL);
    }

    public boolean isIndeterminate() {
        return state == TristateState.INDETERMINATE_DES || state == TristateState.INDETERMINATE_SEL;
    }

    @Override
    public boolean isSelected() {
        return state == TristateState.SELECTED;
    }

    public boolean isDeselected() {
        return state == TristateState.DESELECTED;
    }

    public void iterateState() {
        if (allowsIndeterminate) {
            setState(state.next());
        } else {
            if (state == TristateState.SELECTED) {
                setState(TristateState.DESELECTED);
            } else if (state == TristateState.DESELECTED) {
                setState(TristateState.SELECTED);
            } else {
                setState(state.next());
            }
        }
    }

    public TristateState getState() {
        return state;
    }

    public void setState(final TristateState state) {
        if (this.state == state)
            return;
        if (!allowsIndeterminate && state.isIndeterminate()) {
            return;
        }
        this.state = state;
        displayState();
        fireStateChanged();
        int indeterminate = 3;
        // noinspection MagicConstant
        fireItemStateChanged(new ItemEvent(this, ItemEvent.ITEM_STATE_CHANGED, this, indeterminate));
    }

    public boolean allowsIndeterminate() {
        return allowsIndeterminate;
    }

    public void setAllowsIndeterminate(final boolean allowsIndeterminate) {
        this.allowsIndeterminate = allowsIndeterminate;
        if (!allowsIndeterminate && state.isIndeterminate()) {
            setState(state.next());
        }
    }
}
