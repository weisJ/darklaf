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
 */
package com.github.weisj.darklaf.ui;

import java.beans.PropertyChangeListener;

import javax.swing.Action;

public abstract class UIAction implements Action {
    private final String name;

    public UIAction(String name) {
        this.name = name;
    }

    public final String getName() {
        return name;
    }

    @Override
    public Object getValue(String key) {
        if (NAME.equals(key)) {
            return name;
        }
        return null;
    }

    @Override
    public final boolean isEnabled() {
        return true;
    }

    // UIAction is not mutable, this does nothing.
    @Override
    public void putValue(String key, Object value) {}

    // UIAction is not mutable, this does nothing.
    @Override
    public void setEnabled(boolean b) {}

    // UIAction is not mutable, this does nothing.
    @Override
    public void addPropertyChangeListener(PropertyChangeListener listener) {}

    // UIAction is not mutable, this does nothing.
    @Override
    public void removePropertyChangeListener(PropertyChangeListener listener) {}
}
