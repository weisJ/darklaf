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
package com.github.weisj.darklaf.bridge;

import javax.swing.*;
import java.beans.PropertyChangeListener;

/**
 * This is a mirror of the UIAction implementation in the sun.java package.
 */
public abstract class DarkUIActionBridge implements Action {
    private String name;

    public DarkUIActionBridge(final String name) {
        this.name = name;
    }

    public final String getName() {
        return name;
    }

    @SuppressWarnings("StringEquality")
    public Object getValue(final String key) {
        if (key == NAME) {
            return name;
        }
        return null;
    }

    // UIAction is not mutable, this does nothing.
    public void putValue(final String key, final Object value) {
    }

    /**
     * Cover method for <code>isEnabled(null)</code>.
     */
    public final boolean isEnabled() {
        return accept(null);
    }

    // UIAction is not mutable, this does nothing.
    public void setEnabled(final boolean b) {
    }

    /**
     * Subclasses that need to conditionalize the enabled state should
     * override this. Be aware that <code>sender</code> may be null.
     *
     * @param sender Widget enabled state is being asked for, may be null.
     */
    @Override
    public boolean accept(final Object sender) {
        return true;
    }

    // UIAction is not mutable, this does nothing.
    public void addPropertyChangeListener(final PropertyChangeListener listener) {
    }

    // UIAction is not mutable, this does nothing.
    public void removePropertyChangeListener(final PropertyChangeListener listener) {
    }
}
