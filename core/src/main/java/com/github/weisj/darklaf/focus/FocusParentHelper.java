/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
package com.github.weisj.darklaf.focus;

import java.awt.*;
import java.awt.event.FocusEvent;
import java.util.Map;
import java.util.WeakHashMap;
import java.util.function.Consumer;

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyUtil;

public final class FocusParentHelper {

    public static final String KEY_FOCUS_PARENT = "focusParent";
    public static final String KEY_FOCUS_ACTION = "focusOnParentChangedAction";
    private static final Map<Component, Component> listeners = new WeakHashMap<>();

    static {
        Toolkit.getDefaultToolkit().addAWTEventListener(event -> {
            if (!(event instanceof FocusEvent e)) return;
            if (e.getID() != FocusEvent.FOCUS_GAINED && e.getID() != FocusEvent.FOCUS_LOST) return;
            Component comp = e.getComponent();
            listeners.forEach((c, focusParent) -> {
                if (SwingUtilities.isDescendingFrom(comp, focusParent)) {
                    OnFocusChangedAction onFocusChangedAction =
                            PropertyUtil.getObject(c, KEY_FOCUS_ACTION, OnFocusChangedAction.class);
                    if (onFocusChangedAction != null) {
                        onFocusChangedAction.accept(c);
                    } else {
                        c.repaint();
                    }
                }
            });
        }, AWTEvent.FOCUS_EVENT_MASK);
    }

    public static void updateFocusParentRegistry(final JComponent c, final Component parent) {
        if (parent == null) {
            c.putClientProperty(KEY_FOCUS_ACTION, null);
            listeners.remove(c);
        } else {
            listeners.put(c, parent);
        }
    }

    public static void setFocusParent(final JComponent c, final JComponent focusParent) {
        setFocusParent(c, focusParent, Component::repaint);
    }

    public static void setFocusParent(final JComponent c, final JComponent focusParent,
            final OnFocusChangedAction focusChangedAction) {
        if (c == null) return;
        c.putClientProperty(KEY_FOCUS_PARENT, focusParent);
        updateFocusParentRegistry(c, focusParent);
        if (focusParent != null) {
            c.putClientProperty(KEY_FOCUS_ACTION, focusChangedAction);
        }
    }

    public interface OnFocusChangedAction extends Consumer<Component> {
    }
}
