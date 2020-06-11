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
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.awt.event.ActionEvent;

/**
 * @author Jannis Weis
 */
public class TabEvent extends ActionEvent {

    public static final int TAB_OPENED = 0;
    public static final int TAB_CLOSED = 1;
    public static final int TAB_CLOSING = 2;
    private final Type type;
    private final int tabIndex;
    private final Component component;

    public TabEvent(final Object source, final Type type, final String command, final int tabIndex, final Component c) {
        super(source, type.ordinal(), command);
        this.type = type;
        this.tabIndex = tabIndex;
        this.component = c;
    }

    public Component getComponent() {
        return component;
    }

    public int getTabIndex() {
        return tabIndex;
    }

    public Type getType() {
        return type;
    }

    @Override
    public String toString() {
        return "TabEvent{" +
               "type=" + type +
               ", tabIndex=" + tabIndex +
               ", component=" + component +
               '}';
    }

    public enum Type {
        TAB_OPENED,
        TAB_CLOSED,
        TAB_CLOSING
    }
}
