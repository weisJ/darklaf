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
 *
 */
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.awt.event.ActionEvent;

/** @author Jannis Weis */
public class TabEvent extends ActionEvent {

    private final Type type;
    private final int tabIndex;
    private final Component component;
    private final ClosableTabbedPane closableTabbedPane;

    public TabEvent(final ClosableTabbedPane closableTabbedPane, final Type type, final int tabIndex,
            final Component c) {
        super(closableTabbedPane, type.ordinal(), type.getCommand());
        this.type = type;
        this.tabIndex = tabIndex;
        this.component = c;
        this.closableTabbedPane = closableTabbedPane;
    }

    public Component getComponent() {
        return component;
    }

    public ClosableTabbedPane getClosableTabbedPane() {
        return closableTabbedPane;
    }

    public int getTabIndex() {
        return tabIndex;
    }

    public Type getType() {
        return type;
    }

    @Override
    public String toString() {
        return "TabEvent{" + "type=" + type + ", tabIndex=" + tabIndex + ", component=" + component + '}';
    }

    public enum Type {
        TAB_OPENED("tabOpened"),
        TAB_CLOSED("tabClosed"),
        TAB_CLOSING("tabClosing");

        private final String command;

        Type(final String command) {
            this.command = command;
        }

        public String getCommand() {
            return command;
        }
    }
}
