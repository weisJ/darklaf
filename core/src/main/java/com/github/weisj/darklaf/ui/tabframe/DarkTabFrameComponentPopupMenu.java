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
package com.github.weisj.darklaf.ui.tabframe;

import com.github.weisj.darklaf.components.JXPopupMenu;
import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.PropertyKey;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkTabFrameComponentPopupMenu extends JXPopupMenu implements PropertyChangeListener, UIResource {

    private final TabFrameTab tab;
    private final JMenuItem[] actions;
    private int disabled = -1;

    public DarkTabFrameComponentPopupMenu(final TabFrameTab tab) {
        this.tab = tab;
        JMenu moveToMenu = new JMenu("Move to");
        Alignment[] aligns = Alignment.values();
        actions = new JMenuItem[aligns.length];

        Alignment a = Alignment.NORTH_WEST;
        while (a != Alignment.NORTH) {
            createAndAdd(a, moveToMenu);
            a = a.anticlockwise();
        }
        createAndAdd(a, moveToMenu);

        add(moveToMenu);
        if (tab.getOrientation() != null) {
            disabled = tab.getOrientation().ordinal();
            actions[tab.getOrientation().ordinal()].setEnabled(false);
        }
        tab.getComponent().addPropertyChangeListener(this);
    }

    protected void createAndAdd(final Alignment a, final JMenu menu) {
        JMenuItem menuItem = createMenuItem(a);
        actions[a.ordinal()] = menuItem;
        menu.add(menuItem);
    }

    protected JMenuItem createMenuItem(final Alignment a) {
        JMenuItem menuItem = new JMenuItem();
        menuItem.addActionListener(e -> moveTo(a));
        menuItem.setText(getDescription(a));
        menuItem.setIcon(createIcon(a, true));
        menuItem.setDisabledIcon(createIcon(a, false));
        return menuItem;
    }

    protected void moveTo(final Alignment a) {
        if (disabled >= 0) {
            actions[disabled].setEnabled(true);
        }
        disabled = a.ordinal();
        actions[a.ordinal()].setEnabled(false);
        tab.getTabFrame().moveTab(tab, a);
    }

    protected String getDescription(final Alignment a) {
        switch (a) {
            case NORTH:
                return "Top Left";
            case SOUTH:
                return "Bottom Right";
            case EAST:
                return "Right Top";
            case WEST:
                return "Left Bottom";
            case NORTH_EAST:
                return "Top Right";
            case NORTH_WEST:
                return "Left Top";
            case SOUTH_EAST:
                return "Right Bottom";
            case SOUTH_WEST:
                return "Bottom Left";
            case CENTER:
            default:
                return "";
        }
    }

    protected Icon createIcon(final Alignment a, final boolean enabled) {
        String suffix = enabled ? "" : "Disabled";
        switch (a) {
            case NORTH:
                return UIManager.getIcon("TabFrame.moveToTopLeft" + suffix + ".icon");
            case SOUTH:
                return UIManager.getIcon("TabFrame.moveToBottomRight" + suffix + ".icon");
            case EAST:
                return UIManager.getIcon("TabFrame.moveToRightTop" + suffix + ".icon");
            case WEST:
                return UIManager.getIcon("TabFrame.moveToLeftBottom" + suffix + ".icon");
            case NORTH_EAST:
                return UIManager.getIcon("TabFrame.moveToTopRight" + suffix + ".icon");
            case NORTH_WEST:
                return UIManager.getIcon("TabFrame.moveToLeftTop" + suffix + ".icon");
            case SOUTH_EAST:
                return UIManager.getIcon("TabFrame.moveToRightBottom" + suffix + ".icon");
            case SOUTH_WEST:
                return UIManager.getIcon("TabFrame.moveToBottomLeft" + suffix + ".icon");
            case CENTER:
            default:
                return EmptyIcon.create(0);
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        if (TabFrameTab.KEY_ORIENTATION.equals(evt.getPropertyName())) {
            Object a = evt.getNewValue();
            if (a instanceof Alignment) {
                if (disabled >= 0) {
                    actions[disabled].setEnabled(true);
                }
                disabled = ((Alignment) a).ordinal();
                actions[disabled].setEnabled(false);
            }
        } else if (PropertyKey.COMPONENT_POPUP_MENU.equals(evt.getPropertyName())) {
            if (evt.getNewValue() != this) {
                tab.getComponent().removePropertyChangeListener(this);
            }
        }
    }
}
