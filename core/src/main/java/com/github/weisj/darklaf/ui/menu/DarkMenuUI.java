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
package com.github.weisj.darklaf.ui.menu;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.*;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuUI;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.listener.MouseInputDelegate;

public class DarkMenuUI extends BasicMenuUI {

    protected Icon arrowIconHover;
    protected JMenu menu;
    protected MouseListener mouseListener;

    public static ComponentUI createUI(final JComponent x) {
        return new DarkMenuUI();
    }

    @Override
    public void installUI(final JComponent c) {
        menu = (JMenu) c;
        super.installUI(c);
    }

    @Override
    protected MouseInputListener createMouseInputListener(final JComponent c) {
        return new MouseInputDelegate(super.createMouseInputListener(c)) {
            private boolean pressed = false;
            private boolean wasEnabled = false;

            @Override
            public void mouseEntered(final MouseEvent e) {
                MenuSelectionManager manager = MenuSelectionManager.defaultManager();
                MenuElement[] selectedPath = manager.getSelectedPath();
                for (MenuElement element : selectedPath) {
                    if (element.equals(menu)) {
                        return;
                    }
                }
                super.mouseEntered(e);
            }

            @Override
            public void mousePressed(final MouseEvent e) {
                pressed = true;
                wasEnabled = menu.isEnabled() && menu.isSelected() && menu.getPopupMenu().isShowing();
                super.mousePressed(e);
            }

            @Override
            public void mouseReleased(final MouseEvent e) {
                if (!menu.isEnabled()) return;
                if (pressed && wasEnabled) {
                    pressed = false;
                    return;
                }
                super.mouseReleased(e);
            }
        };
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        menu.removeMouseListener(mouseListener);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        acceleratorFont = UIManager.getFont("Menu.font");
        acceleratorForeground = UIManager.getColor("Menu.foreground");
        acceleratorSelectionForeground = UIManager.getColor("Menu.selectionForeground");
        arrowIconHover = UIManager.getIcon("Menu.arrowHover.icon");
    }

    public void paint(final Graphics g, final JComponent c) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        paintMenuItem(g, c, checkIcon, getArrowIcon(),
                      selectionBackground, isSelected(c) ? selectionForeground : c.getForeground(),
                      defaultTextIconGap);
        config.restore();
    }

    protected Icon getArrowIcon() {
        boolean hover = menuItem.getModel().isArmed()
                        || (menuItem instanceof JMenu && menuItem.getModel().isSelected());
        return hover ? arrowIconHover : arrowIcon;
    }

    protected boolean isSelected(final JComponent menuItem) {
        if (!(menuItem instanceof JMenuItem)) return false;
        return menuItem.isEnabled() && ((JMenuItem) menuItem).isArmed();
    }
}
