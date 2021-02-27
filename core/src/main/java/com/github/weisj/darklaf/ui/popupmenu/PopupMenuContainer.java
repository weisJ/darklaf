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
package com.github.weisj.darklaf.ui.popupmenu;

import java.awt.*;

import javax.swing.*;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.listener.PopupMenuAdapter;
import com.github.weisj.darklaf.ui.scrollpane.DarkScrollBarUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class PopupMenuContainer extends JPanel {

    private JScrollPane scrollPane;
    private JPanel view;

    private MenuKeyListener menuKeyListener;
    private PopupMenuListener menuListener;
    private JPopupMenu popupMenu;

    public PopupMenuContainer() {
        super(new BorderLayout());
    }

    private void initComponents() {
        if (view == null) {
            view = new JPanel(new BorderLayout());
            OverlayScrollPane overlayScrollPane = createScrollPane(view);
            scrollPane = overlayScrollPane.getScrollPane();
            add(overlayScrollPane, BorderLayout.CENTER);
        }
    }

    protected MenuKeyListener getMenuKeyListener() {
        if (menuKeyListener == null) {
            menuKeyListener = new MenuKeyListener() {
                @Override
                public void menuKeyTyped(final MenuKeyEvent e) {}

                @Override
                public void menuKeyPressed(final MenuKeyEvent e) {
                    SwingUtilities.invokeLater(() -> {
                        if (popupMenu == null) return;
                        MenuElement[] path = e.getMenuSelectionManager().getSelectedPath();
                        if (path.length == 0) {
                            return;
                        }
                        Rectangle bounds = path[path.length - 1].getComponent().getBounds();
                        Rectangle r = SwingUtilities.convertRectangle(popupMenu, bounds, scrollPane);
                        scrollPane.getViewport().scrollRectToVisible(r);
                    });
                }

                @Override
                public void menuKeyReleased(final MenuKeyEvent e) {}
            };
        }
        return menuKeyListener;
    }

    protected PopupMenuListener getMenuListener() {
        if (menuListener == null) {
            menuListener = new PopupMenuAdapter() {
                @Override
                public void popupMenuWillBecomeInvisible(final PopupMenuEvent e) {
                    onHide();
                }

                @Override
                public void popupMenuCanceled(final PopupMenuEvent e) {
                    onHide();
                }

                private void onHide() {
                    if (popupMenu == null) return;
                    popupMenu.removePopupMenuListener(this);
                    popupMenu.removeMenuKeyListener(menuKeyListener);
                }
            };
        }
        return menuListener;
    }

    protected void uninstallListeners() {
        if (this.popupMenu != null) {
            this.popupMenu.removeMenuKeyListener(menuKeyListener);
            this.popupMenu.removePopupMenuListener(menuListener);
        }
    }

    public void setPopupMenu(final JPopupMenu popupMenu) {
        MenuKeyListener keyListener = getMenuKeyListener();
        PopupMenuListener popupMenuListener = getMenuListener();
        this.popupMenu = popupMenu;
        if (popupMenu != null) {
            popupMenu.removeMenuKeyListener(keyListener);
            popupMenu.removePopupMenuListener(popupMenuListener);
            popupMenu.addMenuKeyListener(keyListener);
            popupMenu.addPopupMenuListener(popupMenuListener);
        }
    }

    public Popup createPopup(final JPopupMenu popupMenu, final int posX, final int posY, final int maxHeight) {
        final Dimension prefSize = popupMenu.getPreferredSize();
        uninstallListeners();
        if (maxHeight <= 0 || prefSize.height <= maxHeight) {
            setBounds(0, 0, prefSize.width, prefSize.height);
            popupMenu.setBorderPainted(true);
            return PopupFactory.getSharedInstance().getPopup(popupMenu.getInvoker(), popupMenu, posX, posY);
        } else {
            initComponents();
            setPopupMenu(popupMenu);
            int increment = 1;
            if (popupMenu.getComponentCount() > 0) {
                increment = Math.max(1, popupMenu.getComponent(0).getPreferredSize().height / 2);
            }
            JScrollBar bar = scrollPane.getVerticalScrollBar();
            bar.setValue(bar.getMinimum());
            bar.setUnitIncrement(increment);
            view.removeAll();
            view.add(popupMenu, BorderLayout.CENTER);
            setBorder(popupMenu.getBorder());
            popupMenu.setBorderPainted(false);
            setPreferredSize(new Dimension(prefSize.width + bar.getPreferredSize().width, maxHeight));
            return PopupFactory.getSharedInstance().getPopup(popupMenu.getInvoker(), this, posX, posY);
        }
    }

    private OverlayScrollPane createScrollPane(final JComponent content) {
        OverlayScrollPane overlayScrollPane = new OverlayScrollPane(content, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        JScrollBar bar = overlayScrollPane.getVerticalScrollBar();
        bar.putClientProperty(DarkScrollBarUI.KEY_SMALL, Boolean.TRUE);
        DarkUIUtil.doNotCancelPopupSetup(bar);
        DarkUIUtil.doNotCancelPopupSetup(overlayScrollPane.getScrollPane());
        return overlayScrollPane;
    }

    public JScrollPane getScrollPane() {
        return scrollPane;
    }
}
