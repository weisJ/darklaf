/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.ui.popupmenu;

import java.awt.*;
import java.awt.event.ContainerAdapter;
import java.awt.event.ContainerEvent;

import javax.swing.*;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.listener.PopupMenuAdapter;
import com.github.weisj.darklaf.ui.scrollpane.DarkScrollBarUI;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.ui.util.OneTimeExecutionLock;

public class PopupMenuContainer extends JPanel {

    private JScrollPane scrollPane;
    private ViewPanel view;

    private MenuKeyListener menuKeyListener;
    private PopupMenuListener menuListener;
    private JPopupMenu popupMenu;
    private boolean hasPopupMenuBorder;

    public PopupMenuContainer() {
        super(new BorderLayout());
    }

    private void initComponents() {
        if (view == null) {
            view = new ViewPanel();
            view.addContainerListener(new ContainerAdapter() {
                @Override
                public void componentRemoved(ContainerEvent e) {
                    if (e.getChild() == popupMenu) {
                        restoreBorderIfNeeded();
                    }
                }
            });
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

    private void restoreBorderIfNeeded() {
        if (hasPopupMenuBorder && popupMenu != null) {
            popupMenu.setBorderPainted(true);
            popupMenu.setBorder(getBorder());
            hasPopupMenuBorder = false;
        }
    }

    public Popup createPopup(final JPopupMenu popupMenu, final Dimension prefSize,
            final int posX, final int posY, final int maxWidth, final int maxHeight) {
        uninstallListeners();
        Dimension adjustedSize = adjustSize(prefSize, maxWidth, maxHeight);

        restoreBorderIfNeeded();

        if (adjustedSize.width == prefSize.width && adjustedSize.height == prefSize.height) {
            setBounds(0, 0, prefSize.width, prefSize.height);
            return PopupFactory.getSharedInstance().getPopup(popupMenu.getInvoker(), popupMenu, posX, posY);
        } else {
            setPopupMenu(popupMenu);
            int increment = 1;
            if (popupMenu.getComponentCount() > 0) {
                increment = Math.max(1, popupMenu.getComponent(0).getPreferredSize().height / 2);
            }
            JScrollBar verticalBar = scrollPane.getVerticalScrollBar();
            verticalBar.setValue(verticalBar.getMinimum());
            verticalBar.setUnitIncrement(increment);

            JScrollBar horizontalBar = scrollPane.getHorizontalScrollBar();
            horizontalBar.setValue(horizontalBar.getMinimum());
            horizontalBar.setUnitIncrement(increment);

            view.setContent(popupMenu);

            setBorder(popupMenu.getBorder());
            popupMenu.setBorderPainted(false);
            popupMenu.setBorder(null);
            hasPopupMenuBorder = true;

            setPreferredSize(adjustedSize);
            return PopupFactory.getSharedInstance().getPopup(popupMenu.getInvoker(), this, posX, posY);
        }
    }

    public Dimension adjustSize(final Dimension size, final int maxWidth, final int maxHeight) {
        boolean exceedsHorizontalSize = maxHeight > 0 && size.height > maxHeight;
        boolean exceedsVerticalSize = maxWidth > 0 && size.width > maxWidth;
        if (!exceedsHorizontalSize && !exceedsVerticalSize) {
            return size;
        } else {
            initComponents();
            JScrollBar verticalBar = scrollPane.getVerticalScrollBar();
            JScrollBar horizontalBar = scrollPane.getHorizontalScrollBar();
            Dimension constraintSize = new Dimension(size);
            if (exceedsHorizontalSize) constraintSize.width += verticalBar.getPreferredSize().width;
            if (exceedsVerticalSize) constraintSize.height += horizontalBar.getPreferredSize().height;
            if (maxWidth > 0) constraintSize.width = Math.min(constraintSize.width, maxWidth);
            if (maxHeight > 0) constraintSize.height = Math.min(constraintSize.height, maxHeight);
            return constraintSize;
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

    private static class ViewPanel extends JPanel {

        private JPopupMenu content;
        private OneTimeExecutionLock sizeLock;

        public void setContent(JPopupMenu content) {
            this.content = content;
            DarkPopupMenuUI ui = DarkUIUtil.getUIOfType(content.getUI(), DarkPopupMenuUI.class);
            this.sizeLock = ui != null ? ui.sizeLock : new OneTimeExecutionLock();
            removeAll();
            add(content);
        }

        @Override
        public Dimension getMinimumSize() {
            if (content != null) {
                try (OneTimeExecutionLock l = sizeLock.lock()) {
                    return content.getMinimumSize();
                }
            }
            return super.getMinimumSize();
        }

        @Override
        public Dimension getPreferredSize() {
            if (content != null) {
                try (OneTimeExecutionLock l = sizeLock.lock()) {
                    return content.getPreferredSize();
                }
            }
            return super.getPreferredSize();
        }

        @Override
        public Dimension getMaximumSize() {
            if (content != null) {
                try (OneTimeExecutionLock l = sizeLock.lock()) {
                    return content.getMaximumSize();
                }
            }
            return super.getMaximumSize();
        }

        @Override
        public void doLayout() {
            if (content != null) {
                content.setBounds(0, 0, getWidth(), getHeight());
            }
        }
    }
}
