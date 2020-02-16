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
package com.github.weisj.darklaf.components;

import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class ScrollPopupMenu extends JPopupMenu {

    private final JPanel contentPane;
    private final JScrollPane scrollPane;
    private int maxHeight;
    private JWindow popWin;
    private int posX;
    private int posY;
    private JPanel view;

    public ScrollPopupMenu(final int maxHeight) {
        this.maxHeight = maxHeight;
        contentPane = new JPanel(new BorderLayout());
        OverlayScrollPane overlayScrollPane = createScrollPane();
        scrollPane = overlayScrollPane.getScrollPane();
        contentPane.add(overlayScrollPane, BorderLayout.CENTER);
        contentPane.setBorder(getBorder());
        setDoubleBuffered(true);
        MenuKeyListener menuKeyListener = new MenuKeyListener() {
            @Override
            public void menuKeyTyped(final MenuKeyEvent e) {
            }

            @Override
            public void menuKeyPressed(final MenuKeyEvent e) {
                SwingUtilities.invokeLater(() -> {
                    MenuElement[] path = e.getMenuSelectionManager().getSelectedPath();
                    if (path.length == 0) {
                        return;
                    }
                    Rectangle bounds = path[path.length - 1].getComponent().getBounds();
                    Rectangle r = SwingUtilities.convertRectangle(ScrollPopupMenu.this, bounds, scrollPane);
                    scrollPane.getViewport().scrollRectToVisible(r);
                });
            }

            @Override
            public void menuKeyReleased(final MenuKeyEvent e) {

            }
        };
        addMenuKeyListener(menuKeyListener);
    }


    private OverlayScrollPane createScrollPane() {
        view = new JPanel(new BorderLayout());
        view.add(this, BorderLayout.CENTER);
        OverlayScrollPane overlayScrollPane =
                new OverlayScrollPane(view, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                      JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        JScrollBar bar = overlayScrollPane.getVerticalScrollBar();
        bar.putClientProperty("ScrollBar.thin", Boolean.TRUE);
        DarkUIUtil.doNotCancelPopupSetup(bar);
        DarkUIUtil.doNotCancelPopupSetup(overlayScrollPane.getScrollPane());
        return overlayScrollPane;
    }

    /**
     * Set the maximum height of the popup. If the size is larger than the specified maximum height the content will be
     * wrapped inside a scroll pane.
     * <p>
     * Note: A value of less than or equal to 0 indicates that the height should not be limited.
     *
     * @param maxHeight the max height to use.
     */
    public void setMaxHeight(final int maxHeight) {
        this.maxHeight = maxHeight;
    }

    @Override
    public JMenuItem add(final JMenuItem menuItem) {
        menuItem.getModel().addChangeListener(e -> contentPane.repaint(menuItem.getBounds()));
        return super.add(menuItem);
    }

    protected void showPopup() {
        Component comp = getInvoker();
        if (comp == null) return;

        while (comp.getParent() != null) {
            comp = comp.getParent();
        }

        if (popWin == null || popWin.getOwner() != comp) {
            popWin = comp instanceof Window ? new JWindow((Window) comp) : new JWindow(new JFrame());
        }
        pack();
        popWin.setLocation(posX, posY);
        popWin.setVisible(true);
        requestFocus();
    }

    /**
     * Get the scroll pane of the popup.
     *
     * @return scroll pane;
     */

    public JScrollPane getScrollPane() {
        return scrollPane;
    }

    @Override
    public boolean isVisible() {
        return popWin != null && popWin.isShowing();
    }

    @Override
    public void setLocation(final int x, final int y) {
        if (popWin != null && popWin.isShowing()) {
            popWin.setLocation(x, y);
        } else {
            posX = x;
            posY = y;
        }
    }


    @Override
    public void setVisible(final boolean b) {
        if (b == isVisible()) {
            return;
        }
        if (b) {
            if (isPopupMenu()) {
                MenuElement[] menuElements = new MenuElement[1];
                if (getSubElements().length > 0) {
                    menuElements = new MenuElement[2];
                    menuElements[1] = getSubElements()[0];
                }
                menuElements[0] = this;
                MenuSelectionManager.defaultManager().setSelectedPath(menuElements);
            }
            firePopupMenuWillBecomeVisible();
            showPopup();
            firePropertyChange("visible", Boolean.FALSE, Boolean.TRUE);
        } else {
            hidePopup();
        }
    }

    protected void hidePopup() {
        if (popWin != null) {
            firePopupMenuWillBecomeInvisible();
            popWin.setVisible(false);
            popWin = null;
            firePropertyChange("visible", Boolean.TRUE, Boolean.FALSE);
            if (isPopupMenu()) {
                MenuSelectionManager.defaultManager().clearSelectedPath();
            }
        }
    }

    @Override
    public void pack() {
        if (popWin == null) {
            return;
        }
        final Dimension prefSize = getPreferredSize();
        if (maxHeight <= 0 || prefSize.height <= maxHeight) {
            setBounds(0, 0, prefSize.width, prefSize.height);
            popWin.setContentPane(this);
            setBorderPainted(true);
            popWin.setSize(prefSize.width, prefSize.height);
        } else {
            int increment = getComponentCount() > 0
                            ? Math.max(1, getComponent(0).getPreferredSize().height / 2)
                            : 1;
            JScrollBar bar = scrollPane.getVerticalScrollBar();
            bar.setValue(bar.getMinimum());
            bar.setUnitIncrement(increment);
            setBorderPainted(false);
            view.add(this);
            popWin.setContentPane(contentPane);
            popWin.pack();
            popWin.setSize(prefSize.width + bar.getPreferredSize().width, maxHeight);
        }
    }

    private boolean isPopupMenu() {
        Component invoker = getInvoker();
        return ((invoker != null) && !(invoker instanceof JMenu));
    }
}
