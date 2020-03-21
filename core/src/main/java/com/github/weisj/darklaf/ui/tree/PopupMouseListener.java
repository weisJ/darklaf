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
package com.github.weisj.darklaf.ui.tree;

import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseMotionAdapter;
import java.util.Objects;

public class PopupMouseListener extends MouseMotionAdapter {

    private final DarkTreeUI treeUI;
    private final JTree tree;
    private final PopupContent content;
    private final Rectangle boundsBuffer = new Rectangle();
    private Popup popup;

    public PopupMouseListener(final DarkTreeUI treeUI, final JTree tree) {
        this.treeUI = treeUI;
        this.tree = tree;
        content = new PopupContent();
        content.setBackground(Color.RED);
    }

    @Override
    public void mouseMoved(final MouseEvent e) {
        JViewport viewport = DarkUIUtil.getParentOfType(JViewport.class, tree);
        if (viewport == null) return;
        TreePath path = treeUI.getClosestPathForLocation(tree, e.getX(), e.getY());
        Rectangle cellBounds = treeUI.getPathBounds(path, tree.getInsets(), boundsBuffer);
        int row = treeUI.getRowForPath(tree, path);
        if (cellBounds.contains(e.getPoint())) {
            if (!viewport.getBounds().contains(cellBounds)) {
                setupContentPane(cellBounds, row, path);
                showPopup(viewport, cellBounds);
            } else {
                hidePopup();
            }
        } else {
            hidePopup();
        }
    }

    protected void setupContentPane(final Rectangle cellBounds, final int row, final TreePath path) {
        content.setPreferredSize(cellBounds.getSize());
        content.setRendererPath(row, path);
    }

    protected void showPopup(final JComponent parent, final Rectangle cellBounds) {
        if (popup == null) {
            Point p = cellBounds.getLocation();
            Point screenPos = parent.getLocationOnScreen();
            popup = PopupFactory.getSharedInstance().getPopup(parent, content,
                                                              screenPos.x + p.x, screenPos.y + p.y);
            popup.show();
        }
        content.paintImmediately(0, 0, cellBounds.width, cellBounds.height);
    }

    protected void hidePopup() {
        if (popup != null) {
            popup.hide();
            popup = null;
        }
    }

    protected class PopupContent extends JPanel {

        protected JRootPane lastRootPane;
        private int row;
        private TreePath path;
        private final Insets insets = new Insets(0, 0, 0, 0);

        protected PopupContent() {
            setOpaque(true);
            setFocusable(false);
            addHierarchyListener(e -> {
                Window w = SwingUtilities.getWindowAncestor(PopupContent.this);
                if (lastRootPane != null) {
                    lastRootPane.putClientProperty(DarkRootPaneUI.KEY_IS_TOOLTIP, false);
                    lastRootPane.putClientProperty("Window.shadow", true);
                }
                if (w instanceof RootPaneContainer
                    && Objects.equals(w.getClass().getEnclosingClass(), Popup.class)) {
                    lastRootPane = ((RootPaneContainer) w).getRootPane();
                    if (lastRootPane != null) {
                        lastRootPane.putClientProperty(DarkRootPaneUI.KEY_IS_TOOLTIP, true);
                        lastRootPane.putClientProperty("Window.shadow", false);
                    }
                }
            });
            addMouseListener(new MouseAdapter() {
                @Override
                public void mouseExited(final MouseEvent e) {
                    hidePopup();
                }

                @Override
                public void mousePressed(final MouseEvent e) {
                    tree.dispatchEvent(SwingUtilities.convertMouseEvent(e.getComponent(), e, tree));
                    SwingUtilities.invokeLater(() -> repaint());
                }

                @Override
                public void mouseReleased(final MouseEvent e) {
                    tree.dispatchEvent(SwingUtilities.convertMouseEvent(e.getComponent(), e, tree));
                    SwingUtilities.invokeLater(() -> repaint());
                }

                @Override
                public void mouseClicked(final MouseEvent e) {
                    tree.dispatchEvent(SwingUtilities.convertMouseEvent(e.getComponent(), e, tree));
                    SwingUtilities.invokeLater(() -> repaint());
                }
            });
        }

        @Override
        public Color getBackground() {
            return treeUI.getRowBackground(row, tree.isRowSelected(row));
        }

        @Override
        protected void paintComponent(final Graphics graphics) {
            if (tree.isEditing() && tree.getEditingPath().equals(path)) {
                hidePopup();
                return;
            }
            Graphics2D g = (Graphics2D) graphics;
            Rectangle bounds = getBounds();
            bounds.setLocation(0, 0);
            g.setColor(getBackground());
            g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
            treeUI.paintRow(g, g.getClipBounds(), insets, bounds, path, row, tree.isExpanded(row), false,
                            treeUI.isLeaf(path.getLastPathComponent()));
        }

        public void setRendererPath(final int row, final TreePath path) {
            this.row = row;
            this.path = path;
        }
    }
}
