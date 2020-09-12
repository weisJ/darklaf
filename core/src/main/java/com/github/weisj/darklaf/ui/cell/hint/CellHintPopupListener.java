/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.cell.hint;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.util.Objects;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.MouseInputAdapter;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.WindowUtil;

public class CellHintPopupListener<T extends JComponent, I> extends MouseInputAdapter {

    private final IndexedCellContainer<T, I> cellContainer;
    private final PopupComponent popupComponent;
    private I lastIndex;
    private Popup popup;

    private boolean respectCellHeight = false;
    private boolean respectCellWidth = true;

    public CellHintPopupListener(final IndexedCellContainer<T, I> cellContainer) {
        this.cellContainer = cellContainer;
        this.popupComponent = new PopupComponent(this);
    }

    public void setRespectCellHeight(final boolean respectCellHeight) {
        this.respectCellHeight = respectCellHeight;
    }

    public void setRespectCellWidth(final boolean respectCellWidth) {
        this.respectCellWidth = respectCellWidth;
    }

    public boolean isRespectCellHeight() {
        return respectCellHeight;
    }

    public boolean isRespectCellWidth() {
        return respectCellWidth;
    }

    public void install() {
        JComponent comp = cellContainer.getComponent();
        comp.addMouseListener(this);
        comp.addMouseMotionListener(this);
    }

    public void uninstall() {
        JComponent comp = cellContainer.getComponent();
        comp.removeMouseListener(this);
        comp.removeMouseMotionListener(this);
    }

    @Override
    public void mouseMoved(final MouseEvent e) {
        final Point p = e.getPoint();
        final I index = cellContainer.getCellPosition(p);
        updatePopup(index, p);
    }

    private void updatePopup(final I index, final Point p) {
        if (cellContainer.getComponent() == null || index == null)
            return;
        final boolean isEditing = cellContainer.isEditingCell(index);
        final Rectangle allocation = cellContainer.getAllocation();
        final Rectangle cellBounds = cellContainer.getCellBoundsAt(index, isEditing);
        if (cellBounds != null && allocation != null) {
            final Rectangle visibleBounds = allocation.intersection(cellBounds);
            if (visibleBounds.contains(p)) {
                final Component comp = cellContainer.getEffectiveCellRendererComponent(index, isEditing);
                final Dimension prefSize = isEditing ? comp.getBounds().getSize() : comp.getPreferredSize();
                if (comp instanceof JComponent) {
                    // Avoid showing the popup if only the border is obscured.
                    Border border = ((JComponent) comp).getBorder();
                    if (border != null) {
                        Insets borderInsets = border.getBorderInsets(comp);
                        prefSize.width -= borderInsets.left + borderInsets.right;
                        prefSize.height -= borderInsets.top + borderInsets.bottom;
                    }
                }
                if (!(visibleBounds.width >= prefSize.width && visibleBounds.height >= prefSize.height)) {
                    Point popupLocation = cellContainer.getComponent().getLocationOnScreen();
                    Rectangle popupBounds = calculatePopupBounds(cellBounds, visibleBounds, !isEditing);
                    if (!visibleBounds.contains(popupBounds)) {
                        cellBounds.x = popupBounds.x - cellBounds.x;
                        cellBounds.y = popupBounds.y - cellBounds.y;
                        popupBounds.x += popupLocation.x;
                        popupBounds.y += popupLocation.y;
                        enter(index, popupBounds, cellBounds);
                        return;
                    } else {
                        lastIndex = index;
                    }
                }
                leave();
                return;
            }
        }
        lastIndex = null;
        leave();
    }

    private Rectangle calculatePopupBounds(final Rectangle cellBounds, final Rectangle visibleBounds,
            final boolean addBorder) {
        Rectangle rect = new Rectangle(visibleBounds);
        boolean ltr = cellContainer.getComponent().getComponentOrientation().isLeftToRight();
        popupComponent.setBorderInsets(1, 1, 1, 1);

        if (isRespectCellWidth()) {
            if (cellBounds.x >= visibleBounds.x
                    && cellBounds.x + cellBounds.width <= visibleBounds.x + visibleBounds.width) {
                rect.x = cellBounds.x;
                rect.width = cellBounds.width;
            } else {
                int upperDiff = Math.max(cellBounds.x + cellBounds.width - visibleBounds.x - visibleBounds.width, 0);
                int lowerDiff = Math.max(visibleBounds.x - cellBounds.x, 0);
                if (ltr && upperDiff > 0)
                    lowerDiff = 0;
                if (!ltr && lowerDiff > 0)
                    upperDiff = 0;
                if (upperDiff >= lowerDiff) {
                    rect.x = visibleBounds.x + visibleBounds.width;
                    rect.width = upperDiff;
                    popupComponent.setLeft(0);
                } else {
                    rect.x = cellBounds.x;
                    rect.width = lowerDiff;
                    popupComponent.setRight(0);
                }
            }
        }
        if (isRespectCellHeight()) {
            if ((cellBounds.y >= visibleBounds.y
                    && cellBounds.y + cellBounds.height <= visibleBounds.y + visibleBounds.height)) {
                rect.y = cellBounds.y;
                rect.height = cellBounds.height;
            } else {
                int upperDiff = Math.max(cellBounds.y + cellBounds.height - visibleBounds.y - visibleBounds.height, 0);
                int lowerDiff = Math.max(visibleBounds.y - cellBounds.y, 0);
                if (upperDiff > 0)
                    lowerDiff = 0;
                if (upperDiff >= lowerDiff) {
                    rect.y = visibleBounds.y + visibleBounds.height;
                    rect.height = upperDiff;
                    popupComponent.setBottom(0);
                } else {
                    rect.y = cellBounds.y;
                    rect.height = lowerDiff;
                    popupComponent.setTop(0);
                }
            }
        }
        if (!addBorder) {
            popupComponent.setBorderInsets(0, 0, 0, 0);
        } else {
            rect.x -= popupComponent.getLeft();
            rect.y -= popupComponent.getTop();
            rect.width += popupComponent.getLeft() + popupComponent.getRight();
            rect.height += popupComponent.getTop() + popupComponent.getBottom();
        }
        return rect;
    }

    @Override
    public void mouseExited(final MouseEvent e) {
        if (isOverEditor(e.getPoint())) {
            if (popup == null) {
                /*
                 * If mouse is over editor and no popup is currently visible check if we need to show the popup.
                 */
                mouseMoved(e);
            }
            return;
        }
        leave();
    }

    private boolean isOverEditor(final Point p) {
        return cellContainer.isEditing() && cellContainer.getCellEditorComponent(lastIndex).getBounds().contains(p);
    }

    public void repaint() {
        if (!cellContainer.getComponent().isShowing())
            return;
        if (popup != null)
            popupComponent.repaint();
        if (lastIndex != null) {
            Point p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, cellContainer.getComponent());
            updatePopup(lastIndex, p);
        }
    }

    private void enter(final I index, final Rectangle bounds, final Rectangle rendererBounds) {
        if (index != null) {
            lastIndex = index;
            popupComponent.setPreferredSize(bounds.getSize());
            popupComponent.setRendererBounds(rendererBounds);
            if (popup != null) {
                Point p = popupComponent.isShowing() ? popupComponent.getLocationOnScreen() : null;
                if (p == null || p.x != bounds.x || p.y != bounds.y || popupComponent.getWidth() != bounds.width
                        || popupComponent.getHeight() != bounds.height) {
                    movePopup(bounds);
                }
            }
            if (popup == null) {
                popup = PopupFactory.getSharedInstance().getPopup(cellContainer.getComponent(), popupComponent,
                        bounds.x, bounds.y);
                popup.show();
                if (DarkPopupFactory.getPopupType(popup) == DarkPopupFactory.PopupType.HEAVY_WEIGHT) {
                    // Ensure heavy weight popup is at desired location.
                    SwingUtilities.invokeLater(() -> {
                        Window w = DarkUIUtil.getWindow(popupComponent);
                        w.setBounds(bounds);
                        WindowUtil.moveWindow(w, popupComponent, bounds.x, bounds.y);
                    });
                }
            }
        }
    }

    private void movePopup(final Rectangle bounds) {
        if (popup == null)
            return;
        DarkPopupFactory.PopupType popupType = DarkPopupFactory.getPopupType(popup);
        Window w = DarkUIUtil.getWindow(popupComponent);
        if (popupType == DarkPopupFactory.PopupType.HEAVY_WEIGHT) {
            moveHeavyWeightPopup(bounds, w);
        } else if (w instanceof RootPaneContainer) {
            moveMediumLightWeightPopup(bounds, w);
        }
        popupComponent.repaint();
    }

    private void moveHeavyWeightPopup(final Rectangle bounds, final Window popupWindow) {
        GraphicsConfiguration gc = popupWindow.getGraphicsConfiguration();
        GraphicsConfiguration componentGc = cellContainer.getComponent().getGraphicsConfiguration();
        if (!Objects.equals(componentGc, gc)) {
            popup.hide();
            popup = null;
        } else {
            popupWindow.setBounds(bounds);
            WindowUtil.moveWindow(popupWindow, popupComponent, bounds.x, bounds.y);
        }
    }

    private void moveMediumLightWeightPopup(final Rectangle bounds, final Window parentWindow) {
        JLayeredPane layeredPane = ((RootPaneContainer) parentWindow).getLayeredPane();
        JRootPane rootPane = ((RootPaneContainer) parentWindow).getRootPane();
        Component comp = DarkUIUtil.getParentBeforeMatching(popupComponent.getParent(), c -> c == layeredPane);
        Rectangle windowBounds = parentWindow.getBounds();
        if (windowBounds.contains(bounds.x, bounds.y)
                && windowBounds.contains(bounds.x + bounds.width, bounds.y + bounds.height)) {
            Point windowPos = rootPane.getLocationOnScreen();
            bounds.x -= windowPos.x;
            bounds.y -= windowPos.y;
            comp.setBounds(bounds);
            Component c = popupComponent;
            while (c != null && comp != c) {
                c.setBounds(0, 0, bounds.width, bounds.height);
                c = c.getParent();
            }
        } else {
            /*
             * Popup was moved outside the window. Request heavy weight popup.
             */
            popup.hide();
            popup = null;
        }
    }

    private void leave() {
        if (popup != null) {
            popup.hide();
            popup = null;
        }
    }

    private Component getRenderer() {
        return cellContainer.getEffectiveCellRendererComponent(lastIndex, cellContainer.isEditingCell(lastIndex));
    }

    private Color getBackground(final Component renderer) {
        return cellContainer.getBackgroundAt(lastIndex, renderer);
    }

    private static class PopupComponent extends JComponent {

        private final CellHintPopupListener<?, ?> cellHintPopupListener;
        private final Insets borderInsets = new Insets(0, 0, 0, 0);
        private final Color borderColor;
        private Rectangle rendererBounds;

        private PopupComponent(final CellHintPopupListener<?, ?> cellHintPopupListener) {
            this.cellHintPopupListener = cellHintPopupListener;
            this.borderColor = UIManager.getColor("CellHintPopup.borderColor");
            putClientProperty(DarkPopupFactory.KEY_NO_DECORATION, true);
            putClientProperty(DarkPopupFactory.KEY_OPAQUE, true);
        }

        public void setTop(final int top) {
            borderInsets.top = top;
        }

        public void setBottom(final int bottom) {
            borderInsets.bottom = bottom;
        }

        public void setLeft(final int left) {
            borderInsets.left = left;
        }

        public void setRight(final int right) {
            borderInsets.right = right;
        }

        public int getTop() {
            return borderInsets.top;
        }

        public int getBottom() {
            return borderInsets.bottom;
        }

        public int getLeft() {
            return borderInsets.left;
        }

        public int getRight() {
            return borderInsets.right;
        }

        public void setBorderInsets(final int top, final int left, final int bottom, final int right) {
            borderInsets.set(top, left, bottom, right);
        }

        public void setRendererBounds(final Rectangle rendererBounds) {
            this.rendererBounds = rendererBounds;
        }

        @Override
        public void paint(final Graphics g) {
            Component renderer = cellHintPopupListener.getRenderer();
            if (rendererBounds != null && renderer != null) {
                Color bg = cellHintPopupListener.getBackground(renderer);
                if (bg == null)
                    bg = cellHintPopupListener.cellContainer.getComponent().getBackground();
                if (bg == null)
                    bg = getBackground();
                if (bg != null) {
                    g.setColor(bg);
                    g.fillRect(0, 0, getWidth(), getHeight());
                }
                g.translate(-rendererBounds.x, -rendererBounds.y);

                // If the renderer is an editor we need to restore the bounds.
                Rectangle bounds = renderer.getBounds();
                renderer.setBounds(0, 0, rendererBounds.width, rendererBounds.height);
                renderer.doLayout();
                renderer.paint(g);
                renderer.setBounds(bounds);

                g.translate(rendererBounds.x, rendererBounds.y);
            }
            g.setColor(getBorderColor());
            PaintUtil.drawRect(g, 0, 0, getWidth(), getHeight(), borderInsets);
        }

        public Color getBorderColor() {
            return borderColor;
        }
    }
}
