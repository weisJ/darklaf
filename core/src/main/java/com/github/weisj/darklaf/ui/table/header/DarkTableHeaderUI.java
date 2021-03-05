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
package com.github.weisj.darklaf.ui.table.header;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicTableHeaderUI;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import com.github.weisj.darklaf.ui.table.DarkTableScrollPaneBorder;
import com.github.weisj.darklaf.ui.table.DarkTableUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;

/** @author Jannis Weis */
public class DarkTableHeaderUI extends BasicTableHeaderUI {

    private static final int HEADER_HEIGHT = 26;
    protected Color borderColor;
    protected Color background;
    protected int defaultHeight;
    protected DarkTableHeaderRendererDelegate rendererDelegate;
    private int lastMaxVisible = -1;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkTableHeaderUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        header.remove(rendererPane);
        rendererPane = createCellRendererPane();
        header.add(rendererPane);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        background = UIManager.getColor("TableHeader.background");
        borderColor = UIManager.getColor("TableHeader.borderColor");
        LookAndFeel.installBorder(header, "TableHeader.border");
        defaultHeight = UIManager.getInt("TableHeader.height");
        if (defaultHeight <= 0) {
            defaultHeight = HEADER_HEIGHT;
        }
        rendererDelegate = new DarkTableHeaderRendererDelegate(header.getDefaultRenderer());
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        Dimension dim = super.getPreferredSize(c);
        dim.height = Math.max(defaultHeight, dim.height);
        return dim;
    }

    protected CellRendererPane createCellRendererPane() {
        return new DarkTableHeaderRendererPane();
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
        LookAndFeel.uninstallBorder(header);
    }

    @Override
    public void paint(final Graphics g2, final JComponent c) {
        final Graphics2D g = (Graphics2D) g2;
        final GraphicsContext config = new GraphicsContext(g);

        int x = 0;
        int y = 0;
        int h = c.getHeight();
        int w = c.getWidth();

        g.setColor(getHeaderBackground());
        g.fillRect(x, y, w, h);

        if (header.getColumnModel().getColumnCount() <= 0) {
            return;
        }
        boolean ltr = header.getComponentOrientation().isLeftToRight();

        Rectangle clip = g.getClipBounds();
        Point leftClip = clip.getLocation();
        Point rightCip = new Point(clip.x + clip.width - 1, clip.y);
        TableColumnModel cm = header.getColumnModel();
        int cMin = Math.max(header.columnAtPoint(ltr ? leftClip : rightCip), 0);
        int cMax = header.columnAtPoint(ltr ? rightCip : leftClip);

        // If the table does not have enough columns to fill the view we'll get -1.
        // Replace this with the index of the last column.
        if (cMax == -1) {
            cMax = cm.getColumnCount() - 1;
        }

        Rectangle bounds = c.getVisibleRect();
        Point left = bounds.getLocation();
        Point right = new Point(bounds.x + bounds.width - 1, bounds.y);
        int cMinVisible = Math.max(header.columnAtPoint(ltr ? left : right), 0);
        int cMaxVisible = header.columnAtPoint(ltr ? right : left);

        if (cMaxVisible == -1) {
            cMaxVisible = cm.getColumnCount() - 1;
        }

        Color borderColor = c.getBorder() instanceof DarkTableScrollPaneBorder
                ? ((DarkTableScrollPaneBorder) c.getBorder()).getBorderColor()
                : getBorderColor();
        g.setColor(borderColor);

        TableColumn draggedColumn = header.getDraggedColumn();
        Rectangle cellRect = header.getHeaderRect(ltr ? cMin : cMax);
        paintCells(g, h, ltr, cm, cMin, cMax, cMinVisible, cMaxVisible, borderColor, draggedColumn, cellRect);

        // Paint the dragged column if we are dragging.
        if (draggedColumn != null) {
            paintDraggedArea(g, ltr, cMin, cMax, borderColor, draggedColumn);
        }

        // Remove all components in the rendererPane.
        rendererPane.removeAll();
        config.restore();

        /*
         * If the table is in a scroll pane which uses buffer backed viewports scrolling leads to the
         * previously last cell missing part of its border. We manually repaint it to prevent the appearance
         * of joint cells.
         */
        if (lastMaxVisible != cMaxVisible) {
            if (lastMaxVisible >= 0) {
                Rectangle r = header.getHeaderRect(lastMaxVisible);
                SwingUtilities.invokeLater(() -> header.repaint(r));
            }
            lastMaxVisible = cMaxVisible;
        }
    }

    public void paintCells(final Graphics2D g, final int h, final boolean ltr, final TableColumnModel cm,
            final int cMin, final int cMax, final int cMinVisible, final int cMaxVisible, final Color borderColor,
            final TableColumn draggedColumn, final Rectangle cellRect) {
        if (ltr) {
            for (int column = cMin; column <= cMax; column++) {
                paintSingleCell(g, h, cm, cMinVisible, cMaxVisible, borderColor, draggedColumn, cellRect, column);
            }
        } else {
            for (int column = cMax; column >= cMin; column--) {
                paintSingleCell(g, h, cm, cMinVisible, cMaxVisible, borderColor, draggedColumn, cellRect, column);
            }
        }
    }

    public void paintSingleCell(final Graphics2D g, final int h, final TableColumnModel cm, final int cMinVisible,
            final int cMaxVisible, final Color borderColor, final TableColumn draggedColumn, final Rectangle cellRect,
            final int column) {
        TableColumn aColumn;
        int columnWidth;
        aColumn = cm.getColumn(column);
        columnWidth = aColumn.getWidth();
        cellRect.width = columnWidth;
        if (aColumn != draggedColumn) {
            paintCell(g, cellRect, column);
        }
        g.setColor(borderColor);
        if (column < cMaxVisible) {
            g.fillRect(cellRect.x + cellRect.width - 1, 0, 1, h);
        }
        cellRect.x += cellRect.width;
    }

    public void paintDraggedArea(final Graphics2D g, final boolean ltr, final int cMin, final int cMax,
            final Color borderColor, final TableColumn draggedColumn) {
        int draggedColumnIndex = viewIndexForColumn(draggedColumn);
        boolean scrollPaneRtl = isScrollPaneRtl();
        Rectangle draggedCellRect = header.getHeaderRect(draggedColumnIndex);
        int dist = DarkTableUI.adjustDistance(header.getDraggedDistance(), draggedCellRect, header.getTable());
        // Draw a gray well in place of the moving column.
        g.setColor(header.getParent().getBackground());
        g.fillRect(draggedCellRect.x, draggedCellRect.y, draggedCellRect.width, draggedCellRect.height);
        g.setColor(borderColor);
        if (scrollBarVisible()) {
            if (ltr) {
                if (!scrollPaneRtl) {
                    g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1,
                            draggedCellRect.height);
                } else {
                    if (draggedColumnIndex != cMax) {
                        g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1,
                                draggedCellRect.height);
                    } else {
                        g.fillRect(draggedCellRect.x + draggedCellRect.width, draggedCellRect.y, 1,
                                draggedCellRect.height);
                    }
                    if (draggedColumnIndex == cMin) {
                        g.fillRect(draggedCellRect.x, draggedCellRect.y, 1, draggedCellRect.height);
                    }
                }
            } else {
                if (!scrollPaneRtl) {
                    g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1,
                            draggedCellRect.height);
                } else {
                    if (draggedColumnIndex != cMin) {
                        g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1,
                                draggedCellRect.height);
                    }
                    if (draggedColumnIndex == cMax) {
                        g.fillRect(draggedCellRect.x, draggedCellRect.y, 1, draggedCellRect.height);
                    }
                }
            }
        } else {
            if (ltr) {
                if (draggedColumnIndex != cMax) {
                    g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1,
                            draggedCellRect.height);
                } else {
                    g.fillRect(draggedCellRect.x + draggedCellRect.width, draggedCellRect.y, 1, draggedCellRect.height);
                }
            } else {
                if (draggedColumnIndex != cMin) {
                    g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1,
                            draggedCellRect.height);
                }
            }
        }

        draggedCellRect.x += dist;

        // Fill the background.
        g.setColor(header.getBackground());
        g.fillRect(draggedCellRect.x, draggedCellRect.y, draggedCellRect.width, draggedCellRect.height);
        paintCell(g, draggedCellRect, draggedColumnIndex);

        g.setColor(borderColor);

        boolean onLeftEdge = ltr ? draggedColumnIndex == cMin : draggedColumnIndex == cMax;
        boolean onRightEdge = ltr ? draggedColumnIndex == cMax : draggedColumnIndex == cMin;
        // left
        if (dist != 0 || !onLeftEdge) {
            g.fillRect(draggedCellRect.x - 1, draggedCellRect.y, 1, draggedCellRect.height);
        }
        // right
        if (dist != 0 || !onRightEdge) {
            g.fillRect(draggedCellRect.x + draggedCellRect.width - 1, draggedCellRect.y, 1, draggedCellRect.height);
        }
    }

    protected void paintCell(final Graphics g, final Rectangle cellRect, final int columnIndex) {
        Component component = getHeaderCellRenderer(columnIndex);
        rendererPane.paintComponent(g, component, header, cellRect.x, cellRect.y, cellRect.width, cellRect.height,
                true);
    }

    protected Color getHeaderBackground() {
        return background;
    }

    protected Color getBorderColor() {
        return borderColor;
    }

    protected boolean isScrollPaneRtl() {
        if (!isInScrollPane()) return false;
        Container comp = DarkUIUtil.getUnwrappedParent(header).getParent();
        return !comp.getComponentOrientation().isLeftToRight();
    }

    protected boolean scrollBarVisible() {
        Component comp = DarkUIUtil.getUnwrappedParent(header);
        if (comp != null) {
            comp = comp.getParent();
        }
        return comp instanceof JScrollPane && ((JScrollPane) comp).getVerticalScrollBar().isVisible();
    }

    protected boolean isInScrollPane() {
        Component comp = DarkUIUtil.getUnwrappedParent(header);
        if (comp != null) {
            comp = comp.getParent();
        }
        return comp instanceof JScrollPane;
    }

    protected int viewIndexForColumn(final TableColumn aColumn) {
        TableColumnModel cm = header.getColumnModel();
        for (int column = 0; column < cm.getColumnCount(); column++) {
            if (cm.getColumn(column) == aColumn) {
                return column;
            }
        }
        return -1;
    }

    protected Component getHeaderCellRenderer(final int columnIndex) {
        TableColumn aColumn = header.getColumnModel().getColumn(columnIndex);
        TableCellRenderer renderer = aColumn.getHeaderRenderer();
        if (renderer == null) {
            renderer = header.getDefaultRenderer();
        }

        boolean hasFocus = !header.isPaintingForPrint() && header.hasFocus();
        rendererDelegate.setDelegate(renderer);
        return rendererDelegate.getTableCellRendererComponent(header.getTable(), aColumn.getHeaderValue(), false,
                hasFocus, -1, columnIndex);
    }
}
