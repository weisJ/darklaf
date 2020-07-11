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
package com.github.weisj.darklaf.ui.table;

import java.awt.*;

import javax.swing.*;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;

import com.github.weisj.darklaf.util.DarkUIUtil;

/**
 * @author Jannis Weis
 */
public abstract class DarkTableUIBridge extends TableUIBridge {

    protected Color dropLine;
    protected Color dropLineShort;

    @Override
    protected void installDefaults() {
        super.installDefaults();
        dropLine = UIManager.getColor("Table.dropLineColor");
        dropLineShort = UIManager.getColor("Table.dropLineShortColor");
    }

    /**
     * Paint a representation of the <code>table</code> instance that was set in installUI().
     */
    @Override
    public void paint(final Graphics g, final JComponent c) {
        Rectangle clip = g.getClipBounds();

        Rectangle bounds = table.getBounds();
        // account for the fact that the graphics has already been translated
        // into the table's bounds
        bounds.x = bounds.y = 0;

        if (table.getRowCount() <= 0 || table.getColumnCount() <= 0 ||
        // this check prevents us from painting the entire table
        // when the clip doesn't intersect our bounds at all
            !bounds.intersects(clip)) {

            paintDropLines(g);
            return;
        }

        boolean ltr = table.getComponentOrientation().isLeftToRight();
        Point upperLeft, lowerRight;
        // compute the visible part of table which needs to be painted
        Rectangle visibleBounds = clip.intersection(bounds);
        upperLeft = visibleBounds.getLocation();
        lowerRight = new Point(visibleBounds.x + visibleBounds.width - 1,
                               visibleBounds.y + visibleBounds.height - 1);

        int rMin = table.rowAtPoint(upperLeft);
        int rMax = table.rowAtPoint(lowerRight);
        // This should never happen (as long as our bounds intersect the clip,
        // which is why we bail above if that is the case).
        if (rMin == -1) {
            rMin = 0;
        }
        // If the table does not have enough rows to fill the view we'll get -1.
        // (We could also get -1 if our bounds don't intersect the clip,
        // which is why we bail above if that is the case).
        // Replace this with the index of the last row.
        if (rMax == -1) {
            rMax = table.getRowCount() - 1;
        }

        // For FIT_WIDTH, all columns should be printed irrespective of
        // how many columns are visible. So, we used clip which is already set to
        // total col width instead of visible region
        // Since JDarkTableUI.KEY_IS_PRINT_MODE is not accessible
        // from here, we aet DarkTableUI.KEY_IS_PRINT_MODE in TablePrintable#print and
        // access from here.
        Object printMode = table.getClientProperty(DarkTableUI.KEY_IS_PRINT_MODE);
        if ((printMode == JTable.PrintMode.FIT_WIDTH)) {
            upperLeft = clip.getLocation();
            lowerRight = new Point(clip.x + clip.width - 1,
                                   clip.y + clip.height - 1);
        }
        int cMin = table.columnAtPoint(ltr ? upperLeft : lowerRight);
        int cMax = table.columnAtPoint(ltr ? lowerRight : upperLeft);
        // This should never happen.
        if (cMin == -1) {
            cMin = 0;
        }
        // If the table does not have enough columns to fill the view we'll get -1.
        // Replace this with the index of the last column.
        if (cMax == -1) {
            cMax = table.getColumnCount() - 1;
        }

        Component comp = DarkUIUtil.getUnwrappedParent(table);
        if (comp != null) {
            comp = comp.getParent();
        }

        if (comp != null && !(comp instanceof JViewport) && !(comp instanceof JScrollPane)) {
            // We did rMax-1 to paint the same number of rows that are drawn on console
            // otherwise 1 extra row is printed per page than that are displayed
            // when there is no scrollPane and we do printing of table
            // but not when rmax is already pointing to index of last row
            // and if there is any selected rows
            if (rMax != (table.getRowCount() - 1) &&
                (table.getSelectedRow() == -1)) {
                // Do not decrement rMax if rMax becomes
                // less than or equal to rMin
                // else cells will not be painted
                if (rMax - rMin > 1) {
                    rMax = rMax - 1;
                }
            }
        }

        // Paint the grid.
        paintGrid(g, rMin, rMax, cMin, cMax);

        // Paint the cells.
        paintCells(g, rMin, rMax, cMin, cMax);

        paintDropLines(g);
    }

    @Override
    protected void paintDropLines(final Graphics g) {
        JTable.DropLocation loc = table.getDropLocation();
        if (loc == null) {
            return;
        }

        Color color = dropLine;
        Color shortColor = dropLineShort;
        if (color == null && shortColor == null) {
            return;
        }

        Rectangle rect;

        rect = getHDropLineRect(loc);
        if (rect != null) {
            int x = rect.x;
            int w = rect.width;
            if (color != null) {
                extendRect(rect, true);
                g.setColor(color);
                g.fillRect(rect.x, rect.y, rect.width, rect.height);
            }
            if (!loc.isInsertColumn() && shortColor != null) {
                g.setColor(shortColor);
                g.fillRect(x, rect.y, w, rect.height);
            }
        }

        rect = getVDropLineRect(loc);
        if (rect != null) {
            int y = rect.y;
            int h = rect.height;
            if (color != null) {
                extendRect(rect, false);
                g.setColor(color);
                g.fillRect(rect.x, rect.y, rect.width, rect.height);
            }
            if (!loc.isInsertRow() && shortColor != null) {
                g.setColor(shortColor);
                g.fillRect(rect.x, y, rect.width, h);
            }
        }
    }

    @Override
    protected abstract void paintGrid(final Graphics g, final int rMin, final int rMax, final int cMin, final int cMax);

    @Override
    protected void paintCells(final Graphics g, final int rMin, final int rMax, final int cMin, final int cMax) {
        JTableHeader header = table.getTableHeader();
        TableColumn draggedColumn = (header == null) ? null : header.getDraggedColumn();

        TableColumnModel cm = table.getColumnModel();
        int columnMargin = cm.getColumnMargin();

        Rectangle cellRect;
        TableColumn aColumn;
        int columnWidth;
        if (table.getComponentOrientation().isLeftToRight()) {
            for (int row = rMin; row <= rMax; row++) {
                cellRect = table.getCellRect(row, cMin, false);
                for (int column = cMin; column <= cMax; column++) {
                    aColumn = cm.getColumn(column);
                    columnWidth = aColumn.getWidth();
                    cellRect.width = columnWidth - columnMargin;
                    if (aColumn != draggedColumn) {
                        paintCell(g, cellRect, row, column, rMin, rMax);
                    }
                    cellRect.x += columnWidth;
                }
            }
        } else {
            for (int row = rMin; row <= rMax; row++) {
                cellRect = table.getCellRect(row, cMin, false);
                aColumn = cm.getColumn(cMin);
                if (aColumn != draggedColumn) {
                    columnWidth = aColumn.getWidth();
                    cellRect.width = columnWidth - columnMargin;
                    paintCell(g, cellRect, row, cMin, cMin, cMax);
                }
                for (int column = cMin + 1; column <= cMax; column++) {
                    aColumn = cm.getColumn(column);
                    columnWidth = aColumn.getWidth();
                    cellRect.width = columnWidth - columnMargin;
                    cellRect.x -= columnWidth;
                    if (aColumn != draggedColumn) {
                        paintCell(g, cellRect, row, column, cMin, cMax);
                    }
                }
            }
        }

        // Paint the dragged column if we are dragging.
        if (draggedColumn != null) {
            paintDraggedArea(g, rMin, rMax, cMin, cMax, draggedColumn, header.getDraggedDistance());
        }

        // Remove any renderers that may be left in the rendererPane.
        rendererPane.removeAll();
    }

    @Deprecated
    protected final void paintCell(final Graphics g, final Rectangle cellRect, final int row, final int column) {}

    protected abstract void paintCell(final Graphics g, final Rectangle cellRect, final int row, final int column,
                                      final int cMin, final int cMax);

    @Override
    protected int viewIndexForColumn(final TableColumn aColumn) {
        return viewIndexForColumn(aColumn, table);
    }

    protected abstract void paintDraggedArea(final Graphics g,
                                             final int rMin, final int rMax,
                                             final int cMin, final int cMax,
                                             final TableColumn draggedColumn, final int distance);

    public static int viewIndexForColumn(final TableColumn aColumn, final JTable table) {
        TableColumnModel cm = table.getColumnModel();
        for (int column = 0; column < cm.getColumnCount(); column++) {
            if (cm.getColumn(column) == aColumn) {
                return column;
            }
        }
        return -1;
    }
}
