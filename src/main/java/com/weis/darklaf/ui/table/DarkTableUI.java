package com.weis.darklaf.ui.table;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

public class DarkTableUI extends DarkTableUIBridge {

    private static final int ROW_HEIGHT = 22;
    private final FocusListener focusListener = new FocusListener() {
        @Override
        public void focusGained(final FocusEvent e) {
            var bg = table.getSelectionBackground();
            if (bg instanceof UIResource) {
                table.setSelectionBackground(UIManager.getColor("Table.focusSelectionBackground"));
            }
            table.repaint();
        }

        @Override
        public void focusLost(final FocusEvent e) {
            var bg = table.getSelectionBackground();
            if (bg instanceof UIResource) {
                if (table.isEditing()) {
                    table.setSelectionBackground(table.getBackground());
                } else {
                    table.setSelectionBackground(UIManager.getColor("Table.selectionBackground"));
                }
            }
            table.repaint();
        }
    };

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTableUI();
    }

    protected static int adjustDistance(final int distance, @NotNull final Rectangle rect,
                                        @NotNull final JTable comp) {
        int dist = distance;
        int min = 0;
        int max = comp.getX() + comp.getWidth();
        if (rect.x + dist <= min) {
            dist = min - rect.x;
        }
        if (rect.x + rect.width + dist >= max) {
            dist = max - rect.x - rect.width;
        }
        return dist;
    }

    protected static void setupRendererComponents(@NotNull final JTable table) {
        var cellRenderer = new DarkTableCellRenderer();
        table.setDefaultRenderer(Object.class, cellRenderer);
        table.setDefaultRenderer(String.class, cellRenderer);
        table.setDefaultRenderer(Integer.class, cellRenderer);
        table.setDefaultRenderer(Double.class, cellRenderer);
        table.setDefaultRenderer(Float.class, cellRenderer);
        table.setDefaultRenderer(Boolean.class, cellRenderer);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        table.setRowHeight(ROW_HEIGHT);
        table.setDefaultEditor(Object.class, new DarkTableCellEditor());
        setupRendererComponents(table);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        table.addFocusListener(focusListener);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        table.removeFocusListener(focusListener);
    }

    @Override
    protected void paintDraggedArea(@NotNull final Graphics g, final int rMin, final int rMax,
                                    final int cMin, final int cMax,
                                    final TableColumn draggedColumn, final int distance) {
        int draggedColumnIndex = viewIndexForColumn(draggedColumn);

        Rectangle minCell = table.getCellRect(rMin, draggedColumnIndex, true);
        Rectangle maxCell = table.getCellRect(rMax, draggedColumnIndex, true);

        Rectangle vacatedColumnRect = minCell.union(maxCell);

        int dist = adjustDistance(distance, vacatedColumnRect, table);

        // Paint a gray well in place of the moving column.
        g.setColor(table.getParent().getBackground());
        g.fillRect(vacatedColumnRect.x, vacatedColumnRect.y,
                   vacatedColumnRect.width - 1, vacatedColumnRect.height);


        // Move to the where the cell has been dragged.
        vacatedColumnRect.x += dist;

        boolean scrollVisible = scrollBarVisible();
        boolean drawBottomBorder = !table.getShowHorizontalLines() && !scrollVisible;
        boolean ltr = table.getComponentOrientation().isLeftToRight();

        // Fill the background.
        g.setColor(table.getBackground());
        g.fillRect(vacatedColumnRect.x, vacatedColumnRect.y,
                   vacatedColumnRect.width, vacatedColumnRect.height);


        // Paint the vertical grid lines if necessary.
        if (table.getShowVerticalLines()) {
            g.setColor(table.getGridColor());
            int x1 = vacatedColumnRect.x;
            int y1 = vacatedColumnRect.y;
            int x2 = x1 + vacatedColumnRect.width - 1;
            int y2 = y1 + vacatedColumnRect.height - 1;

            boolean onLeftEdge = ltr ? draggedColumnIndex == cMin : draggedColumnIndex == cMax;
            boolean onRightEdge = ltr ? draggedColumnIndex == cMax : draggedColumnIndex == cMin;
            if (scrollBarVisible()) {
                if (isScrollPaneRtl()) {
                    onLeftEdge = false;
                } else {
                    onRightEdge = false;
                }
            }
            // Left
            if (dist != 0 || !onLeftEdge) {
                if (draggedColumnIndex == cMin && scrollBarVisible() && isScrollPaneRtl()) x1++;
                g.fillRect(x1 - 1, y1, 1, y2 - y1);
            }
            // Right
            if (dist != 0 || !onRightEdge) {
                g.fillRect(x2, y1, 1, y2 - y1);
            }
        }

        for (int row = rMin; row <= rMax; row++) {
            // Render the cell value
            Rectangle r = table.getCellRect(row, draggedColumnIndex, false);
            r.x += dist;
            paintCell(g, r, row, draggedColumnIndex, cMin, cMax);

            // Paint the (lower) horizontal grid line if necessary.
            if (table.getShowHorizontalLines() || (!scrollVisible && row == rMax)) {
                g.setColor(table.getGridColor());
                if (drawBottomBorder) {
                    g.setColor(getBorderColor());
                }
                Rectangle rcr = table.getCellRect(row, draggedColumnIndex, true);
                rcr.x += dist;
                int x1 = rcr.x - 1;
                int y1 = rcr.y;
                int x2 = x1 + rcr.width + 1;
                int y2 = y1 + rcr.height - 1;
                g.fillRect(x1, y2, x2 - x1, 1);
            }
        }
        if (drawBottomBorder) {
            var rect = table.getCellRect(rMax, draggedColumnIndex, true);
            int y = rect.y + rect.height - 1;
            g.setColor(getBorderColor());
            g.fillRect(rect.x, y, rect.width, 1);
        }
    }

    @Override
    protected void paintGrid(@NotNull final Graphics g,
                             final int rMin, final int rMax, final int cMin, final int cMax) {
        g.setColor(table.getGridColor());

        Rectangle minCell = table.getCellRect(rMin, cMin, true);
        Rectangle maxCell = table.getCellRect(rMax, cMax, true);
        Rectangle damagedArea = minCell.union(maxCell);

        int draggedIndex = viewIndexForColumn(table.getTableHeader().getDraggedColumn());
        if (table.getShowHorizontalLines()) {
            int tableWidth = damagedArea.x + damagedArea.width;
            if (table.getComponentOrientation().isLeftToRight()) {
                if (draggedIndex == cMax) {
                    tableWidth -= 1;
                }
            } else {
                if (draggedIndex == cMin) {
                    tableWidth -= 1;
                }
            }
            int y = damagedArea.y;
            for (int row = rMin; row <= rMax; row++) {
                y += table.getRowHeight(row);
                SwingUtilities2.drawHLine(g, damagedArea.x, tableWidth - 1, y - 1);
            }
        }

        boolean scrollPaneRtl = isScrollPaneRtl();
        boolean scrollVisible = scrollBarVisible();
        if (table.getShowVerticalLines()) {
            TableColumnModel cm = table.getColumnModel();
            int tableHeight = damagedArea.y + damagedArea.height;
            int x;
            boolean ltr = table.getComponentOrientation().isLeftToRight();
            if (ltr) {
                x = damagedArea.x;
                if (scrollPaneRtl && scrollBarVisible()) {
                    SwingUtilities2.drawVLine(g, x, 0, tableHeight - 1);
                }
                for (int column = cMin; column <= cMax; column++) {
                    int w = cm.getColumn(column).getWidth();
                    x += w;
                    if (showVerticalLine(true, scrollVisible, !scrollPaneRtl, column, draggedIndex, cMin, cMax)) {
                        SwingUtilities2.drawVLine(g, x - 1, 0, tableHeight - 1);
                    }
                }
            } else {
                x = damagedArea.x;
                if (scrollPaneRtl && scrollBarVisible()) {
                    SwingUtilities2.drawVLine(g, x, 0, tableHeight - 1);
                }
                for (int column = cMax; column >= cMin; column--) {
                    int w = cm.getColumn(column).getWidth();
                    x += w;
                    if (showVerticalLine(false, scrollVisible, !scrollPaneRtl, column, draggedIndex, cMin, cMax)) {
                        SwingUtilities2.drawVLine(g, x - 1, 0, tableHeight - 1);
                    }
                }
            }
        }
        if (!table.getShowHorizontalLines() && table.getRowCount() != 0 && !scrollVisible) {
            g.setColor(getBorderColor());
            int y = table.getHeight() - 1;
            g.fillRect(0, y, table.getWidth(), 1);
        }
    }

    protected Color getBorderColor() {
        return UIManager.getColor("TableHeader.borderColor");
    }

    protected boolean isScrollPaneRtl() {
        if (!isInScrollPane()) return false;
        Container comp = SwingUtilities.getUnwrappedParent(table).getParent();
        return !comp.getComponentOrientation().isLeftToRight();
    }

    protected boolean scrollBarVisible() {
        Container comp = SwingUtilities.getUnwrappedParent(table);
        if (comp != null) {
            comp = comp.getParent();
        }
        return comp instanceof JScrollPane && ((JScrollPane) comp).getVerticalScrollBar().isVisible();
    }

    protected boolean isInScrollPane() {
        Container comp = SwingUtilities.getUnwrappedParent(table);
        if (comp != null) {
            comp = comp.getParent();
        }
        return comp instanceof JScrollPane;
    }

    protected boolean showVerticalLine(final boolean ltr, final boolean scrollVisible,
                                       final boolean scrollLtR, final int column, final int draggedIndex,
                                       final int cMin, final int cMax) {
        int dist = adjustDistance(table.getTableHeader().getDraggedDistance(),
                                  table.getCellRect(0, draggedIndex, true),
                                  table);
        boolean isDragged = column == draggedIndex && dist != 0;
        if (!scrollVisible) {
            if (ltr) {
                return column != cMax;
            } else {
                return column != cMin;
            }
        } else {
            if (ltr) {
                if (scrollLtR) {
                    return column != cMax || !isDragged;
                } else {
                    return column != cMax;
                }
            } else {
                if (scrollLtR) {
                    return column != cMin || !isDragged;
                } else {
                    return column != cMin;
                }
            }
        }
    }

    @Override
    protected void paintCell(final Graphics g, final Rectangle cellRect, final int row, final int column,
                             final int cMin, final int cMax) {
        boolean scrollLtR = !isScrollPaneRtl();
        boolean ltr = table.getComponentOrientation().isLeftToRight();
        int draggedIndex = viewIndexForColumn(table.getTableHeader().getDraggedColumn());
        int dist = adjustDistance(table.getTableHeader().getDraggedDistance(),
                                  table.getCellRect(row, draggedIndex, true),
                                  table);
        boolean isDragged = column == draggedIndex && dist != 0;
        var r = new Rectangle(cellRect);
        if (!scrollBarVisible()) {
            if (ltr) {
                if (column == cMax && !isDragged) r.width += 1;
            } else {
                if (column == cMin && !isDragged) r.width += 1;
            }
        } else if (!scrollLtR) {
            if (ltr) {
                if (column == cMax && !isDragged) r.width += 1;
                if (column == cMin && !isDragged) {
                    r.width -= 1;
                    r.x += 1;
                }
            } else {
                if (column == cMin && !isDragged) r.width += 1;
                if (column == cMax && !isDragged) {
                    r.width -= 1;
                    r.x += 1;
                }
            }
        }
        super.paintCell(g, r, row, column, cMin, cMax);
    }
}
