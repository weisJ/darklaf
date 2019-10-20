/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.table;

import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;
import sun.swing.UIAction;

import javax.swing.*;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicTableHeaderUI;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellRenderer;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.util.Enumeration;
import java.util.Objects;

/**
 * @author Jannis Weis
 */
public class DarkTableHeaderUIBridge extends BasicTableHeaderUI {

    protected static final Cursor resizeCursor = Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR);

    //
// Instance Variables
//
    protected static final FocusListener focusListener = new FocusListener() {
        public void focusGained(@NotNull final FocusEvent e) {
            repaintHeader(e.getSource());
        }

        public void focusLost(@NotNull final FocusEvent e) {
            repaintHeader(e.getSource());
        }

        protected void repaintHeader(final Object source) {
            if (source instanceof JTableHeader) {
                JTableHeader th = (JTableHeader) source;
                DarkTableHeaderUIBridge ui =
                        (DarkTableHeaderUIBridge) DarkUIUtil.getUIOfType(th.getUI(), DarkTableHeaderUIBridge.class);
                if (ui == null) {
                    return;
                }
                th.repaint(th.getHeaderRect(ui.getSelectedColumnIndex()));
            }
        }
    };
    // The column header over which the mouse currently is.
    protected int rolloverColumn = -1;
    // The column that should be highlighted when the table header has the focus.
    protected int selectedColumnIndex = 0; // Read ONLY via getSelectedColumnIndex!

    /**
     * Returns a new instance of {@code DarkTableHeaderUIBridge}.
     *
     * @param h a component.
     * @return a new instance of {@code DarkTableHeaderUIBridge}
     */
    public static ComponentUI createUI(final JComponent h) {
        return new DarkTableHeaderUIBridge();
    }

//
//  Factory methods for the Listeners
//

    protected static boolean canResize(final TableColumn column,
                                       final JTableHeader header) {
        return (column != null) && header.getResizingAllowed()
                && column.getResizable();
    }

//
//  The installation/uninstall procedures and support
//

    protected void updateRolloverColumn(final MouseEvent e) {
        if (header.getDraggedColumn() == null &&
                header.contains(e.getPoint())) {

            int col = header.columnAtPoint(e.getPoint());
            if (col != rolloverColumn) {
                int oldRolloverColumn = rolloverColumn;
                rolloverColumn = col;
                rolloverColumnUpdated(oldRolloverColumn, rolloverColumn);
            }
        }
    }

    //
// Support for keyboard and mouse access
//
    protected int selectNextColumn(final boolean doIt) {
        int newIndex = getSelectedColumnIndex();
        if (newIndex < header.getColumnModel().getColumnCount() - 1) {
            newIndex++;
            if (doIt) {
                selectColumn(newIndex);
            }
        }
        return newIndex;
    }

    protected int getSelectedColumnIndex() {
        int numCols = header.getColumnModel().getColumnCount();
        if (selectedColumnIndex >= numCols && numCols > 0) {
            selectedColumnIndex = numCols - 1;
        }
        return selectedColumnIndex;
    }
    /**
     * Selects the specified column in the table header. Repaints the affected header cells and makes sure the newly
     * selected one is visible.
     */
    void selectColumn(final int newColIndex) {
        selectColumn(newColIndex, true);
    }

    /**
     * Creates the mouse listener for the {@code JTableHeader}.
     *
     * @return the mouse listener for the {@code JTableHeader}
     */
    protected MouseInputListener createMouseInputListener() {
        return new DarkTableHeaderUIBridge.MouseInputHandler();
    }

//  Installation

    /**
     * Used by selectColumn to scroll horizontally, if necessary, to ensure that the newly selected column is visible.
     */
    protected void scrollToColumn(final int col) {
        Container container;
        JTable table;

        //Test whether the header is in a scroll pane and has a table.
        if ((header.getParent() == null) ||
                ((container = header.getParent().getParent()) == null) ||
                !(container instanceof JScrollPane) ||
                ((table = header.getTable()) == null)) {
            return;
        }

        //Now scroll, if necessary.
        Rectangle vis = table.getVisibleRect();
        Rectangle cellBounds = table.getCellRect(0, col, true);
        vis.x = cellBounds.x;
        vis.width = cellBounds.width;
        table.scrollRectToVisible(vis);
    }

    void selectColumn(final int newColIndex, final boolean doScroll) {
        Rectangle repaintRect = header.getHeaderRect(selectedColumnIndex);
        header.repaint(repaintRect);
        selectedColumnIndex = newColIndex;
        repaintRect = header.getHeaderRect(newColIndex);
        header.repaint(repaintRect);
        if (doScroll) {
            scrollToColumn(newColIndex);
        }
    }

    protected static class Actions extends UIAction {
        public static final String TOGGLE_SORT_ORDER =
                "toggleSortOrder";
        public static final String SELECT_COLUMN_TO_LEFT =
                "selectColumnToLeft";
        public static final String SELECT_COLUMN_TO_RIGHT =
                "selectColumnToRight";
        public static final String MOVE_COLUMN_LEFT =
                "moveColumnLeft";
        public static final String MOVE_COLUMN_RIGHT =
                "moveColumnRight";
        public static final String RESIZE_LEFT =
                "resizeLeft";
        public static final String RESIZE_RIGHT =
                "resizeRight";
        public static final String FOCUS_TABLE =
                "focusTable";

        public Actions(final String name) {
            super(name);
        }

        @Override
        public boolean accept(final Object sender) {
            if (sender instanceof JTableHeader) {
                JTableHeader th = (JTableHeader) sender;
                TableColumnModel cm = th.getColumnModel();
                if (cm.getColumnCount() <= 0) {
                    return false;
                }

                String key = getName();
                DarkTableHeaderUIBridge ui =
                        (DarkTableHeaderUIBridge) DarkUIUtil.getUIOfType(th.getUI(), DarkTableHeaderUIBridge.class);
                if (ui != null) {
                    if (Objects.equals(key, MOVE_COLUMN_LEFT)) {
                        return th.getReorderingAllowed()
                                && maybeMoveColumn(true, th, ui, false);
                    } else if (Objects.equals(key, MOVE_COLUMN_RIGHT)) {
                        return th.getReorderingAllowed()
                                && maybeMoveColumn(false, th, ui, false);
                    } else if (Objects.equals(key, RESIZE_LEFT) ||
                            Objects.equals(key, RESIZE_RIGHT)) {
                        return canResize(cm.getColumn(ui.getSelectedColumnIndex()), th);
                    } else if (Objects.equals(key, FOCUS_TABLE)) {
                        return (th.getTable() != null);
                    }
                }
            }
            return true;
        }

        protected boolean maybeMoveColumn(final boolean leftArrow, final JTableHeader th,
                                          final DarkTableHeaderUIBridge ui, final boolean doIt) {
            int oldIndex = ui.getSelectedColumnIndex();
            int newIndex;

            if (th.getComponentOrientation().isLeftToRight()) {
                newIndex = leftArrow ? ui.selectPreviousColumn(doIt)
                                     : ui.selectNextColumn(doIt);
            } else {
                newIndex = leftArrow ? ui.selectNextColumn(doIt)
                                     : ui.selectPreviousColumn(doIt);
            }

            if (newIndex != oldIndex) {
                if (doIt) {
                    th.getColumnModel().moveColumn(oldIndex, newIndex);
                } else {
                    return true; // we'd do the move if asked
                }
            }

            return false;
        }

        public void actionPerformed(final ActionEvent e) {
            JTableHeader th = (JTableHeader) e.getSource();
            DarkTableHeaderUIBridge ui =
                    (DarkTableHeaderUIBridge) DarkUIUtil.getUIOfType(th.getUI(), DarkTableHeaderUIBridge.class);
            if (ui == null) {
                return;
            }

            String name = getName();
            if (Objects.equals(TOGGLE_SORT_ORDER, name)) {
                JTable table = th.getTable();
                RowSorter<?> sorter = table == null ? null : table.getRowSorter();
                if (sorter != null) {
                    int columnIndex = ui.getSelectedColumnIndex();
                    columnIndex = table.convertColumnIndexToModel(
                            columnIndex);
                    sorter.toggleSortOrder(columnIndex);
                }
            } else if (Objects.equals(SELECT_COLUMN_TO_LEFT, name)) {
                if (th.getComponentOrientation().isLeftToRight()) {
                    ui.selectPreviousColumn(true);
                } else {
                    ui.selectNextColumn(true);
                }
            } else if (Objects.equals(SELECT_COLUMN_TO_RIGHT, name)) {
                if (th.getComponentOrientation().isLeftToRight()) {
                    ui.selectNextColumn(true);
                } else {
                    ui.selectPreviousColumn(true);
                }
            } else if (Objects.equals(MOVE_COLUMN_LEFT, name)) {
                moveColumn(true, th, ui);
            } else if (Objects.equals(MOVE_COLUMN_RIGHT, name)) {
                moveColumn(false, th, ui);
            } else if (Objects.equals(RESIZE_LEFT, name)) {
                resize(true, th, ui);
            } else if (Objects.equals(RESIZE_RIGHT, name)) {
                resize(false, th, ui);
            } else if (Objects.equals(FOCUS_TABLE, name)) {
                JTable table = th.getTable();
                if (table != null) {
                    table.requestFocusInWindow();
                }
            }
        }

        protected void moveColumn(final boolean leftArrow, final JTableHeader th,
                                  final DarkTableHeaderUIBridge ui) {
            maybeMoveColumn(leftArrow, th, ui, true);
        }

        protected void resize(final boolean leftArrow, final JTableHeader th,
                              final DarkTableHeaderUIBridge ui) {
            int columnIndex = ui.getSelectedColumnIndex();
            TableColumn resizingColumn =
                    th.getColumnModel().getColumn(columnIndex);

            th.setResizingColumn(resizingColumn);
            int oldWidth = resizingColumn.getWidth();
            int newWidth = oldWidth;

            if (th.getComponentOrientation().isLeftToRight()) {
                newWidth = newWidth + (leftArrow ? -1 : 1);
            } else {
                newWidth = newWidth + (leftArrow ? 1 : -1);
            }

            ui.changeColumnWidth(resizingColumn, th, oldWidth, newWidth);
        }
    }
    /**
     * This class should be treated as a &quot;protected&quot; inner class. Instantiate it only within subclasses of
     * {@code DarkTableHeaderUIBridge}.
     */
    public class MouseInputHandler implements MouseInputListener {

        protected int mouseXOffset;
        protected Cursor otherCursor = resizeCursor;

        public void mouseClicked(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            if (e.getClickCount() % 2 == 1 &&
                    SwingUtilities.isLeftMouseButton(e)) {
                JTable table = header.getTable();
                RowSorter<?> sorter;
                if (table != null && (sorter = table.getRowSorter()) != null) {
                    int columnIndex = header.columnAtPoint(e.getPoint());
                    if (columnIndex != -1) {
                        columnIndex = table.convertColumnIndexToModel(
                                columnIndex);
                        sorter.toggleSortOrder(columnIndex);
                    }
                }
            }
        }

        public void mousePressed(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            header.setDraggedColumn(null);
            header.setResizingColumn(null);
            header.setDraggedDistance(0);

            Point p = e.getPoint();

            // First find which header cell was hit
            TableColumnModel columnModel = header.getColumnModel();
            int index = header.columnAtPoint(p);

            if (index != -1) {
                // The last 3 pixels + 3 pixels of next column are for resizing
                TableColumn resizingColumn = getResizingColumn(p, index);
                if (canResize(resizingColumn, header)) {
                    header.setResizingColumn(resizingColumn);
                    if (header.getComponentOrientation().isLeftToRight()) {
                        mouseXOffset = p.x - resizingColumn.getWidth();
                    } else {
                        mouseXOffset = p.x + resizingColumn.getWidth();
                    }
                } else if (header.getReorderingAllowed()) {
                    TableColumn hitColumn = columnModel.getColumn(index);
                    header.setDraggedColumn(hitColumn);
                    mouseXOffset = p.x;
                }
            }

            if (header.getReorderingAllowed()) {
                int oldRolloverColumn = rolloverColumn;
                rolloverColumn = -1;
                rolloverColumnUpdated(oldRolloverColumn, rolloverColumn);
            }
        }

        protected TableColumn getResizingColumn(final Point p, final int column) {
            if (column == -1) {
                return null;
            }
            Rectangle r = header.getHeaderRect(column);
            r.grow(-3, 0);
            if (r.contains(p)) {
                return null;
            }
            int midPoint = r.x + r.width / 2;
            int columnIndex;
            if (header.getComponentOrientation().isLeftToRight()) {
                columnIndex = (p.x < midPoint) ? column - 1 : column;
            } else {
                columnIndex = (p.x < midPoint) ? column : column - 1;
            }
            if (columnIndex == -1) {
                return null;
            }
            return header.getColumnModel().getColumn(columnIndex);
        }

        public void mouseReleased(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            setDraggedDistance(0, viewIndexForColumn(header.getDraggedColumn()));

            header.setResizingColumn(null);
            header.setDraggedColumn(null);

            updateRolloverColumn(e);
        }

        public void mouseEntered(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            updateRolloverColumn(e);
        }

        public void mouseExited(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            int oldRolloverColumn = rolloverColumn;
            rolloverColumn = -1;
            rolloverColumnUpdated(oldRolloverColumn, rolloverColumn);
        }

        protected void setDraggedDistance(final int draggedDistance, final int column) {
            header.setDraggedDistance(draggedDistance);
            if (column != -1) {
                header.getColumnModel().moveColumn(column, column);
            }
        }

        public void mouseDragged(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            int mouseX = e.getX();

            TableColumn resizingColumn = header.getResizingColumn();
            TableColumn draggedColumn = header.getDraggedColumn();

            boolean headerLeftToRight = header.getComponentOrientation().isLeftToRight();

            if (resizingColumn != null) {
                int oldWidth = resizingColumn.getWidth();
                int newWidth;
                if (headerLeftToRight) {
                    newWidth = mouseX - mouseXOffset;
                } else {
                    newWidth = mouseXOffset - mouseX;
                }
                mouseXOffset += changeColumnWidth(resizingColumn, header,
                                                  oldWidth, newWidth);
            } else if (draggedColumn != null) {
                TableColumnModel cm = header.getColumnModel();
                int draggedDistance = mouseX - mouseXOffset;
                int direction = (draggedDistance < 0) ? -1 : 1;
                int columnIndex = viewIndexForColumn(draggedColumn);
                int newColumnIndex = columnIndex + (headerLeftToRight ? direction : -direction);
                if (0 <= newColumnIndex && newColumnIndex < cm.getColumnCount()) {
                    int width = cm.getColumn(newColumnIndex).getWidth();
                    if (Math.abs(draggedDistance) > (width / 2)) {

                        mouseXOffset = mouseXOffset + direction * width;
                        header.setDraggedDistance(draggedDistance - direction * width);

                        //Cache the selected column.
                        int selectedIndex = SwingUtilities2.convertColumnIndexToModel(header.getColumnModel(),
                                                                                      getSelectedColumnIndex());

                        //Now do the move.
                        cm.moveColumn(columnIndex, newColumnIndex);

                        //Update the selected index.
                        selectColumn(SwingUtilities2.convertColumnIndexToView(header.getColumnModel(), selectedIndex),
                                     false);

                        return;
                    }
                }
                setDraggedDistance(draggedDistance, columnIndex);
            }

            updateRolloverColumn(e);
        }

        public void mouseMoved(final MouseEvent e) {
            if (!header.isEnabled()) {
                return;
            }
            if (canResize(getResizingColumn(e.getPoint()), header) !=
                    (header.getCursor() == resizeCursor)) {
                swapCursor();
            }
            updateRolloverColumn(e);
        }

        protected TableColumn getResizingColumn(final Point p) {
            return getResizingColumn(p, header.columnAtPoint(p));
        }
//
// Protected & protected Methods
//

        protected void swapCursor() {
            Cursor tmp = header.getCursor();
            header.setCursor(otherCursor);
            otherCursor = tmp;
        }
    }

    public void installUI(final JComponent c) {
        super.installUI(c);
    }

    protected int selectPreviousColumn(final boolean doIt) {
        int newIndex = getSelectedColumnIndex();
        if (newIndex > 0) {
            newIndex--;
            if (doIt) {
                selectColumn(newIndex);
            }
        }
        return newIndex;
    }

    protected int changeColumnWidth(final TableColumn resizingColumn,
                                    final JTableHeader th,
                                    final int oldWidth, final int newWidth) {
        resizingColumn.setWidth(newWidth);

        Container container;
        JTable table;

        if ((th.getParent() == null) ||
                ((container = th.getParent().getParent()) == null) ||
                !(container instanceof JScrollPane) ||
                ((table = th.getTable()) == null)) {
            return 0;
        }

        if (!container.getComponentOrientation().isLeftToRight() &&
                !th.getComponentOrientation().isLeftToRight()) {
            JViewport viewport = ((JScrollPane) container).getViewport();
            int viewportWidth = viewport.getWidth();
            int diff = newWidth - oldWidth;
            int newHeaderWidth = table.getWidth() + diff;

            /* Resize a table */
            Dimension tableSize = table.getSize();
            tableSize.width += diff;
            table.setSize(tableSize);

            /* If this table is in AUTO_RESIZE_OFF mode and
             * has a horizontal scrollbar, we need to update
             * a view's position.
             */
            if ((newHeaderWidth >= viewportWidth) &&
                    (table.getAutoResizeMode() == JTable.AUTO_RESIZE_OFF)) {
                Point p = viewport.getViewPosition();
                p.x = Math.max(0, Math.min(newHeaderWidth - viewportWidth,
                                           p.x + diff));
                viewport.setViewPosition(p);
                return diff;
            }
        }
        return 0;
    }

    /**
     * Initializes JTableHeader properties such as font, foreground, and background. The font, foreground, and
     * background properties are only set if their current value is either null or a UIResource, other properties are
     * set if the current value is null.
     *
     * @see #installUI
     */
    protected void installDefaults() {
        LookAndFeel.installColorsAndFont(header, "TableHeader.background",
                                         "TableHeader.foreground", "TableHeader.font");
        LookAndFeel.installProperty(header, "opaque", Boolean.TRUE);
    }


    /**
     * Attaches listeners to the JTableHeader.
     */
    protected void installListeners() {
        mouseInputListener = createMouseInputListener();

        header.addMouseListener(mouseInputListener);
        header.addMouseMotionListener(mouseInputListener);
        header.addFocusListener(focusListener);
    }

// Uninstall methods





    public void uninstallUI(final JComponent c) {
        uninstallDefaults();
        uninstallListeners();
        uninstallKeyboardActions();

        header.remove(rendererPane);
        rendererPane = null;
        header = null;
    }


    /**
     * Uninstalls default properties
     */
    protected void uninstallDefaults() {
    }


    /**
     * Unregisters listeners.
     */
    protected void uninstallListeners() {
        header.removeMouseListener(mouseInputListener);
        header.removeMouseMotionListener(mouseInputListener);
        header.removeFocusListener(focusListener);

        mouseInputListener = null;
    }


    /**
     * Unregisters default key actions.
     */
    protected void uninstallKeyboardActions() {
        SwingUtilities.replaceUIInputMap(header, JComponent.WHEN_FOCUSED, null);
        SwingUtilities.replaceUIActionMap(header, null);
    }

//
// Support for mouse rollover
//


    /**
     * Returns the index of the column header over which the mouse currently is. When the mouse is not over the table
     * header, -1 is returned.
     *
     * @return the index of the current rollover column
     * @see #rolloverColumnUpdated(int, int)
     * @since 1.6
     */
    protected int getRolloverColumn() {
        return rolloverColumn;
    }


    /**
     * This method gets called every time when a rollover column in the table header is updated. Every look and feel
     * that supports a rollover effect in a table header should override this method and repaint the header.
     *
     * @param oldColumn the index of the previous rollover column or -1 if the mouse was not over a column
     * @param newColumn the index of the new rollover column or -1 if the mouse is not over a column
     * @see #getRolloverColumn()
     * @see JTableHeader#getHeaderRect(int)
     * @since 1.6
     */
    protected void rolloverColumnUpdated(final int oldColumn, final int newColumn) {
    }


    /**
     * Returns the baseline.
     *
     * @throws NullPointerException     {@inheritDoc}
     * @throws IllegalArgumentException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public int getBaseline(final JComponent c, final int width, final int height) {
        super.getBaseline(c, width, height);
        int baseline = -1;
        TableColumnModel columnModel = header.getColumnModel();
        for (int column = 0; column < columnModel.getColumnCount();
             column++) {
            TableColumn aColumn = columnModel.getColumn(column);
            Component comp = getHeaderRenderer(column);
            Dimension pref = comp.getPreferredSize();
            int columnBaseline = comp.getBaseline(pref.width, height);
            if (columnBaseline >= 0) {
                if (baseline == -1) {
                    baseline = columnBaseline;
                } else if (baseline != columnBaseline) {
                    baseline = -1;
                    break;
                }
            }
        }
        return baseline;
    }

//
// Baseline
//

    public void paint(final Graphics g, final JComponent c) {
        if (header.getColumnModel().getColumnCount() <= 0) {
            return;
        }
        boolean ltr = header.getComponentOrientation().isLeftToRight();

        Rectangle clip = g.getClipBounds();
        Point left = clip.getLocation();
        Point right = new Point(clip.x + clip.width - 1, clip.y);
        TableColumnModel cm = header.getColumnModel();
        int cMin = header.columnAtPoint(ltr ? left : right);
        int cMax = header.columnAtPoint(ltr ? right : left);
        // This should never happen.
        if (cMin == -1) {
            cMin = 0;
        }
        // If the table does not have enough columns to fill the view we'll get -1.
        // Replace this with the index of the last column.
        if (cMax == -1) {
            cMax = cm.getColumnCount() - 1;
        }

        TableColumn draggedColumn = header.getDraggedColumn();
        int columnWidth;
        Rectangle cellRect = header.getHeaderRect(ltr ? cMin : cMax);
        TableColumn aColumn;
        if (ltr) {
            for (int column = cMin; column <= cMax; column++) {
                aColumn = cm.getColumn(column);
                columnWidth = aColumn.getWidth();
                cellRect.width = columnWidth;
                if (aColumn != draggedColumn) {
                    paintCell(g, cellRect, column);
                }
                cellRect.x += columnWidth;
            }
        } else {
            for (int column = cMax; column >= cMin; column--) {
                aColumn = cm.getColumn(column);
                columnWidth = aColumn.getWidth();
                cellRect.width = columnWidth;
                if (aColumn != draggedColumn) {
                    paintCell(g, cellRect, column);
                }
                cellRect.x += columnWidth;
            }
        }

        // Paint the dragged column if we are dragging.
        if (draggedColumn != null) {
            int draggedColumnIndex = viewIndexForColumn(draggedColumn);
            Rectangle draggedCellRect = header.getHeaderRect(draggedColumnIndex);

            // Draw a gray well in place of the moving column.
            g.setColor(header.getParent().getBackground());
            g.fillRect(draggedCellRect.x, draggedCellRect.y,
                       draggedCellRect.width, draggedCellRect.height);

            draggedCellRect.x += header.getDraggedDistance();

            // Fill the background.
            g.setColor(header.getBackground());
            g.fillRect(draggedCellRect.x, draggedCellRect.y,
                       draggedCellRect.width, draggedCellRect.height);

            paintCell(g, draggedCellRect, draggedColumnIndex);
        }

        // Remove all components in the rendererPane.
        rendererPane.removeAll();
    }

//
// Paint Methods and support
//

    protected Component getHeaderRenderer(final int columnIndex) {
        TableColumn aColumn = header.getColumnModel().getColumn(columnIndex);
        TableCellRenderer renderer = aColumn.getHeaderRenderer();
        if (renderer == null) {
            renderer = header.getDefaultRenderer();
        }

        boolean hasFocus = !header.isPaintingForPrint()
                && (columnIndex == getSelectedColumnIndex())
                && header.hasFocus();
        return renderer.getTableCellRendererComponent(header.getTable(),
                                                      aColumn.getHeaderValue(),
                                                      false, hasFocus,
                                                      -1, columnIndex);
    }

    protected void paintCell(final Graphics g, final Rectangle cellRect, final int columnIndex) {
        Component component = getHeaderRenderer(columnIndex);
        rendererPane.paintComponent(g, component, header, cellRect.x, cellRect.y,
                                    cellRect.width, cellRect.height, true);
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

    protected int getHeaderHeight() {
        int height = 0;
        boolean accomodatedDefault = false;
        TableColumnModel columnModel = header.getColumnModel();
        for (int column = 0; column < columnModel.getColumnCount(); column++) {
            TableColumn aColumn = columnModel.getColumn(column);
            boolean isDefault = (aColumn.getHeaderRenderer() == null);

            if (!isDefault || !accomodatedDefault) {
                Component comp = getHeaderRenderer(column);
                int rendererHeight = comp.getPreferredSize().height;
                height = Math.max(height, rendererHeight);

                // Configuring the header renderer to calculate its preferred size
                // is expensive. Optimise this by assuming the default renderer
                // always has the same height as the first non-zero height that
                // it returns for a non-null/non-empty value.
                if (isDefault && rendererHeight > 0) {
                    Object headerValue = aColumn.getHeaderValue();
                    if (headerValue != null) {
                        headerValue = headerValue.toString();

                        if (headerValue != null && !headerValue.equals("")) {
                            accomodatedDefault = true;
                        }
                    }
                }
            }
        }
        return height;
    }

//
// Size Methods
//

    protected Dimension createHeaderSize(long width) {
        // None of the callers include the intercell spacing, do it here.
        if (width > Integer.MAX_VALUE) {
            width = Integer.MAX_VALUE;
        }
        return new Dimension((int) width, getHeaderHeight());
    }

    /**
     * Return the minimum size of the header. The minimum width is the sum of the minimum widths of each column (plus
     * inter-cell spacing).
     */
    public Dimension getMinimumSize(final JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getMinWidth();
        }
        return createHeaderSize(width);
    }

    /**
     * Return the preferred size of the header. The preferred height is the maximum of the preferred heights of all of
     * the components provided by the header renderers. The preferred width is the sum of the preferred widths of each
     * column (plus inter-cell spacing).
     */
    public Dimension getPreferredSize(final JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getPreferredWidth();
        }
        return createHeaderSize(width);
    }

    /**
     * Return the maximum size of the header. The maximum width is the sum of the maximum widths of each column (plus
     * inter-cell spacing).
     */
    public Dimension getMaximumSize(final JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = header.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getMaxWidth();
        }
        return createHeaderSize(width);
    }


}
