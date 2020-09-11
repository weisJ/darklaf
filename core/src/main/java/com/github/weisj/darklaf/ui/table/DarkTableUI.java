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
package com.github.weisj.darklaf.ui.table;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.util.Objects;
import java.util.function.Supplier;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.table.*;

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererPane;
import com.github.weisj.darklaf.ui.table.renderer.DarkColorTableCellRendererEditor;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellEditorDelegate;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellRenderer;
import com.github.weisj.darklaf.ui.table.renderer.DarkTableCellRendererDelegate;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public class DarkTableUI extends DarkTableUIBridge implements TableConstants {

    private static final int ROW_HEIGHT_FALLBACK = 22;
    protected Color selectionBackgroundNoFocus;
    protected Color selectionBackground;
    protected Color borderColor;
    protected Handler handler;

    protected DarkTableCellRendererDelegate rendererDelegate;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkTableUI();
    }

    protected boolean pointOutsidePrefSize(final int row, final int column, final Point p) {
        return false;
    }

    @Override
    protected FocusListener createFocusListener() {
        return getHandler();
    }

    @Override
    protected KeyListener createKeyListener() {
        return getHandler();
    }

    @Override
    protected Handler getHandler() {
        if (handler == null) {
            handler = new DarkHandler(super.createKeyListener(), super.createFocusListener());
        }
        return handler;
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
        Container oldUnwrapped = DarkUIUtil.getUnwrappedParent(table.getParent());
        LookAndFeel.uninstallBorder((JComponent) oldUnwrapped);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        int rowHeight = UIManager.getInt("Table.rowHeight");
        if (rowHeight > 0) {
            LookAndFeel.installProperty(table, "rowHeight", ROW_HEIGHT_FALLBACK);
        }
        PropertyUtil.installBooleanProperty(table, KEY_RENDER_BOOLEAN_AS_CHECKBOX, "Table.renderBooleanAsCheckBox");
        PropertyUtil.installBooleanProperty(table, KEY_ALTERNATE_ROW_COLOR, "Table.alternateRowColor");
        PropertyUtil.installBooleanProperty(table, "terminateEditOnFocusLost", "Table.terminateEditOnFocusLost");
        PropertyUtil.installProperty(table, KEY_BOOLEAN_RENDER_TYPE, UIManager.getString("Table.booleanRenderType"));

        setupRendererComponents(table);

        borderColor = UIManager.getColor("TableHeader.borderColor");
        selectionBackground = UIManager.getColor("Table.backgroundSelected");
        selectionBackgroundNoFocus = UIManager.getColor("Table.backgroundSelectedNoFocus");
        rendererPane = createCellRendererPane();
        table.setSurrendersFocusOnKeystroke(true);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        handler = null;
    }

    protected CellRendererPane createCellRendererPane() {
        return new DarkCellRendererPane();
    }

    protected static void setupRendererComponents(final JTable table) {
        TableCellRenderer cellRenderer = new DarkTableCellRenderer();
        TableCellEditor cellEditor = new DarkTableCellEditorDelegate();
        DarkColorTableCellRendererEditor colorRendererEditor = new DarkColorTableCellRendererEditor();

        table.setDefaultRenderer(Object.class, cellRenderer);
        table.setDefaultRenderer(Color.class, colorRendererEditor);

        table.setDefaultEditor(Object.class, cellEditor);
        table.setDefaultEditor(Number.class, cellEditor);
        table.setDefaultEditor(Boolean.class, cellEditor);
        table.setDefaultEditor(Color.class, colorRendererEditor);
    }

    @Override
    protected void paintGrid(final Graphics g, final int rMin, final int rMax, final int cMin, final int cMax) {
        g.setColor(table.getGridColor());

        Rectangle minCell = table.getCellRect(rMin, cMin, true);
        Rectangle maxCell = table.getCellRect(rMax, cMax, true);
        Rectangle damagedArea = minCell.union(maxCell);

        JTableHeader header = table.getTableHeader();
        int draggedIndex = -1;
        if (header != null) {
            draggedIndex = viewIndexForColumn(header.getDraggedColumn());
        }
        if (table.getShowHorizontalLines()) {
            int tableWidth = damagedArea.x + damagedArea.width;
            if (table.getComponentOrientation().isLeftToRight()) {
                if (draggedIndex >= 0 && draggedIndex == cMax) {
                    tableWidth -= 1;
                }
            } else {
                if (draggedIndex >= 0 && draggedIndex == cMin) {
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
            int tableHeight = getPreferredSize(table).height;
            int x;
            boolean ltr = table.getComponentOrientation().isLeftToRight();
            x = damagedArea.x;
            if (scrollPaneRtl && scrollBarVisible()) {
                SwingUtilities2.drawVLine(g, x, 0, tableHeight - 1);
            }
            if (ltr) {
                for (int column = cMin; column <= cMax; column++) {
                    int w = cm.getColumn(column).getWidth();
                    x += w;
                    if (showVerticalLine(true, scrollVisible, !scrollPaneRtl, column, draggedIndex, cMin, cMax)) {
                        SwingUtilities2.drawVLine(g, x - 1, 0, tableHeight - 1);
                    }
                }
            } else {
                for (int column = cMax; column >= cMin; column--) {
                    int w = cm.getColumn(column).getWidth();
                    x += w;
                    if (showVerticalLine(false, scrollVisible, !scrollPaneRtl, column, draggedIndex, cMin, cMax)) {
                        SwingUtilities2.drawVLine(g, x - 1, 0, tableHeight - 1);
                    }
                }
            }
        }
    }

    protected boolean isScrollPaneRtl() {
        if (!isInScrollPane()) return false;
        Container comp = DarkUIUtil.getUnwrappedParent(table).getParent();
        return !comp.getComponentOrientation().isLeftToRight();
    }

    protected boolean scrollBarVisible() {
        JScrollPane comp = DarkUIUtil.getParentOfType(JScrollPane.class, table, 2);
        if (comp == null) return false;
        OverlayScrollPane overlayScrollPane = DarkUIUtil.getParentOfType(OverlayScrollPane.class, table, 3);
        return overlayScrollPane == null && comp.getVerticalScrollBar().isVisible();
    }

    protected boolean showVerticalLine(
            final boolean ltr, final boolean scrollVisible, final boolean scrollLtR, final int column,
            final int draggedIndex, final int cMin, final int cMax
    ) {
        JTableHeader header = table.getTableHeader();
        int dist = header != null
            ? adjustDistance(header.getDraggedDistance(), table.getCellRect(0, draggedIndex, true), table)
            : 0;
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

    protected boolean isInScrollPane() {
        Container comp = DarkUIUtil.getUnwrappedParent(table);
        if (comp != null) {
            comp = comp.getParent();
        }
        return comp instanceof JScrollPane;
    }

    @Override
    protected void paintDraggedArea(
            final Graphics g, final int rMin, final int rMax, final int cMin, final int cMax,
            final TableColumn draggedColumn, final int distance
    ) {
        int draggedColumnIndex = viewIndexForColumn(draggedColumn);

        Rectangle minCell = table.getCellRect(rMin, draggedColumnIndex, true);
        Rectangle maxCell = table.getCellRect(rMax, draggedColumnIndex, true);

        Rectangle vacatedColumnRect = minCell.union(maxCell);

        int dist = adjustDistance(distance, vacatedColumnRect, table);

        // Paint a gray well in place of the moving column.
        Container parent = table.getParent();
        if (isInScrollPane()) {
            Component par = DarkUIUtil.getUnwrappedParent(table).getParent();
            if (par != null && par.getParent() != null) {
                parent = par.getParent();
            }
        }
        int tableHeight = getPreferredSize(table).height;
        g.setColor(parent.getBackground());
        int width = vacatedColumnRect.width;
        if (draggedColumnIndex < cMax) width--;

        g.fillRect(vacatedColumnRect.x, 0, width, tableHeight);

        // Move to the where the cell has been dragged.
        vacatedColumnRect.x += dist;

        g.setColor(Color.RED);
        PaintUtil.drawRect(g, vacatedColumnRect);

        boolean ltr = table.getComponentOrientation().isLeftToRight();

        // Fill the background.
        g.setColor(table.getBackground());
        g.fillRect(vacatedColumnRect.x, 0, vacatedColumnRect.width, tableHeight);

        // Paint the vertical grid lines if necessary.
        if (table.getShowVerticalLines()) {
            g.setColor(table.getGridColor());
            int x1 = vacatedColumnRect.x;
            int y1 = 0;
            int x2 = x1 + vacatedColumnRect.width - 1;
            int y2 = y1 + tableHeight;

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

            if (table.getShowHorizontalLines()) {
                g.setColor(table.getGridColor());
                Rectangle rcr = table.getCellRect(row, draggedColumnIndex, true);
                rcr.x += dist;
                int x1 = rcr.x;
                int y1 = rcr.y;
                int x2 = x1 + rcr.width;
                int y2 = y1 + rcr.height - 1;
                g.fillRect(x1, y2, x2 - x1, 1);
            }
        }
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        /*
         * JTable always subtracts the cell margins even if for the last column. This results in part of the
         * cell not being repainted. Dispatch repaint here manually.
         */
        if (table.getShowVerticalLines()) {
            Rectangle r = g.getClipBounds();
            int spacing = table.getColumnModel().getColumnMargin();
            if (r.x + r.width == c.getWidth() - spacing) {
                r.x = r.x + r.width;
                r.width = spacing;
                c.repaint(r);
            }
        }
        super.paint(g, c);
    }

    public static boolean ignoreKeyCodeOnEdit(final KeyEvent event, final JTable table) {
        if (event != null) {
            int keyCode = event.getKeyCode();
            switch (keyCode) {
                case KeyEvent.VK_ALT_GRAPH:
                case KeyEvent.VK_META:
                case KeyEvent.VK_CAPS_LOCK:
                case KeyEvent.VK_HOME:
                case KeyEvent.VK_WINDOWS:
                case KeyEvent.VK_CONTEXT_MENU:
                case KeyEvent.VK_PRINTSCREEN:
                case KeyEvent.VK_NUM_LOCK:
                case KeyEvent.VK_SCROLL_LOCK:
                case KeyEvent.VK_CLEAR:
                case KeyEvent.VK_HELP:
                case KeyEvent.VK_INSERT:
                case KeyEvent.VK_ESCAPE:
                    return true;
                default:
                    break;
            }
            if (table == null) return false;
            KeyStroke stroke = KeyStroke.getKeyStroke(event.getExtendedKeyCode(), event.getModifiersEx());
            String actionName =
                String.valueOf(table.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).get(stroke));
            if ("null".equals(actionName)) actionName = null;
            String cutActionName = String.valueOf(TransferHandler.getCutAction().getValue(Action.NAME));
            String copyActionName = String.valueOf(TransferHandler.getCopyAction().getValue(Action.NAME));
            return Objects.equals(actionName, copyActionName) || Objects.equals(actionName, cutActionName);
        }
        return false;
    }

    protected void paintCell(
            final Graphics g, final Rectangle r, final int row, final int column, final int cMin, final int cMax
    ) {
        // if (true) return;
        boolean isEditorCell = table.isEditing() && table.getEditingRow() == row && table.getEditingColumn() == column;

        int x = r.x;
        int y = r.y;
        int w = r.width;
        int h = r.height;

        if (table.getShowVerticalLines() && !scrollBarVisible()) {
            if (column == table.getColumnCount() - 1) {
                w++;
            }
        }

        if (isEditorCell) {
            if (!table.getShowVerticalLines()) {
                if (column > cMin) x--;
                if (column > cMin && column < cMax) w++;
            }
        }
        if (isEditorCell) {
            Component component = table.getEditorComponent();
            component.setBounds(x, y, w, h);
            component.validate();
        } else {
            TableCellRenderer renderer = getCellRenderer(row, column);
            Component component = table.prepareRenderer(renderer, row, column);
            CellUtil.setSelectedFlag(component, table.isCellSelected(row, column));
            rendererPane.paintComponent(g, component, table, x, y, w, h, true);
        }
    }

    protected TableCellRenderer getCellRenderer(final int row, final int column) {
        TableCellRenderer renderer = table.getCellRenderer(row, column);
        if (renderer instanceof DarkTableCellRendererDelegate) return renderer;
        if (rendererDelegate == null) {
            rendererDelegate = new DarkTableCellRendererDelegate(renderer);
        }
        rendererDelegate.setDelegate(renderer);
        return rendererDelegate;
    }

    public static int adjustDistance(final int distance, final Rectangle rect, final JTable comp) {
        int dist = distance;
        int min = 0;
        int max = comp.getWidth();
        if (rect.x + dist <= min) {
            dist = min - rect.x;
        }
        if (rect.x + rect.width + dist >= max) {
            dist = max - rect.x - rect.width;
        }
        return dist;
    }

    protected class DarkHandler extends Handler {

        protected int lastIndex = -1;

        protected DarkHandler(final KeyListener keyListener, final FocusListener focusListener) {
            super(keyListener, focusListener);
        }

        @Override
        public void mouseClicked(final MouseEvent e) {
            super.mouseClicked(e);
            if (isFileList && SwingUtilities.isLeftMouseButton(e)) {
                int row = table.rowAtPoint(e.getPoint());
                JFileChooser fc = getFileChooser();
                if (row < 0 || fc == null) return;
                int column = getFileNameColumnIndex();
                boolean isSelected = table.getSelectionModel().getLeadSelectionIndex() == row
                    && table.getColumnModel().getSelectionModel().getLeadSelectionIndex() == column;
                if (
                    (!fc.isMultiSelectionEnabled() || fc.getSelectedFiles().length <= 1) && isSelected
                        && lastIndex == row && DarkUIUtil.isOverText(e, row, column, table)
                ) {
                    startEditing(row, column);
                } else {
                    lastIndex = row;
                }
            }
        }

        protected JFileChooser getFileChooser() {
            Object obj =
                PropertyUtil.getObject(table, DarkTableUI.KEY_FILE_CHOOSER_PARENT, Supplier.class, Object::new).get();
            return obj instanceof JFileChooser ? (JFileChooser) obj : null;
        }

        protected Integer getFileNameColumnIndex() {
            return PropertyUtil.getInteger(table, DarkTableUI.KEY_FILENAME_COLUMN_INDEX);
        }

        protected void startEditing(final int row, final int column) {
            table.editCellAt(row, column, null);
            Component editorComponent = table.getEditorComponent();
            if (editorComponent != null && !editorComponent.hasFocus()) {
                SwingUtilities2.compositeRequestFocus(editorComponent);
            }
        }

        @Override
        protected void maybeStartTimer() {}

        @Override
        public void actionPerformed(final ActionEvent ae) {}

        @Override
        public void keyTyped(final KeyEvent e) {
            if (ignoreKeyCodeOnEdit(e, table)) {
                e.consume();
                return;
            }
            super.keyTyped(e);
        }

        @Override
        public void propertyChange(final PropertyChangeEvent e) {
            super.propertyChange(e);
            String key = e.getPropertyName();
            if (KEY_HORIZONTAL_LINES.equals(key)) {
                boolean b = Boolean.TRUE.equals(e.getNewValue());
                table.setRowMargin(b ? 1 : 0);
            } else if (KEY_VERTICAL_LINES.equals(key)) {
                boolean b = Boolean.TRUE.equals(e.getNewValue());
                TableColumnModel cm = table.getColumnModel();
                cm.setColumnMargin(b ? 1 : 0);
            } else if (PropertyKey.ANCESTOR.equals(key)) {
                Object oldVal = e.getOldValue();
                Object newVal = e.getNewValue();
                if (oldVal instanceof Component) {
                    Container oldUnwrapped = DarkUIUtil.getUnwrappedParent((Component) oldVal);
                    LookAndFeel.uninstallBorder((JComponent) oldUnwrapped);
                }
                if (newVal instanceof Component) {
                    Container newUnwrapped = DarkUIUtil.getUnwrappedParent((Component) newVal);
                    if ((newUnwrapped instanceof JScrollPane)) {
                        LookAndFeel.installBorder((JComponent) newUnwrapped, "Table.scrollPaneBorder");
                    }
                }
                checkFocus();
            } else if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
                table.doLayout();
                table.repaint();
            } else if (
                KEY_ALTERNATE_ROW_COLOR.equals(key) || KEY_RENDER_BOOLEAN_AS_CHECKBOX.equals(key)
                    || KEY_BOOLEAN_RENDER_TYPE.equals(key)
            ) {
                table.repaint();
            } else if (PropertyKey.ENABLED.equals(key)) {
                DarkUIUtil.repaint(table.getTableHeader());
            }
        }

        @Override
        public void focusGained(final FocusEvent e) {
            Color bg = table.getSelectionBackground();
            if (bg instanceof UIResource) {
                table.setSelectionBackground(selectionBackground);
            }
            Component comp = table.getEditorComponent();
            if (comp != null) {
                Color newBg = CellUtil.getTableBackground(comp, table, true, true, table.getEditingRow());
                CellUtil.setupBackground(comp, newBg);
            }
            table.repaint();
        }

        @Override
        public void focusLost(final FocusEvent e) {
            Color bg = table.getSelectionBackground();
            if (bg instanceof UIResource) {
                if (table.isEditing()) {
                    table.setSelectionBackground(table.getBackground());
                } else {
                    table.setSelectionBackground(selectionBackgroundNoFocus);
                }
            }
            Component comp = table.getEditorComponent();
            if (comp != null && !DarkUIUtil.hasFocus(comp, e)) {
                Color newBg = CellUtil.getTableBackground(comp, table, true, false, table.getEditingRow());
                CellUtil.setupBackground(comp, newBg);
            }
            table.repaint();
        }

        protected void checkFocus() {
            boolean focus = DarkUIUtil.hasFocus(table);
            if (focus) {
                focusGained(null);
            } else {
                focusLost(null);
            }
        }
    }
}
