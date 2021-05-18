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
package com.github.weisj.darklaf.ui.table;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTableUI;
import javax.swing.table.DefaultTableCellRenderer;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableColumn;

import sun.swing.DefaultLookup;
import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.ui.DragRecognitionSupport;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.SwingUtil;

/** The type Table ui bridge. */
public abstract class TableUIBridge extends BasicTableUI {

    /** The constant BASELINE_COMPONENT_KEY. */
    protected static final StringBuilder BASELINE_COMPONENT_KEY = new StringBuilder("Table.baselineComponent");

    //
    // Instance Variables
    //

    //
    // Helper class for keyboard actions
    //
    /** Local cache of Table's client property DarkTableUI.KEY_IS_FILE_LIST */
    protected boolean isFileList = false;

    //
    // The Table's Key listener
    //

    //
    // The Table's focus listener
    //

    //
    // The Table's mouse and mouse motion listeners
    //

    /**
     * Gets adjusted lead.
     *
     * @param table the table
     * @param row the row
     * @return the adjusted lead
     */
    protected static int getAdjustedLead(final JTable table, final boolean row) {
        return row ? getAdjustedLead(table, row, table.getSelectionModel())
                : getAdjustedLead(table, row, table.getColumnModel().getSelectionModel());
    }

    /**
     * Gets adjusted lead.
     *
     * @param table the table
     * @param row the row
     * @param model the model
     * @return the adjusted lead
     */
    protected static int getAdjustedLead(final JTable table, final boolean row, final ListSelectionModel model) {
        int index = model.getLeadSelectionIndex();
        int compare = row ? table.getRowCount() : table.getColumnCount();
        return index < compare ? index : -1;
    }

    /**
     * Point outside pref size boolean.
     *
     * @param row the row
     * @param column the column
     * @param p the p
     * @return the boolean
     */
    /*
     * Returns true if the given point is outside the preferredSize of the item at the given row of the
     * table. (Column must be 0). Returns false if the DarkTableUI.KEY_IS_FILE_LIST client property is
     * not set.
     */
    protected boolean pointOutsidePrefSize(final int row, final int column, final Point p) {
        if (!isFileList) {
            return false;
        }

        return SwingUtilities2.pointOutsidePrefSize(table, row, column, p);
    }

    //
    // Factory methods for the Listeners
    //

    public void installUI(final JComponent c) {
        table = (JTable) c;
        super.installUI(c);
        table.remove(rendererPane);
        rendererPane = new CellRendererPane();
        table.add(rendererPane);
    }

    /**
     * Initialize JTable properties, e.g. font, foreground, and background. The font, foreground, and
     * background properties are only set if their current value is either null or a UIResource, other
     * properties are set if the current value is null.
     *
     * @see #installUI #installUI
     */
    protected void installDefaults() {
        super.installDefaults();
        isFileList = PropertyUtil.getBooleanProperty(table, DarkTableUI.KEY_IS_FILE_LIST);
    }

    /** Attaches listeners to the JTable. */
    protected void installListeners() {
        super.installListeners();
        mouseInputListener = createMouseInputListener();
        table.addPropertyChangeListener(getHandler());
    }

    //
    // The installation/uninstall procedures and support
    //

    // Installation

    /**
     * Creates the mouse listener for the {@code JTable}.
     *
     * @return the mouse listener for the {@code JTable}
     */
    protected MouseInputListener createMouseInputListener() {
        return getHandler();
    }

    /**
     * Gets handlerF.
     *
     * @return the handler
     */
    protected abstract Handler getHandler();

    /**
     * Gets input map.
     *
     * @param condition the condition
     * @return the input map
     */
    InputMap getInputMap(final int condition) {
        if (condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) {
            InputMap keyMap = (InputMap) DefaultLookup.get(table, this, "Table.ancestorInputMap");
            InputMap rtlKeyMap;

            if (table.getComponentOrientation().isLeftToRight() || ((rtlKeyMap =
                    (InputMap) DefaultLookup.get(table, this, "Table.ancestorInputMap.RightToLeft")) == null)) {
                return keyMap;
            } else {
                rtlKeyMap.setParent(keyMap);
                return rtlKeyMap;
            }
        }
        return null;
    }

    public void uninstallUI(final JComponent c) {
        uninstallDefaults();
        uninstallListeners();
        uninstallKeyboardActions();

        table.remove(rendererPane);
        rendererPane = null;
        table = null;
    }

    /** Uninstalls default properties. */
    protected void uninstallDefaults() {
        if (table.getTransferHandler() instanceof UIResource) {
            table.setTransferHandler(null);
        }
    }

    // Uninstallation

    /** Unregisters listeners. */
    protected void uninstallListeners() {
        table.removeFocusListener(focusListener);
        table.removeKeyListener(keyListener);
        table.removeMouseListener(mouseInputListener);
        table.removeMouseMotionListener(mouseInputListener);
        table.removePropertyChangeListener(getHandler());
        if (isFileList) {
            table.getSelectionModel().removeListSelectionListener(getHandler());
        }

        focusListener = null;
        keyListener = null;
        mouseInputListener = null;
    }

    /** Unregisters keyboard actions. */
    protected void uninstallKeyboardActions() {
        SwingUtilities.replaceUIInputMap(table, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
        SwingUtilities.replaceUIActionMap(table, null);
    }

    /** Paint a representation of the <code>table</code> instance that was set in installUI(). */
    public abstract void paint(final Graphics g, final JComponent c);

    /**
     * Return the preferred size of the table. The preferred height is the row height times the number
     * of rows. The preferred width is the sum of the preferred widths of each column.
     */
    public Dimension getPreferredSize(final JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = table.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getPreferredWidth();
        }
        return createTableSize(width);
    }

    /**
     * Return the minimum size of the table. The minimum height is the row height times the number of
     * rows. The minimum width is the sum of the minimum widths of each column.
     */
    public Dimension getMinimumSize(final JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = table.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getMinWidth();
        }
        return createTableSize(width);
    }

    /**
     * Return the maximum size of the table. The maximum height is the row heighttimes the number of
     * rows. The maximum width is the sum of the maximum widths of each column.
     */
    public Dimension getMaximumSize(final JComponent c) {
        long width = 0;
        Enumeration<TableColumn> enumeration = table.getColumnModel().getColumns();
        while (enumeration.hasMoreElements()) {
            TableColumn aColumn = enumeration.nextElement();
            width = width + aColumn.getMaxWidth();
        }
        return createTableSize(width);
    }

    //
    // Size Methods
    //

    /**
     * Returns the baseline.
     *
     * @throws NullPointerException {@inheritDoc}
     * @throws IllegalArgumentException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public int getBaseline(final JComponent c, final int width, final int height) {
        super.getBaseline(c, width, height);
        UIDefaults lafDefaults = UIManager.getLookAndFeelDefaults();
        Component renderer = (Component) lafDefaults.get(BASELINE_COMPONENT_KEY);
        if (renderer == null) {
            DefaultTableCellRenderer tcr = new DefaultTableCellRenderer();
            renderer = tcr.getTableCellRendererComponent(table, "a", false, false, -1, -1);
            lafDefaults.put(BASELINE_COMPONENT_KEY, renderer);
        }
        renderer.setFont(table.getFont());
        int rowMargin = table.getRowMargin();
        return renderer.getBaseline(Integer.MAX_VALUE, table.getRowHeight() - rowMargin) + rowMargin / 2;
    }

    /**
     * Returns an enum indicating how the baseline of the component changes as the size changes.
     *
     * @throws NullPointerException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(final JComponent c) {
        super.getBaselineResizeBehavior(c);
        return Component.BaselineResizeBehavior.CONSTANT_ASCENT;
    }

    /**
     * Create table size dimension.
     *
     * @param width the width
     * @return the dimension
     */
    protected Dimension createTableSize(final long width) {
        int height = 0;
        int rowCount = table.getRowCount();
        if (rowCount > 0 && table.getColumnCount() > 0) {
            Rectangle r = table.getCellRect(rowCount - 1, 0, true);
            height = r.y + r.height;
        }
        // Width is always positive. The call to abs() is a workaround for
        // a bug in the 1.1.6 JIT on Windows.
        long tmp = Math.abs(width);
        if (tmp > Integer.MAX_VALUE) {
            tmp = Integer.MAX_VALUE;
        }
        return new Dimension((int) tmp, height);
    }

    /**
     * Paint drop lines.
     *
     * @param g the g
     */
    protected abstract void paintDropLines(final Graphics g);

    //
    // Paint methods and support
    //

    /**
     * Gets h drop line rect.
     *
     * @param loc the loc
     * @return the h drop line rect
     */
    protected Rectangle getHDropLineRect(final JTable.DropLocation loc) {
        if (!loc.isInsertRow()) {
            return null;
        }

        int row = loc.getRow();
        int col = loc.getColumn();
        if (col >= table.getColumnCount()) {
            col--;
        }

        Rectangle rect = table.getCellRect(row, col, true);

        if (row >= table.getRowCount()) {
            row--;
            Rectangle prevRect = table.getCellRect(row, col, true);
            rect.y = prevRect.y + prevRect.height;
        }

        if (rect.y == 0) {
            rect.y = -1;
        } else {
            rect.y -= 2;
        }

        rect.height = 3;

        return rect;
    }

    /**
     * Extend rect rectangle.
     *
     * @param rect the rect
     * @param horizontal the horizontal
     * @return the rectangle
     */
    protected Rectangle extendRect(final Rectangle rect, final boolean horizontal) {
        if (rect == null) {
            return rect;
        }

        if (horizontal) {
            rect.x = 0;
            rect.width = table.getWidth();
        } else {
            rect.y = 0;

            if (table.getRowCount() != 0) {
                Rectangle lastRect = table.getCellRect(table.getRowCount() - 1, 0, true);
                rect.height = lastRect.y + lastRect.height;
            } else {
                rect.height = table.getHeight();
            }
        }

        return rect;
    }

    /**
     * Gets v drop line rect.
     *
     * @param loc the loc
     * @return the v drop line rect
     */
    protected Rectangle getVDropLineRect(final JTable.DropLocation loc) {
        if (!loc.isInsertColumn()) {
            return null;
        }

        boolean ltr = table.getComponentOrientation().isLeftToRight();
        int col = loc.getColumn();
        Rectangle rect = table.getCellRect(loc.getRow(), col, true);

        if (col >= table.getColumnCount()) {
            col--;
            rect = table.getCellRect(loc.getRow(), col, true);
            if (ltr) {
                rect.x = rect.x + rect.width;
            }
        } else if (!ltr) {
            rect.x = rect.x + rect.width;
        }

        if (rect.x == 0) {
            rect.x = -1;
        } else {
            rect.x -= 2;
        }

        rect.width = 3;

        return rect;
    }

    /**
     * Paint grid.
     *
     * @param g the g
     * @param rMin the r min
     * @param rMax the r max
     * @param cMin the c min
     * @param cMax the c max
     */
    /*
     * Paints the grid lines within <I>aRect</I>, using the grid color set with <I>setGridColor</I>.
     * Paints vertical lines if <code>getShowVerticalLines()</code> returns true and paints horizontal
     * lines if <code>getShowHorizontalLines()</code> returns true.
     */
    protected abstract void paintGrid(final Graphics g, final int rMin, final int rMax, final int cMin, final int cMax);

    /**
     * Paint cells.
     *
     * @param g the g
     * @param rMin the r min
     * @param rMax the r max
     * @param cMin the c min
     * @param cMax the c max
     */
    protected abstract void paintCells(final Graphics g, final int rMin, final int rMax, final int cMin,
            final int cMax);

    /**
     * Paint cell.
     *
     * @param g the g
     * @param cellRect the cell rect
     * @param row the row
     * @param column the column
     */
    protected abstract void paintCell(final Graphics g, final Rectangle cellRect, final int row, final int column);

    /**
     * View index for column int.
     *
     * @param aColumn the a column
     * @return the int
     */
    protected abstract int viewIndexForColumn(final TableColumn aColumn);

    /** The type Handler. */
    protected class Handler implements FocusListener, MouseInputListener, PropertyChangeListener, ListSelectionListener,
            ActionListener, DragRecognitionSupport.BeforeDrag, KeyListener {

        protected final FocusListener focusListener;
        protected final KeyListener keyListener;

        /** The Dispatch component. */
        // Component receiving mouse events during editing.
        // May not be editorComponent.
        protected Component dispatchComponent;
        /** The Pressed row. */
        // The row and column where the press occurred and the
        // press event itself
        protected int pressedRow;
        /** The Pressed col. */
        protected int pressedCol;
        /** The Pressed event. */
        protected MouseEvent pressedEvent;
        /** The Drag press did selection. */
        // Whether or not the mouse press (which is being considered as part
        // of a drag sequence) also caused the selection change to be fully
        // processed.
        protected boolean dragPressDidSelection;
        /** The Drag started. */
        // Set to true when a drag gesture has been fully recognized and DnD
        // begins. Use this to ignore further mouse events which could be
        // delivered if DnD is cancelled (via ESCAPE for example)
        protected boolean dragStarted;

        /** The Should start timer. */
        // MouseInputListener
        // Whether or not we should start the editing timer on release
        protected boolean shouldStartTimer;
        /** The Outside pref size. */
        // To cache the return value of pointOutsidePrefSize since we use
        // it multiple times.
        protected boolean outsidePrefSize;
        /** The Timer. */
        // Used to delay the start of editing.
        protected Timer timer = null;

        protected Handler(final KeyListener keyListener, final FocusListener focusListener) {
            this.keyListener = keyListener;
            this.focusListener = focusListener;
        }

        @Override
        public void focusGained(final FocusEvent e) {
            if (focusListener != null) focusListener.focusGained(e);
        }

        @Override
        public void focusLost(final FocusEvent e) {
            if (focusListener != null) focusListener.focusLost(e);
        }

        @Override
        public void keyPressed(final KeyEvent e) {
            if (keyListener != null) keyListener.keyPressed(e);
        }

        @Override
        public void keyReleased(final KeyEvent e) {
            if (keyListener != null) keyListener.keyReleased(e);
        }

        @Override
        public void keyTyped(final KeyEvent e) {
            if (keyListener != null) keyListener.keyTyped(e);
        }

        public void mouseClicked(final MouseEvent e) {}

        public void mousePressed(final MouseEvent e) {
            if (SwingUtil.shouldIgnore(e, table)) {
                return;
            }

            if (table.isEditing() && !table.getCellEditor().stopCellEditing()) {
                Component editorComponent = table.getEditorComponent();
                if (editorComponent != null && !editorComponent.hasFocus()) {
                    SwingUtilities2.compositeRequestFocus(editorComponent);
                }
                return;
            }

            Point p = e.getPoint();
            pressedRow = table.rowAtPoint(p);
            pressedCol = table.columnAtPoint(p);
            outsidePrefSize = pointOutsidePrefSize(pressedRow, pressedCol, p);

            if (isFileList) {
                shouldStartTimer = table.isCellSelected(pressedRow, pressedCol) && !e.isShiftDown()
                        && !DarkUIUtil.isMenuShortcutKeyDown(e) && !outsidePrefSize;
            }

            if (table.getDragEnabled()) {
                mousePressedDND(e);
            } else {
                SwingUtilities2.adjustFocus(table);
                if (!isFileList) {
                    setValueIsAdjusting(true);
                }
                adjustSelection(e);
            }
        }

        public void mouseReleased(final MouseEvent e) {
            if (SwingUtil.shouldIgnore(e, table)) {
                return;
            }

            if (table.getDragEnabled()) {
                mouseReleasedDND(e);
            } else {
                if (isFileList) {
                    maybeStartTimer();
                }
            }

            pressedEvent = null;
            repostEvent(e);
            dispatchComponent = null;
            setValueIsAdjusting(false);
        }

        /**
         * Mouse released dnd.
         *
         * @param e the e
         */
        protected void mouseReleasedDND(final MouseEvent e) {
            MouseEvent me = DragRecognitionSupport.mouseReleased(e);
            if (me != null) {
                SwingUtilities2.adjustFocus(table);
                if (!dragPressDidSelection) {
                    adjustSelection(me);
                }
            }

            if (!dragStarted) {
                if (isFileList) {
                    maybeStartTimer();
                    return;
                }

                Point p = e.getPoint();

                if (pressedEvent != null && table.rowAtPoint(p) == pressedRow && table.columnAtPoint(p) == pressedCol
                        && table.editCellAt(pressedRow, pressedCol, pressedEvent)) {

                    setDispatchComponent(pressedEvent);
                    repostEvent(pressedEvent);

                    // This may appear completely odd, but must be done for backward
                    // compatibility reasons. Developers have been known to rely on
                    // a call to shouldSelectCell after editing has begun.
                    CellEditor ce = table.getCellEditor();
                    if (ce != null) {
                        ce.shouldSelectCell(pressedEvent);
                    }
                }
            }
        }

        /**
         * Adjust selection.
         *
         * @param e the e
         */
        protected void adjustSelection(final MouseEvent e) {
            // Fix for 4835633
            if (outsidePrefSize) {
                // If shift is down in multi-select, we should just return.
                // For single select or non-shift-click, clear the selection
                if (e.getID() == MouseEvent.MOUSE_PRESSED && (!e.isShiftDown()
                        || table.getSelectionModel().getSelectionMode() == ListSelectionModel.SINGLE_SELECTION)) {
                    table.clearSelection();
                    TableCellEditor tce = table.getCellEditor();
                    if (tce != null) {
                        tce.stopCellEditing();
                    }
                }
                return;
            }
            // The autoscroller can generate drag events outside the
            // table's range.
            if ((pressedCol == -1) || (pressedRow == -1)) {
                return;
            }

            boolean dragEnabled = table.getDragEnabled();

            if (!dragEnabled && !isFileList && table.editCellAt(pressedRow, pressedCol, e)) {
                setDispatchComponent(e);
                repostEvent(e);
            }

            CellEditor editor = table.getCellEditor();
            if (dragEnabled || editor == null || editor.shouldSelectCell(e)) {
                table.changeSelection(pressedRow, pressedCol, DarkUIUtil.isMenuShortcutKeyDown(e), e.isShiftDown());
            }
        }

        /** Maybe start timer. */
        protected void maybeStartTimer() {
            if (!shouldStartTimer) {
                return;
            }

            if (timer == null) {
                timer = new Timer(1200, this);
                timer.setRepeats(false);
            }

            timer.start();
        }

        /**
         * Sets dispatch component.
         *
         * @param e the e
         */
        protected void setDispatchComponent(final MouseEvent e) {
            Component editorComponent = table.getEditorComponent();
            Point p = e.getPoint();
            Point p2 = SwingUtilities.convertPoint(table, p, editorComponent);
            dispatchComponent = SwingUtilities.getDeepestComponentAt(editorComponent, p2.x, p2.y);
            SwingUtilities2.setSkipClickCount(dispatchComponent, e.getClickCount() - 1);
        }

        /**
         * Repost event boolean.
         *
         * @param e the e
         * @return the boolean
         */
        protected boolean repostEvent(final MouseEvent e) {
            // Check for isEditing() in case another event has
            // caused the editor to be removed. See bug #4306499.
            if (dispatchComponent == null || !table.isEditing()) {
                return false;
            }
            MouseEvent e2 = SwingUtilities.convertMouseEvent(table, e, dispatchComponent);
            dispatchComponent.dispatchEvent(e2);
            return true;
        }

        /**
         * Mouse pressed dnd.
         *
         * @param e the e
         */
        protected void mousePressedDND(final MouseEvent e) {
            pressedEvent = e;
            boolean grabFocus = true;
            dragStarted = false;

            if (canStartDrag() && DragRecognitionSupport.mousePressed(e)) {

                dragPressDidSelection = false;

                if (DarkUIUtil.isMenuShortcutKeyDown(e) && isFileList) {
                    // do nothing for control - will be handled on release
                    // or when drag starts
                    return;
                } else if (!e.isShiftDown() && table.isCellSelected(pressedRow, pressedCol)) {
                    // clicking on something that's already selected
                    // and need to make it the lead now
                    table.getSelectionModel().addSelectionInterval(pressedRow, pressedRow);
                    table.getColumnModel().getSelectionModel().addSelectionInterval(pressedCol, pressedCol);
                    return;
                }

                dragPressDidSelection = true;

                // could be a drag initiating event - don't grab focus
                grabFocus = false;
            } else if (!isFileList) {
                // When drag can't happen, mouse drags might change the selection in the table
                // so we want the isAdjusting flag to be set
                setValueIsAdjusting(true);
            }

            if (grabFocus) {
                SwingUtilities2.adjustFocus(table);
            }

            adjustSelection(e);
        }

        public void mouseEntered(final MouseEvent e) {}

        public void mouseExited(final MouseEvent e) {}

        /**
         * Can start drag boolean.
         *
         * @return the boolean
         */
        protected boolean canStartDrag() {
            if (pressedRow == -1 || pressedCol == -1) {
                return false;
            }

            if (isFileList) {
                return !outsidePrefSize;
            }

            // if this is a single selection table
            if ((table.getSelectionModel().getSelectionMode() == ListSelectionModel.SINGLE_SELECTION) && (table
                    .getColumnModel().getSelectionModel().getSelectionMode() == ListSelectionModel.SINGLE_SELECTION)) {

                return true;
            }

            return table.isCellSelected(pressedRow, pressedCol);
        }

        /**
         * Sets value is adjusting.
         *
         * @param flag the flag
         */
        protected void setValueIsAdjusting(final boolean flag) {
            table.getSelectionModel().setValueIsAdjusting(flag);
            table.getColumnModel().getSelectionModel().setValueIsAdjusting(flag);
        }

        public void valueChanged(final ListSelectionEvent e) {
            if (timer != null) {
                timer.stop();
                timer = null;
            }
        }

        public void actionPerformed(final ActionEvent ae) {
            table.editCellAt(pressedRow, pressedCol, null);
            Component editorComponent = table.getEditorComponent();
            if (editorComponent != null && !editorComponent.hasFocus()) {
                SwingUtilities2.compositeRequestFocus(editorComponent);
            }
        }

        public void dragStarting(final MouseEvent me) {
            dragStarted = true;

            if (DarkUIUtil.isMenuShortcutKeyDown(me) && isFileList) {
                table.getSelectionModel().addSelectionInterval(pressedRow, pressedRow);
                table.getColumnModel().getSelectionModel().addSelectionInterval(pressedCol, pressedCol);
            }

            pressedEvent = null;
        }

        public void mouseDragged(final MouseEvent e) {
            if (SwingUtil.shouldIgnore(e, table)) {
                return;
            }

            if (table.getDragEnabled() && (DragRecognitionSupport.mouseDragged(e, this) || dragStarted)) {
                return;
            }

            repostEvent(e);

            // Check isFileList:
            // Until we support drag-selection, dragging should not change
            // the selection (act like single-select).
            if (isFileList || table.isEditing()) {
                return;
            }

            Point p = e.getPoint();
            int row = table.rowAtPoint(p);
            int column = table.columnAtPoint(p);
            // The autoscroller can generate drag events outside the
            // table's range.
            if ((column == -1) || (row == -1)) {
                return;
            }

            table.changeSelection(row, column, DarkUIUtil.isMenuShortcutKeyDown(e), true);
        }

        public void mouseMoved(final MouseEvent e) {}

        // PropertyChangeListener
        public void propertyChange(final PropertyChangeEvent event) {
            String changeName = event.getPropertyName();
            if (DarkTableUI.KEY_IS_FILE_LIST.equals(changeName)) {
                isFileList = PropertyUtil.getBooleanProperty(table, DarkTableUI.KEY_IS_FILE_LIST);
            }
        }
    }
}
