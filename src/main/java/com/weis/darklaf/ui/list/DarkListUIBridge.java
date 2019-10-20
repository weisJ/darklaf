package com.weis.darklaf.ui.list;

import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.LazyActionMap;
import org.jdesktop.swingx.plaf.basic.core.BasicTransferable;
import org.jdesktop.swingx.plaf.basic.core.DragRecognitionSupport;
import sun.swing.DefaultLookup;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicListUI;
import javax.swing.text.Position;
import java.awt.*;
import java.awt.datatransfer.Transferable;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.geom.Point2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Objects;

/**
 * An extensible implementation of {@code ListUI}.
 * <p>
 * {@code BasicListUI} instances cannot be shared between multiple lists.
 *
 * @author Hans Muller
 * @author Philip Milne
 * @author Shannon Hickey (drag and drop)
 */

public class DarkListUIBridge extends BasicListUI {

    protected static final StringBuilder BASELINE_COMPONENT_KEY =
            new StringBuilder("List.baselineComponent");
    /**
     * The bit relates to model changed property.
     */
    protected static final int modelChanged = 1 << 0;
    /**
     * The bit relates to selection model changed property.
     */
    protected static final int selectionModelChanged = 1 << 1;

    // Listeners that this UI attaches to the JList
    /**
     * The bit relates to font changed property.
     */
    protected static final int fontChanged = 1 << 2;
    /**
     * The bit relates to fixed cell width changed property.
     */
    protected static final int fixedCellWidthChanged = 1 << 3;
    /**
     * The bit relates to fixed cell height changed property.
     */
    protected static final int fixedCellHeightChanged = 1 << 4;
    /**
     * The bit relates to prototype cell value changed property.
     */
    protected static final int prototypeCellValueChanged = 1 << 5;
    /**
     * The bit relates to cell renderer changed property.
     */
    protected static final int cellRendererChanged = 1 << 6;
    protected static final int layoutOrientationChanged = 1 << 7;
    protected static final int heightChanged = 1 << 8;
    protected static final int widthChanged = 1 << 9;
    protected static final int componentOrientationChanged = 1 << 10;
    protected static final int DROP_LINE_THICKNESS = 2;
    /**
     * Used by IncrementLeadSelectionAction. Indicates the action should change the lead, and not select it.
     */
    protected static final int CHANGE_LEAD = 0;
    /**
     * Used by IncrementLeadSelectionAction. Indicates the action should change the selection and lead.
     */
    protected static final int CHANGE_SELECTION = 1;
    /**
     * Used by IncrementLeadSelectionAction. Indicates the action should extend the selection from the anchor to the
     * next index.
     */
    protected static final int EXTEND_SELECTION = 2;

    // Following ivars are used if the list is laying out horizontally
    protected static final TransferHandler defaultTransferHandler = new ListTransferHandler();
    /**
     * The instance of {@code JList}.
     */
    protected JList<Object> list = null;
    /**
     * The instance of {@code CellRendererPane}.
     */
    protected CellRendererPane rendererPane;
    /**
     * {@code FocusListener} that attached to {@code JList}.
     */
    protected FocusListener focusListener;
    /**
     * {@code MouseInputListener} that attached to {@code JList}.
     */
    protected MouseInputListener mouseInputListener;
    /**
     * {@code ListSelectionListener} that attached to {@code JList}.
     */
    protected ListSelectionListener listSelectionListener;

    /* The bits below define JList property changes that affect layout.
     * When one of these properties changes we set a bit in
     * updateLayoutStateNeeded.  The change is dealt with lazily, see
     * maybeUpdateLayoutState.  Changes to the JLists model, e.g. the
     * models length changed, are handled similarly, see DataListener.
     */
    /**
     * {@code ListDataListener} that attached to {@code JList}.
     */
    protected ListDataListener listDataListener;
    /**
     * {@code PropertyChangeListener} that attached to {@code JList}.
     */
    protected PropertyChangeListener propertyChangeListener;
    protected Handler handler;
    /**
     * The array of cells' height
     */
    protected int[] cellHeights = null;
    /**
     * The height of cell.
     */
    protected int cellHeight = -1;
    /**
     * The width of cell.
     */
    protected int cellWidth = -1;
    /**
     * The value represents changes to {@code JList} model.
     */
    protected int updateLayoutStateNeeded = modelChanged;
    /**
     * Height of the list. When asked to paint, if the current size of the list differs, this will update the layout
     * state.
     */
    protected int listHeight;
    /**
     * Width of the list. When asked to paint, if the current size of the list differs, this will update the layout
     * state.
     */
    protected int listWidth;
    /**
     * The layout orientation of the list.
     */
    protected int layoutOrientation;
    /**
     * Number of columns to create.
     */
    protected int columnCount;
    /**
     * Preferred height to make the list, this is only used if the the list is layed out horizontally.
     */
    protected int preferredHeight;
    /**
     * Number of rows per column. This is only used if the row height is fixed.
     */
    protected int rowsPerColumn;
    /**
     * The time factor to treate the series of typed alphanumeric key as prefix for first letter navigation.
     */
    protected long timeFactor = 1000L;
    /**
     * Local cache of JList's client property "List.isFileList"
     */
    protected boolean isFileList = false;
    /**
     * Local cache of JList's component orientation property
     */
    protected boolean isLeftToRight = true;

    /**
     * Returns a new instance of {@code BasicListUI}. {@code BasicListUI} delegates are allocated one per {@code
     * JList}.
     *
     * @param list a component
     * @return a new {@code ListUI} implementation for the Windows look and feel.
     */
    public static ComponentUI createUI(final JComponent list) {
        return new BasicListUI();
    }

    protected static int adjustIndex(final int index, final JList<?> list) {
        return index < list.getModel().getSize() ? index : -1;
    }

    /**
     * Paint one List cell: compute the relevant state, get the "rubber stamp" cell renderer component, and then use the
     * {@code CellRendererPane} to paint it. Subclasses may want to override this method rather than {@code paint()}.
     *
     * @param g            an instance of {@code Graphics}
     * @param row          a row
     * @param rowBounds    a bounding rectangle to render to
     * @param cellRenderer a list of {@code ListCellRenderer}
     * @param dataModel    a list model
     * @param selModel     a selection model
     * @param leadIndex    a lead index
     * @see #paint
     */
    protected void paintCell(
            final Graphics g,
            final int row,
            final Rectangle rowBounds,
            final ListCellRenderer<Object> cellRenderer,
            final ListModel<Object> dataModel,
            final ListSelectionModel selModel,
            final int leadIndex) {
        Object value = dataModel.getElementAt(row);
        boolean cellHasFocus = list.hasFocus() && (row == leadIndex);
        boolean isSelected = selModel.isSelectedIndex(row);

        Component rendererComponent =
                cellRenderer.getListCellRendererComponent(list, value, row, isSelected, cellHasFocus);

        int cx = rowBounds.x;
        int cy = rowBounds.y;
        int cw = rowBounds.width;
        int ch = rowBounds.height;

        if (isFileList) {
            // Shrink renderer to preferred size. This is mostly used on Windows
            // where selection is only shown around the file name, instead of
            // across the whole list cell.
            int w = Math.min(cw, rendererComponent.getPreferredSize().width + 4);
            if (!isLeftToRight) {
                cx += (cw - w);
            }
            cw = w;
        }

        rendererPane.paintComponent(g, rendererComponent, list, cx, cy, cw, ch, true);
    }

    /**
     * Paint the rows that intersect the Graphics objects clipRect.  This method calls paintCell as necessary.
     * Subclasses may want to override these methods.
     *
     * @see #paintCell
     */
    public void paint(final Graphics g, final JComponent c) {
        Shape clip = g.getClip();
        paintImpl(g, c);
        g.setClip(clip);

        paintDropLine(g);
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
        int rowHeight = list.getFixedCellHeight();
        UIDefaults lafDefaults = UIManager.getLookAndFeelDefaults();
        Component renderer = (Component) lafDefaults.get(
                BASELINE_COMPONENT_KEY);
        if (renderer == null) {
            @SuppressWarnings("unchecked")
            ListCellRenderer<Object> lcr = (ListCellRenderer) UIManager.get(
                    "List.cellRenderer");

            // fix for 6711072 some LAFs like Nimbus do not provide this
            // UIManager key and we should not through a NPE here because of it
            if (lcr == null) {
                lcr = new DefaultListCellRenderer();
            }
            renderer = lcr.getListCellRendererComponent(
                    list, "a", -1, false, false);
            lafDefaults.put(BASELINE_COMPONENT_KEY, renderer);
        }
        renderer.setFont(list.getFont());
        // JList actually has much more complex behavior here.
        // If rowHeight != -1 the rowHeight is either the max of all cell
        // heights (layout orientation != VERTICAL), or is variable depending
        // upon the cell.  We assume a default size.
        // We could theoretically query the real renderer, but that would
        // not work for an empty model and the results may vary with
        // the content.
        if (rowHeight == -1) {
            rowHeight = renderer.getPreferredSize().height;
        }
        return renderer.getBaseline(Integer.MAX_VALUE, rowHeight) +
                list.getInsets().top;
    }

    /**
     * Returns an enum indicating how the baseline of the component changes as the size changes.
     *
     * @throws NullPointerException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(
            final JComponent c) {
        super.getBaselineResizeBehavior(c);
        return Component.BaselineResizeBehavior.CONSTANT_ASCENT;
    }

    /**
     * The preferredSize of the list depends upon the layout orientation.
     *
     * <table class="striped">
     * <caption>Describes the preferred size for each layout orientation
     * </caption>
     * <thead>
     *   <tr>
     *     <th scope="col">Layout Orientation
     *     <th scope="col">Preferred Size
     * </thead>
     * <tbody>
     *   <tr>
     *     <th scope="row">JList.VERTICAL
     *     <td>The preferredSize of the list is total height of the rows
     *     and the maximum width of the cells. If JList.fixedCellHeight
     *     is specified then the total height of the rows is just
     *     (cellVerticalMargins + fixedCellHeight) * model.getSize() where
     *     rowVerticalMargins is the space we allocate for drawing
     *     the yellow focus outline. Similarly if fixedCellWidth is
     *     specified then we just use that.
     *   <tr>
     *     <th scope="row">JList.VERTICAL_WRAP
     *     <td>If the visible row count is greater than zero, the preferredHeight
     *     is the maximum cell height * visibleRowCount. If the visible row
     *     count is &lt;= 0, the preferred height is either the current height
     *     of the list, or the maximum cell height, whichever is
     *     bigger. The preferred width is than the maximum cell width *
     *     number of columns needed. Where the number of columns needs is
     *     list.height / max cell height. Max cell height is either the fixed
     *     cell height, or is determined by iterating through all the cells
     *     to find the maximum height from the ListCellRenderer.
     *   <tr>
     *     <th scope="row">JList.HORIZONTAL_WRAP
     *     <td>If the visible row count is greater than zero, the preferredHeight
     *     is the maximum cell height * adjustedRowCount. Where
     *     visibleRowCount is used to determine the number of columns.
     *     Because this lays out horizontally the number of rows is
     *     then determined from the column count. For example, lets say
     *     you have a model with 10 items and the visible row count is 8.
     *     The number of columns needed to display this is 2, but you no
     *     longer need 8 rows to display this, you only need 5, thus
     *     the adjustedRowCount is 5.
     * <p>
     *     If the visible row count is &lt;= 0, the preferred height is dictated
     *     by the number of columns, which will be as many as can fit in the
     *     width of the {@code JList} (width / max cell width), with at least
     *     one column. The preferred height then becomes the model size / number
     *     of columns * maximum cell height. Max cell height is either the fixed
     *     cell height, or is determined by iterating through all the cells to
     *     find the maximum height from the ListCellRenderer.
     * </tbody>
     * </table>
     * <p>
     * The above specifies the raw preferred width and height. The resulting
     * preferred width is the above width + insets.left + insets.right and
     * the resulting preferred height is the above height + insets.top +
     * insets.bottom. Where the <code>Insets</code> are determined from
     * <code>list.getInsets()</code>.
     *
     * @param c The JList component.
     * @return The total size of the list.
     */
    public Dimension getPreferredSize(final JComponent c) {
        maybeUpdateLayoutState();

        int lastRow = list.getModel().getSize() - 1;
        if (lastRow < 0) {
            return new Dimension(0, 0);
        }

        Insets insets = list.getInsets();
        int width = cellWidth * columnCount + insets.left + insets.right;
        int height;

        if (layoutOrientation != JList.VERTICAL) {
            height = preferredHeight;
        } else {
            Rectangle bounds = getCellBounds(list, lastRow);

            if (bounds != null) {
                height = bounds.y + bounds.height + insets.bottom;
            } else {
                height = 0;
            }
        }
        return new Dimension(width, height);
    }

    /**
     * Selected the previous row and force it to be visible.
     *
     * @see JList#ensureIndexIsVisible
     */
    protected void selectPreviousIndex() {
        int s = list.getSelectedIndex();
        if (s > 0) {
            s -= 1;
            list.setSelectedIndex(s);
            list.ensureIndexIsVisible(s);
        }
    }

    /**
     * Selected the previous row and force it to be visible.
     *
     * @see JList#ensureIndexIsVisible
     */
    protected void selectNextIndex() {
        int s = list.getSelectedIndex();
        if ((s + 1) < list.getModel().getSize()) {
            s += 1;
            list.setSelectedIndex(s);
            list.ensureIndexIsVisible(s);
        }
    }

    /**
     * Registers the keyboard bindings on the <code>JList</code> that the
     * <code>BasicListUI</code> is associated with. This method is called at
     * installUI() time.
     *
     * @see #installUI
     */
    protected void installKeyboardActions() {
        InputMap inputMap = getInputMap(JComponent.WHEN_FOCUSED);
        SwingUtilities.replaceUIInputMap(list, JComponent.WHEN_FOCUSED, inputMap);
        LazyActionMap.installLazyActionMap(list, BasicListUI.class, "List.actionMap");
    }

    /**
     * Unregisters keyboard actions installed from
     * <code>installKeyboardActions</code>.
     * This method is called at uninstallUI() time - subclassess should ensure that all of the keyboard actions
     * registered at installUI time are removed here.
     *
     * @see #installUI
     */
    protected void uninstallKeyboardActions() {
        SwingUtilities.replaceUIActionMap(list, null);
        SwingUtilities.replaceUIInputMap(list, JComponent.WHEN_FOCUSED, null);
    }

    /**
     * Creates and installs the listeners for the JList, its model, and its selectionModel.  This method is called at
     * installUI() time.
     *
     * @see #installUI
     * @see #uninstallListeners
     */
    protected void installListeners() {
        TransferHandler th = list.getTransferHandler();
        if (th == null || th instanceof UIResource) {
            list.setTransferHandler(defaultTransferHandler);
            // default TransferHandler doesn't support drop
            // so we don't want drop handling
            if (list.getDropTarget() instanceof UIResource) {
                list.setDropTarget(null);
            }
        }

        focusListener = createFocusListener();
        mouseInputListener = createMouseInputListener();
        propertyChangeListener = createPropertyChangeListener();
        listSelectionListener = createListSelectionListener();
        listDataListener = createListDataListener();

        list.addFocusListener(focusListener);
        list.addMouseListener(mouseInputListener);
        list.addMouseMotionListener(mouseInputListener);
        list.addPropertyChangeListener(propertyChangeListener);
        list.addKeyListener(getHandler());

        ListModel<Object> model = list.getModel();
        if (model != null) {
            model.addListDataListener(listDataListener);
        }

        ListSelectionModel selectionModel = list.getSelectionModel();
        if (selectionModel != null) {
            selectionModel.addListSelectionListener(listSelectionListener);
        }
    }

    /**
     * Removes the listeners from the JList, its model, and its selectionModel.  All of the listener fields, are reset
     * to null here.  This method is called at uninstallUI() time, it should be kept in sync with installListeners.
     *
     * @see #uninstallUI
     * @see #installListeners
     */
    protected void uninstallListeners() {
        list.removeFocusListener(focusListener);
        list.removeMouseListener(mouseInputListener);
        list.removeMouseMotionListener(mouseInputListener);
        list.removePropertyChangeListener(propertyChangeListener);
        list.removeKeyListener(getHandler());

        ListModel<Object> model = list.getModel();
        if (model != null) {
            model.removeListDataListener(listDataListener);
        }

        ListSelectionModel selectionModel = list.getSelectionModel();
        if (selectionModel != null) {
            selectionModel.removeListSelectionListener(listSelectionListener);
        }

        focusListener = null;
        mouseInputListener = null;
        listSelectionListener = null;
        listDataListener = null;
        propertyChangeListener = null;
        handler = null;
    }

    /**
     * Initializes list properties such as font, foreground, and background, and adds the CellRendererPane. The font,
     * foreground, and background properties are only set if their current value is either null or a UIResource, other
     * properties are set if the current value is null.
     *
     * @see #uninstallDefaults
     * @see #installUI
     * @see CellRendererPane
     */
    protected void installDefaults() {
        list.setLayout(null);

        LookAndFeel.installBorder(list, "List.border");

        LookAndFeel.installColorsAndFont(list, "List.background", "List.foreground", "List.font");

        LookAndFeel.installProperty(list, "opaque", Boolean.TRUE);

        if (list.getCellRenderer() == null) {
            @SuppressWarnings("unchecked")
            ListCellRenderer<Object> tmp = (ListCellRenderer) (UIManager.get("List.cellRenderer"));
            list.setCellRenderer(tmp);
        }

        Color sbg = list.getSelectionBackground();
        if (sbg == null || sbg instanceof UIResource) {
            list.setSelectionBackground(UIManager.getColor("List.selectionBackground"));
        }

        Color sfg = list.getSelectionForeground();
        if (sfg == null || sfg instanceof UIResource) {
            list.setSelectionForeground(UIManager.getColor("List.selectionForeground"));
        }

        Long l = (Long) UIManager.get("List.timeFactor");
        timeFactor = (l != null) ? l : 1000L;

        updateIsFileList();
    }

    /**
     * Sets the list properties that have not been explicitly overridden to {@code null}. A property is considered
     * overridden if its current value is not a {@code UIResource}.
     *
     * @see #installDefaults
     * @see #uninstallUI
     * @see CellRendererPane
     */
    protected void uninstallDefaults() {
        LookAndFeel.uninstallBorder(list);
        if (list.getFont() instanceof UIResource) {
            list.setFont(null);
        }
        if (list.getForeground() instanceof UIResource) {
            list.setForeground(null);
        }
        if (list.getBackground() instanceof UIResource) {
            list.setBackground(null);
        }
        if (list.getSelectionBackground() instanceof UIResource) {
            list.setSelectionBackground(null);
        }
        if (list.getSelectionForeground() instanceof UIResource) {
            list.setSelectionForeground(null);
        }
        if (list.getCellRenderer() instanceof UIResource) {
            list.setCellRenderer(null);
        }
        if (list.getTransferHandler() instanceof UIResource) {
            list.setTransferHandler(null);
        }
    }

    /**
     * Initializes <code>this.list</code> by calling <code>installDefaults()</code>,
     * <code>installListeners()</code>, and <code>installKeyboardActions()</code>
     * in order.
     *
     * @see #installDefaults
     * @see #installListeners
     * @see #installKeyboardActions
     */
    public void installUI(final JComponent c) {
        @SuppressWarnings("unchecked")
        JList<Object> tmp = (JList) c;
        list = tmp;

        layoutOrientation = list.getLayoutOrientation();

        rendererPane = new CellRendererPane();
        list.add(rendererPane);

        columnCount = 1;

        updateLayoutStateNeeded = modelChanged;
        isLeftToRight = list.getComponentOrientation().isLeftToRight();

        installDefaults();
        installListeners();
        installKeyboardActions();
    }

    protected void updateIsFileList() {
        boolean b = Boolean.TRUE.equals(list.getClientProperty("List.isFileList"));
        if (b != isFileList) {
            isFileList = b;
            Font oldFont = list.getFont();
            if (oldFont == null || oldFont instanceof UIResource) {
                Font newFont = UIManager.getFont(b ? "FileChooser.listFont" : "List.font");
                if (newFont != null && newFont != oldFont) {
                    list.setFont(newFont);
                }
            }
        }
    }

    protected Handler getHandler() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    InputMap getInputMap(final int condition) {
        if (condition == JComponent.WHEN_FOCUSED) {
            InputMap keyMap = (InputMap) DefaultLookup.get(
                    list, this, "List.focusInputMap");
            InputMap rtlKeyMap;

            if (isLeftToRight ||
                    ((rtlKeyMap = (InputMap) DefaultLookup.get(list, this,
                                                               "List.focusInputMap.RightToLeft")) == null)) {
                return keyMap;
            } else {
                rtlKeyMap.setParent(keyMap);
                return rtlKeyMap;
            }
        }
        return null;
    }

    /**
     * Uninitializes <code>this.list</code> by calling <code>uninstallListeners()</code>,
     * <code>uninstallKeyboardActions()</code>, and <code>uninstallDefaults()</code>
     * in order.  Sets this.list to null.
     *
     * @see #uninstallListeners
     * @see #uninstallKeyboardActions
     * @see #uninstallDefaults
     */
    public void uninstallUI(final JComponent c) {
        uninstallListeners();
        uninstallDefaults();
        uninstallKeyboardActions();

        cellWidth = cellHeight = -1;
        cellHeights = null;

        listWidth = listHeight = -1;

        list.remove(rendererPane);
        rendererPane = null;
        list = null;
    }

    /**
     * {@inheritDoc}
     *
     * @throws NullPointerException {@inheritDoc}
     */
    public int locationToIndex(final JList<?> list, final Point location) {
        maybeUpdateLayoutState();
        return convertLocationToModel(location.x, location.y);
    }

    /**
     * {@inheritDoc}
     */
    public Point indexToLocation(final JList<?> list, final int index) {
        maybeUpdateLayoutState();
        Rectangle rect = getCellBounds(list, index, index);

        if (rect != null) {
            return new Point(rect.x, rect.y);
        }
        return null;
    }

    /**
     * {@inheritDoc}
     */
    public Rectangle getCellBounds(final JList<?> list, final int index1, final int index2) {
        maybeUpdateLayoutState();

        int minIndex = Math.min(index1, index2);
        int maxIndex = Math.max(index1, index2);

        if (minIndex >= list.getModel().getSize()) {
            return null;
        }

        Rectangle minBounds = getCellBounds(list, minIndex);

        if (minBounds == null) {
            return null;
        }
        if (minIndex == maxIndex) {
            return minBounds;
        }
        Rectangle maxBounds = getCellBounds(list, maxIndex);

        if (maxBounds != null) {
            if (layoutOrientation == JList.HORIZONTAL_WRAP) {
                int minRow = convertModelToRow(minIndex);
                int maxRow = convertModelToRow(maxIndex);

                if (minRow != maxRow) {
                    minBounds.x = 0;
                    minBounds.width = list.getWidth();
                }
            } else if (minBounds.x != maxBounds.x) {
                // Different columns
                minBounds.y = 0;
                minBounds.height = list.getHeight();
            }
            minBounds.add(maxBounds);
        }
        return minBounds;
    }

    /**
     * Returns the height of the specified row based on the current layout.
     *
     * @param row a row
     * @return the specified row height or -1 if row isn't valid
     * @see #convertYToRow
     * @see #convertRowToY
     * @see #updateLayoutState
     */
    protected int getRowHeight(final int row) {
        return getHeight(0, row);
    }

    /**
     * Convert the {@code JList} relative coordinate to the row that contains it, based on the current layout. If {@code
     * y0} doesn't fall within any row, return -1.
     *
     * @param y0 a relative Y coordinate
     * @return the row that contains y0, or -1
     * @see #getRowHeight
     * @see #updateLayoutState
     */
    protected int convertYToRow(final int y0) {
        return convertLocationToRow(0, y0, false);
    }

    /**
     * Return the {@code JList} relative Y coordinate of the origin of the specified row or -1 if row isn't valid.
     *
     * @param row a row
     * @return the Y coordinate of the origin of row, or -1
     * @see #getRowHeight
     * @see #updateLayoutState
     */
    protected int convertRowToY(final int row) {
        if (row >= getRowCount(0) || row < 0) {
            return -1;
        }
        Rectangle bounds = getCellBounds(list, row, row);
        return bounds.y;
    }

    /**
     * If updateLayoutStateNeeded is non zero, call updateLayoutState() and reset updateLayoutStateNeeded.  This method
     * should be called by methods before doing any computation based on the geometry of the list. For example it's the
     * first call in paint() and getPreferredSize().
     *
     * @see #updateLayoutState
     */
    protected void maybeUpdateLayoutState() {
        if (updateLayoutStateNeeded != 0) {
            updateLayoutState();
            updateLayoutStateNeeded = 0;
        }
    }

    /**
     * Recompute the value of cellHeight or cellHeights based and cellWidth, based on the current font and the current
     * values of fixedCellWidth, fixedCellHeight, and prototypeCellValue.
     *
     * @see #maybeUpdateLayoutState
     */
    protected void updateLayoutState() {
        /* If both JList fixedCellWidth and fixedCellHeight have been
         * set, then initialize cellWidth and cellHeight, and set
         * cellHeights to null.
         */

        int fixedCellHeight = list.getFixedCellHeight();
        int fixedCellWidth = list.getFixedCellWidth();

        cellWidth = fixedCellWidth;

        if (fixedCellHeight != -1) {
            cellHeight = fixedCellHeight;
            cellHeights = null;
        } else {
            cellHeight = -1;
            cellHeights = new int[list.getModel().getSize()];
        }

        /* If either of  JList fixedCellWidth and fixedCellHeight haven't
         * been set, then initialize cellWidth and cellHeights by
         * scanning through the entire model.  Note: if the renderer is
         * null, we just set cellWidth and cellHeights[*] to zero,
         * if they're not set already.
         */

        if ((fixedCellWidth == -1) || (fixedCellHeight == -1)) {

            ListModel<Object> dataModel = list.getModel();
            int dataModelSize = dataModel.getSize();
            ListCellRenderer<Object> renderer = list.getCellRenderer();

            if (renderer != null) {
                for (int index = 0; index < dataModelSize; index++) {
                    Object value = dataModel.getElementAt(index);
                    Component c = renderer.getListCellRendererComponent(list, value, index, false, false);
                    rendererPane.add(c);
                    Dimension cellSize = c.getPreferredSize();
                    if (fixedCellWidth == -1) {
                        cellWidth = Math.max(cellSize.width, cellWidth);
                    }
                    if (fixedCellHeight == -1) {
                        cellHeights[index] = cellSize.height;
                    }
                }
            } else {
                if (cellWidth == -1) {
                    cellWidth = 0;
                }
                if (cellHeights == null) {
                    cellHeights = new int[dataModelSize];
                }
                for (int index = 0; index < dataModelSize; index++) {
                    cellHeights[index] = 0;
                }
            }
        }

        columnCount = 1;
        if (layoutOrientation != JList.VERTICAL) {
            updateHorizontalLayoutState(fixedCellWidth, fixedCellHeight);
        }
    }

    /**
     * Creates a delegate that implements {@code MouseInputListener}. The delegate is added to the corresponding {@code
     * java.awt.Component} listener lists at {@code installUI()} time. Subclasses can override this method to return a
     * custom {@code MouseInputListener}, e.g.
     * <pre>
     * class MyListUI extends BasicListUI {
     *    protected MouseInputListener <b>createMouseInputListener</b>() {
     *        return new MyMouseInputHandler();
     *    }
     *    public class MyMouseInputHandler extends MouseInputHandler {
     *        public void mouseMoved(MouseEvent e) {
     *            // do some extra work when the mouse moves
     *            super.mouseMoved(e);
     *        }
     *    }
     * }
     * </pre>
     *
     * @return an instance of {@code MouseInputListener}
     * @see MouseInputHandler
     * @see #installUI
     */
    protected MouseInputListener createMouseInputListener() {
        return getHandler();
    }

    /**
     * Returns an instance of {@code FocusListener}.
     *
     * @return an instance of {@code FocusListener}
     */
    protected FocusListener createFocusListener() {
        return getHandler();
    }

    /**
     * Creates an instance of {@code ListSelectionHandler} that's added to the {@code JLists} by selectionModel as
     * needed.  Subclasses can override this method to return a custom {@code ListSelectionListener}, e.g.
     * <pre>
     * class MyListUI extends BasicListUI {
     *    protected ListSelectionListener <b>createListSelectionListener</b>() {
     *        return new MySelectionListener();
     *    }
     *    public class MySelectionListener extends ListSelectionHandler {
     *        public void valueChanged(ListSelectionEvent e) {
     *            // do some extra work when the selection changes
     *            super.valueChange(e);
     *        }
     *    }
     * }
     * </pre>
     *
     * @return an instance of {@code ListSelectionHandler}
     * @see ListSelectionHandler
     * @see #installUI
     */
    protected ListSelectionListener createListSelectionListener() {
        return getHandler();
    }

    /**
     * Creates an instance of {@code ListDataListener} that's added to the {@code JLists} by model as needed. Subclasses
     * can override this method to return a custom {@code ListDataListener}, e.g.
     * <pre>
     * class MyListUI extends BasicListUI {
     *    protected ListDataListener <b>createListDataListener</b>() {
     *        return new MyListDataListener();
     *    }
     *    public class MyListDataListener extends ListDataHandler {
     *        public void contentsChanged(ListDataEvent e) {
     *            // do some extra work when the models contents change
     *            super.contentsChange(e);
     *        }
     *    }
     * }
     * </pre>
     *
     * @return an instance of {@code ListDataListener}
     * @see ListDataListener
     * @see JList#getModel
     * @see #installUI
     */
    protected ListDataListener createListDataListener() {
        return getHandler();
    }

    /**
     * Creates an instance of {@code PropertyChangeHandler} that's added to the {@code JList} by {@code installUI()}.
     * Subclasses can override this method to return a custom {@code PropertyChangeListener}, e.g.
     * <pre>
     * class MyListUI extends BasicListUI {
     *    protected PropertyChangeListener <b>createPropertyChangeListener</b>() {
     *        return new MyPropertyChangeListener();
     *    }
     *    public class MyPropertyChangeListener extends PropertyChangeHandler {
     *        public void propertyChange(PropertyChangeEvent e) {
     *            if (e.getPropertyName().equals("model")) {
     *                // do some extra work when the model changes
     *            }
     *            super.propertyChange(e);
     *        }
     *    }
     * }
     * </pre>
     *
     * @return an instance of {@code PropertyChangeHandler}
     * @see PropertyChangeListener
     * @see #installUI
     */
    protected PropertyChangeListener createPropertyChangeListener() {
        return getHandler();
    }

    /**
     * Returns the row at location x/y.
     *
     * @param closest If true and the location doesn't exactly match a particular location, this will return the closest
     *                row.
     */
    public int convertLocationToRow(final int x, final int y0, final boolean closest) {
        int size = list.getModel().getSize();

        if (size <= 0) {
            return -1;
        }
        Insets insets = list.getInsets();
        if (cellHeights == null) {
            int row = (cellHeight == 0) ? 0 :
                      ((y0 - insets.top) / cellHeight);
            if (closest) {
                if (row < 0) {
                    row = 0;
                } else if (row >= size) {
                    row = size - 1;
                }
            }
            return row;
        } else if (size > cellHeights.length) {
            return -1;
        } else {
            int y = insets.top;
            int row = 0;

            if (closest && y0 < y) {
                return 0;
            }
            int i;
            for (i = 0; i < size; i++) {
                if ((y0 >= y) && (y0 < y + cellHeights[i])) {
                    return row;
                }
                y += cellHeights[i];
                row += 1;
            }
            return i - 1;
        }
    }

    /**
     * Returns the closest location to the model index of the passed in location.
     */
    protected int convertLocationToModel(final int x, final int y) {
        int row = convertLocationToRow(x, y, true);
        int column = convertLocationToColumn(x, y);

        if (row >= 0 && column >= 0) {
            return getModelIndex(column, row);
        }
        return -1;
    }

    /**
     * Returns the closest column to the passed in location.
     */
    protected int convertLocationToColumn(final int x, final int y) {
        if (cellWidth > 0) {
            if (layoutOrientation == JList.VERTICAL) {
                return 0;
            }
            Insets insets = list.getInsets();
            int col;
            if (isLeftToRight) {
                col = (x - insets.left) / cellWidth;
            } else {
                col = (list.getWidth() - x - insets.right - 1) / cellWidth;
            }
            if (col < 0) {
                return 0;
            } else if (col >= columnCount) {
                return columnCount - 1;
            }
            return col;
        }
        return 0;
    }

    /**
     * Returns the model index for the specified display location. If <code>column</code>x<code>row</code> is beyond the
     * length of the model, this will return the model size - 1.
     */
    protected int getModelIndex(final int column, final int row) {
        switch (layoutOrientation) {
            case JList.VERTICAL_WRAP:
                return Math.min(list.getModel().getSize() - 1, rowsPerColumn *
                        column + Math.min(row, rowsPerColumn - 1));
            case JList.HORIZONTAL_WRAP:
                return Math.min(list.getModel().getSize() - 1, row * columnCount +
                        column);
            default:
                return row;
        }
    }

    /**
     * Gets the bounds of the specified model index, returning the resulting bounds, or null if <code>index</code> is
     * not valid.
     */
    protected Rectangle getCellBounds(final JList<?> list, final int index) {
        maybeUpdateLayoutState();

        int row = convertModelToRow(index);
        int column = convertModelToColumn(index);

        if (row == -1 || column == -1) {
            return null;
        }

        Insets insets = list.getInsets();
        int x;
        int w = cellWidth;
        int y = insets.top;
        int h;
        switch (layoutOrientation) {
            case JList.VERTICAL_WRAP:
            case JList.HORIZONTAL_WRAP:
                if (isLeftToRight) {
                    x = insets.left + column * cellWidth;
                } else {
                    x = list.getWidth() - insets.right - (column + 1) * cellWidth;
                }
                y += cellHeight * row;
                h = cellHeight;
                break;
            default:
                x = insets.left;
                if (cellHeights == null) {
                    y += (cellHeight * row);
                } else if (row >= cellHeights.length) {
                    y = 0;
                } else {
                    for (int i = 0; i < row; i++) {
                        y += cellHeights[i];
                    }
                }
                w = list.getWidth() - (insets.left + insets.right);
                h = getRowHeight(index);
                break;
        }
        return new Rectangle(x, y, w, h);
    }

    protected void paintImpl(final Graphics g, final JComponent c) {
        switch (layoutOrientation) {
            case JList.VERTICAL_WRAP:
                if (list.getHeight() != listHeight) {
                    updateLayoutStateNeeded |= heightChanged;
                    redrawList();
                }
                break;
            case JList.HORIZONTAL_WRAP:
                if (list.getWidth() != listWidth) {
                    updateLayoutStateNeeded |= widthChanged;
                    redrawList();
                }
                break;
            default:
                break;
        }
        maybeUpdateLayoutState();

        ListCellRenderer<Object> renderer = list.getCellRenderer();
        ListModel<Object> dataModel = list.getModel();
        ListSelectionModel selModel = list.getSelectionModel();
        int size;

        if ((renderer == null) || (size = dataModel.getSize()) == 0) {
            return;
        }

        // Determine how many columns we need to paint
        Rectangle paintBounds = g.getClipBounds();

        int startColumn, endColumn;
        if (c.getComponentOrientation().isLeftToRight()) {
            startColumn = convertLocationToColumn(paintBounds.x,
                                                  paintBounds.y);
            endColumn = convertLocationToColumn(paintBounds.x +
                                                        paintBounds.width,
                                                paintBounds.y);
        } else {
            startColumn = convertLocationToColumn(paintBounds.x +
                                                          paintBounds.width,
                                                  paintBounds.y);
            endColumn = convertLocationToColumn(paintBounds.x,
                                                paintBounds.y);
        }
        int maxY = paintBounds.y + paintBounds.height;
        int leadIndex = adjustIndex(list.getLeadSelectionIndex(), list);
        int rowIncrement = (layoutOrientation == JList.HORIZONTAL_WRAP) ?
                           columnCount : 1;


        for (int colCounter = startColumn; colCounter <= endColumn;
             colCounter++) {
            // And then how many rows in this columnn
            int row = convertLocationToRowInColumn(paintBounds.y, colCounter);
            int rowCount = getRowCount(colCounter);
            int index = getModelIndex(colCounter, row);
            Rectangle rowBounds = getCellBounds(list, index, index);

            if (rowBounds == null) {
                // Not valid, bail!
                return;
            }
            while (row < rowCount && rowBounds.y < maxY &&
                    index < size) {
                rowBounds.height = getHeight(colCounter, row);
                g.setClip(rowBounds.x, rowBounds.y, rowBounds.width,
                          rowBounds.height);
                g.clipRect(paintBounds.x, paintBounds.y, paintBounds.width,
                           paintBounds.height);
                paintCell(g, index, rowBounds, renderer, dataModel, selModel,
                          leadIndex);
                rowBounds.y += rowBounds.height;
                index += rowIncrement;
                row++;
            }
        }
        // Empty out the renderer pane, allowing renderers to be gc'ed.
        rendererPane.removeAll();
    }

    protected void paintDropLine(final Graphics g) {
        JList.DropLocation loc = list.getDropLocation();
        if (loc == null || !loc.isInsert()) {
            return;
        }

        Color c = DefaultLookup.getColor(list, this, "List.dropLineColor", null);
        if (c != null) {
            g.setColor(c);
            Rectangle rect = getDropLineRect(loc);
            g.fillRect(rect.x, rect.y, rect.width, rect.height);
        }
    }

    protected Rectangle getDropLineRect(final JList.DropLocation loc) {
        int size = list.getModel().getSize();

        if (size == 0) {
            Insets insets = list.getInsets();
            if (layoutOrientation == JList.HORIZONTAL_WRAP) {
                if (isLeftToRight) {
                    return new Rectangle(insets.left, insets.top, DROP_LINE_THICKNESS, 20);
                } else {
                    return new Rectangle(list.getWidth() - DROP_LINE_THICKNESS - insets.right,
                                         insets.top, DROP_LINE_THICKNESS, 20);
                }
            } else {
                return new Rectangle(insets.left, insets.top,
                                     list.getWidth() - insets.left - insets.right,
                                     DROP_LINE_THICKNESS);
            }
        }

        Rectangle rect = null;
        int index = loc.getIndex();
        boolean decr = false;

        if (layoutOrientation == JList.HORIZONTAL_WRAP) {
            if (index == size) {
                decr = true;
            } else if (index != 0 && convertModelToRow(index)
                    != convertModelToRow(index - 1)) {

                Rectangle prev = getCellBounds(list, index - 1);
                Rectangle me = getCellBounds(list, index);
                Point p = loc.getDropPoint();

                if (isLeftToRight) {
                    decr = Point2D.distance(prev.x + prev.width,
                                            prev.y + (int) (prev.height / 2.0),
                                            p.x, p.y)
                            < Point2D.distance(me.x,
                                               me.y + (int) (me.height / 2.0),
                                               p.x, p.y);
                } else {
                    decr = Point2D.distance(prev.x,
                                            prev.y + (int) (prev.height / 2.0),
                                            p.x, p.y)
                            < Point2D.distance(me.x + me.width,
                                               me.y + (int) (prev.height / 2.0),
                                               p.x, p.y);
                }
            }

            if (decr) {
                index--;
                rect = getCellBounds(list, index);
                if (isLeftToRight) {
                    rect.x += rect.width;
                } else {
                    rect.x -= DROP_LINE_THICKNESS;
                }
            } else {
                rect = getCellBounds(list, index);
                if (!isLeftToRight) {
                    rect.x += rect.width - DROP_LINE_THICKNESS;
                }
            }

            if (rect.x >= list.getWidth()) {
                rect.x = list.getWidth() - DROP_LINE_THICKNESS;
            } else if (rect.x < 0) {
                rect.x = 0;
            }

            rect.width = DROP_LINE_THICKNESS;
        } else if (layoutOrientation == JList.VERTICAL_WRAP) {
            if (index == size) {
                index--;
                rect = getCellBounds(list, index);
                rect.y += rect.height;
            } else if (index != 0 && convertModelToColumn(index)
                    != convertModelToColumn(index - 1)) {

                Rectangle prev = getCellBounds(list, index - 1);
                Rectangle me = getCellBounds(list, index);
                Point p = loc.getDropPoint();
                if (Point2D.distance(prev.x + (int) (prev.width / 2.0),
                                     prev.y + prev.height,
                                     p.x, p.y)
                        < Point2D.distance(me.x + (int) (me.width / 2.0),
                                           me.y,
                                           p.x, p.y)) {

                    index--;
                    rect = getCellBounds(list, index);
                    rect.y += rect.height;
                } else {
                    rect = getCellBounds(list, index);
                }
            } else {
                rect = getCellBounds(list, index);
            }

            if (rect.y >= list.getHeight()) {
                rect.y = list.getHeight() - DROP_LINE_THICKNESS;
            }

            rect.height = DROP_LINE_THICKNESS;
        } else {
            if (index == size) {
                index--;
                rect = getCellBounds(list, index);
                rect.y += rect.height;
            } else {
                rect = getCellBounds(list, index);
            }

            if (rect.y >= list.getHeight()) {
                rect.y = list.getHeight() - DROP_LINE_THICKNESS;
            }

            rect.height = DROP_LINE_THICKNESS;
        }

        return rect;
    }

    /**
     * Returns the row that the model index <code>index</code> will be displayed in..
     */
    public int convertModelToRow(final int index) {
        int size = list.getModel().getSize();

        if ((index < 0) || (index >= size)) {
            return -1;
        }

        if (layoutOrientation != JList.VERTICAL && columnCount > 1 &&
                rowsPerColumn > 0) {
            if (layoutOrientation == JList.VERTICAL_WRAP) {
                return index % rowsPerColumn;
            }
            return index / columnCount;
        }
        return index;
    }

    /**
     * Returns the height of the cell at the passed in location.
     */
    protected int getHeight(final int column, final int row) {
        if (column < 0 || column > columnCount || row < 0) {
            return -1;
        }
        if (layoutOrientation != JList.VERTICAL) {
            return cellHeight;
        }
        if (row >= list.getModel().getSize()) {
            return -1;
        }
        return (cellHeights == null) ? cellHeight :
               ((row < cellHeights.length) ? cellHeights[row] : -1);
    }

    /**
     * Returns the column that the model index <code>index</code> will be displayed in.
     */
    protected int convertModelToColumn(final int index) {
        int size = list.getModel().getSize();

        if ((index < 0) || (index >= size)) {
            return -1;
        }

        if (layoutOrientation != JList.VERTICAL && rowsPerColumn > 0 &&
                columnCount > 1) {
            if (layoutOrientation == JList.VERTICAL_WRAP) {
                return index / rowsPerColumn;
            }
            return index % columnCount;
        }
        return 0;
    }

    /**
     * Returns the number of rows in the given column.
     */
    protected int getRowCount(final int column) {
        if (column < 0 || column >= columnCount) {
            return -1;
        }
        if (layoutOrientation == JList.VERTICAL ||
                (column == 0 && columnCount == 1)) {
            return list.getModel().getSize();
        }
        if (column >= columnCount) {
            return -1;
        }
        if (layoutOrientation == JList.VERTICAL_WRAP) {
            if (column < (columnCount - 1)) {
                return rowsPerColumn;
            }
            return list.getModel().getSize() - (columnCount - 1) *
                    rowsPerColumn;
        }
        // JList.HORIZONTAL_WRAP
        int diff = columnCount - (columnCount * rowsPerColumn -
                list.getModel().getSize());

        if (column >= diff) {
            return Math.max(0, rowsPerColumn - 1);
        }
        return rowsPerColumn;
    }

    /**
     * Invoked when the list is layed out horizontally to determine how many columns to create.
     * <p>
     * This updates the <code>rowsPerColumn, </code><code>columnCount</code>,
     * <code>preferredHeight</code> and potentially <code>cellHeight</code>
     * instance variables.
     */
    protected void updateHorizontalLayoutState(final int fixedCellWidth,
                                               final int fixedCellHeight) {
        int visRows = list.getVisibleRowCount();
        int dataModelSize = list.getModel().getSize();
        Insets insets = list.getInsets();

        listHeight = list.getHeight();
        listWidth = list.getWidth();

        if (dataModelSize == 0) {
            rowsPerColumn = columnCount = 0;
            preferredHeight = insets.top + insets.bottom;
            return;
        }

        int height;

        if (fixedCellHeight != -1) {
            height = fixedCellHeight;
        } else {
            // Determine the max of the renderer heights.
            int maxHeight = 0;
            if (cellHeights.length > 0) {
                maxHeight = cellHeights[cellHeights.length - 1];
                for (int counter = cellHeights.length - 2;
                     counter >= 0; counter--) {
                    maxHeight = Math.max(maxHeight, cellHeights[counter]);
                }
            }
            height = cellHeight = maxHeight;
            cellHeights = null;
        }
        // The number of rows is either determined by the visible row
        // count, or by the height of the list.
        rowsPerColumn = dataModelSize;
        if (visRows > 0) {
            rowsPerColumn = visRows;
            columnCount = Math.max(1, dataModelSize / rowsPerColumn);
            if (dataModelSize > 0 && dataModelSize > rowsPerColumn &&
                    dataModelSize % rowsPerColumn != 0) {
                columnCount++;
            }
            if (layoutOrientation == JList.HORIZONTAL_WRAP) {
                // Because HORIZONTAL_WRAP flows differently, the
                // rowsPerColumn needs to be adjusted.
                rowsPerColumn = (dataModelSize / columnCount);
                if (dataModelSize % columnCount > 0) {
                    rowsPerColumn++;
                }
            }
        } else if (layoutOrientation == JList.VERTICAL_WRAP && height != 0) {
            rowsPerColumn = Math.max(1, (listHeight - insets.top -
                    insets.bottom) / height);
            columnCount = Math.max(1, dataModelSize / rowsPerColumn);
            if (dataModelSize > 0 && dataModelSize > rowsPerColumn &&
                    dataModelSize % rowsPerColumn != 0) {
                columnCount++;
            }
        } else if (layoutOrientation == JList.HORIZONTAL_WRAP && cellWidth > 0 &&
                listWidth > 0) {
            columnCount = Math.max(1, (listWidth - insets.left -
                    insets.right) / cellWidth);
            rowsPerColumn = dataModelSize / columnCount;
            if (dataModelSize % columnCount > 0) {
                rowsPerColumn++;
            }
        }
        preferredHeight = rowsPerColumn * cellHeight + insets.top +
                insets.bottom;
    }

    /**
     * Returns the closest row that starts at the specified y-location in the passed in column.
     */
    protected int convertLocationToRowInColumn(final int y, final int column) {
        int x = 0;

        if (layoutOrientation != JList.VERTICAL) {
            if (isLeftToRight) {
                x = column * cellWidth;
            } else {
                x = list.getWidth() - (column + 1) * cellWidth - list.getInsets().right;
            }
        }
        return convertLocationToRow(x, y, true);
    }

    protected void redrawList() {
        list.revalidate();
        list.repaint();
    }

    @SuppressWarnings("serial") // Superclass is a JDK-implementation class
    static class ListTransferHandler extends TransferHandler implements UIResource {

        public int getSourceActions(final JComponent c) {
            return COPY;
        }

        /**
         * Create a Transferable to use as the source for a data transfer.
         *
         * @param c The component holding the data to be transfered.  This argument is provided to enable sharing of
         *          TransferHandlers by multiple components.
         * @return The representation of the data to be transfered.
         */
        @SuppressWarnings("deprecation")
        protected Transferable createTransferable(final JComponent c) {
            if (c instanceof JList) {
                JList<?> list = (JList) c;
                Object[] values = list.getSelectedValues();

                if (values == null || values.length == 0) {
                    return null;
                }

                StringBuilder plainStr = new StringBuilder();
                StringBuilder htmlStr = new StringBuilder();

                htmlStr.append("<html>\n<body>\n<ul>\n");

                for (int i = 0; i < values.length; i++) {
                    Object obj = values[i];
                    String val = ((obj == null) ? "" : obj.toString());
                    plainStr.append(val).append('\n');
                    htmlStr.append("  <li>").append(val).append('\n');
                }

                // remove the last newline
                plainStr.deleteCharAt(plainStr.length() - 1);
                htmlStr.append("</ul>\n</body>\n</html>");

                return new BasicTransferable(plainStr.toString(), htmlStr.toString());
            }

            return null;
        }

    }

    /**
     * Mouse input, and focus handling for JList.  An instance of this class is added to the appropriate
     * java.awt.Component lists at installUI() time.  Note keyboard input is handled with JComponent KeyboardActions,
     * see installKeyboardActions().
     * <p>
     * <strong>Warning:</strong>
     * Serialized objects of this class will not be compatible with future Swing releases. The current serialization
     * support is appropriate for short term storage or RMI between applications running the same version of Swing.  As
     * of 1.4, support for long term storage of all JavaBeans&trade; has been added to the <code>java.beans</code>
     * package. Please see {@link java.beans.XMLEncoder}.
     *
     * @see #createMouseInputListener
     * @see #installKeyboardActions
     * @see #installUI
     */
    @SuppressWarnings("serial") // Same-version serialization only
    public class MouseInputHandler implements MouseInputListener {
        public void mouseClicked(final MouseEvent e) {
            getHandler().mouseClicked(e);
        }

        public void mousePressed(final MouseEvent e) {
            getHandler().mousePressed(e);
        }

        public void mouseReleased(final MouseEvent e) {
            getHandler().mouseReleased(e);
        }

        public void mouseEntered(final MouseEvent e) {
            getHandler().mouseEntered(e);
        }

        public void mouseExited(final MouseEvent e) {
            getHandler().mouseExited(e);
        }

        public void mouseDragged(final MouseEvent e) {
            getHandler().mouseDragged(e);
        }

        public void mouseMoved(final MouseEvent e) {
            getHandler().mouseMoved(e);
        }
    }

    /**
     * This class should be treated as a &quot;protected&quot; inner class. Instantiate it only within subclasses of
     * {@code BasicListUI}.
     */
    public class FocusHandler implements FocusListener {
        /**
         * Repaints focused cells.
         */
        protected void repaintCellFocus() {
            getHandler().repaintCellFocus();
        }

        /* The focusGained() focusLost() methods run when the JList
         * focus changes.
         */

        public void focusGained(final FocusEvent e) {
            getHandler().focusGained(e);
        }

        public void focusLost(final FocusEvent e) {
            getHandler().focusLost(e);
        }
    }

    /**
     * The ListSelectionListener that's added to the JLists selection model at installUI time, and whenever the
     * JList.selectionModel property changes.  When the selection changes we repaint the affected rows.
     * <p>
     * <strong>Warning:</strong>
     * Serialized objects of this class will not be compatible with future Swing releases. The current serialization
     * support is appropriate for short term storage or RMI between applications running the same version of Swing.  As
     * of 1.4, support for long term storage of all JavaBeans&trade; has been added to the <code>java.beans</code>
     * package. Please see {@link java.beans.XMLEncoder}.
     *
     * @see #createListSelectionListener
     * @see #installUI
     */
    @SuppressWarnings("serial") // Same-version serialization only
    public class ListSelectionHandler implements ListSelectionListener {
        public void valueChanged(final ListSelectionEvent e) {
            getHandler().valueChanged(e);
        }
    }

    /**
     * The {@code ListDataListener} that's added to the {@code JLists} model at {@code installUI time}, and whenever the
     * JList.model property changes.
     * <p>
     * <strong>Warning:</strong>
     * Serialized objects of this class will not be compatible with future Swing releases. The current serialization
     * support is appropriate for short term storage or RMI between applications running the same version of Swing.  As
     * of 1.4, support for long term storage of all JavaBeans&trade; has been added to the <code>java.beans</code>
     * package. Please see {@link java.beans.XMLEncoder}.
     *
     * @see JList#getModel
     * @see #maybeUpdateLayoutState
     * @see #createListDataListener
     * @see #installUI
     */
    @SuppressWarnings("serial") // Same-version serialization only
    public class ListDataHandler implements ListDataListener {
        public void intervalAdded(final ListDataEvent e) {
            getHandler().intervalAdded(e);
        }


        public void intervalRemoved(final ListDataEvent e) {
            getHandler().intervalRemoved(e);
        }


        public void contentsChanged(final ListDataEvent e) {
            getHandler().contentsChanged(e);
        }
    }

    /**
     * The PropertyChangeListener that's added to the JList at installUI time.  When the value of a JList property that
     * affects layout changes, we set a bit in updateLayoutStateNeeded. If the JLists model changes we additionally
     * remove our listeners from the old model.  Likewise for the JList selectionModel.
     * <p>
     * <strong>Warning:</strong>
     * Serialized objects of this class will not be compatible with future Swing releases. The current serialization
     * support is appropriate for short term storage or RMI between applications running the same version of Swing.  As
     * of 1.4, support for long term storage of all JavaBeans&trade; has been added to the <code>java.beans</code>
     * package. Please see {@link java.beans.XMLEncoder}.
     *
     * @see #maybeUpdateLayoutState
     * @see #createPropertyChangeListener
     * @see #installUI
     */
    @SuppressWarnings("serial") // Same-version serialization only
    public class PropertyChangeHandler implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent e) {
            getHandler().propertyChange(e);
        }
    }

    protected class Handler implements FocusListener, KeyListener,
            ListDataListener, ListSelectionListener,
            MouseInputListener, PropertyChangeListener,
            DragRecognitionSupport.BeforeDrag {
        //
        // KeyListener
        //
        protected String prefix = "";
        protected String typedString = "";
        protected long lastTime = 0L;
        // Whether or not the mouse press (which is being considered as part
        // of a drag sequence) also caused the selection change to be fully
        // processed.
        protected boolean dragPressDidSelection;

        /**
         * Invoked when a key has been typed.
         * <p>
         * Moves the keyboard focus to the first element whose prefix matches the sequence of alphanumeric keys pressed
         * by the user with delay less than value of <code>timeFactor</code> property (or 1000 milliseconds if it is not
         * defined). Subsequent same key presses move the keyboard focus to the next object that starts with the same
         * letter until another key is pressed, then it is treated as the prefix with appropriate number of the same
         * letters followed by first typed another letter.
         */
        public void keyTyped(final KeyEvent e) {
            JList<?> src = (JList) e.getSource();
            ListModel<?> model = src.getModel();

            if (model.getSize() == 0 || e.isAltDown() ||
                    DarkUIUtil.isMenuShortcutKeyDown(e) ||
                    isNavigationKey(e)) {
                // Nothing to select
                return;
            }
            boolean startingFromSelection = true;

            char c = e.getKeyChar();

            long time = e.getWhen();
            int startIndex = adjustIndex(src.getLeadSelectionIndex(), list);
            if (time - lastTime < timeFactor) {
                typedString += c;
                if ((prefix.length() == 1) && (c == prefix.charAt(0))) {
                    // Subsequent same key presses move the keyboard focus to the next
                    // object that starts with the same letter.
                    startIndex++;
                } else {
                    prefix = typedString;
                }
            } else {
                startIndex++;
                typedString = "" + c;
                prefix = typedString;
            }
            lastTime = time;

            if (startIndex < 0 || startIndex >= model.getSize()) {
                startingFromSelection = false;
                startIndex = 0;
            }
            int index = src.getNextMatch(prefix, startIndex,
                                         Position.Bias.Forward);
            if (index >= 0) {
                src.setSelectedIndex(index);
                src.ensureIndexIsVisible(index);
            } else if (startingFromSelection) { // wrap
                index = src.getNextMatch(prefix, 0,
                                         Position.Bias.Forward);
                if (index >= 0) {
                    src.setSelectedIndex(index);
                    src.ensureIndexIsVisible(index);
                }
            }
        }

        /**
         * Invoked when a key has been pressed.
         * <p>
         * Checks to see if the key event is a navigation key to prevent dispatching these keys for the first letter
         * navigation.
         */
        public void keyPressed(final KeyEvent e) {
            if (isNavigationKey(e)) {
                prefix = "";
                typedString = "";
                lastTime = 0L;
            }
        }

        /**
         * Invoked when a key has been released. See the class description for {@link KeyEvent} for a definition of a
         * key released event.
         */
        public void keyReleased(final KeyEvent e) {
        }

        /**
         * Returns whether or not the supplied key event maps to a key that is used for navigation.  This is used for
         * optimizing key input by only passing non- navigation keys to the first letter navigation mechanism.
         */
        protected boolean isNavigationKey(final KeyEvent event) {
            InputMap inputMap = list.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);
            KeyStroke key = KeyStroke.getKeyStrokeForEvent(event);

            return inputMap != null && inputMap.get(key) != null;
        }

        //
        // PropertyChangeListener
        //
        public void propertyChange(final PropertyChangeEvent e) {
            String propertyName = e.getPropertyName();

            /* If the JList.model property changes, remove our listener,
             * listDataListener from the old model and add it to the new one.
             */
            if (Objects.equals(propertyName, "model")) {
                ListModel<?> oldModel = (ListModel) e.getOldValue();
                ListModel<?> newModel = (ListModel) e.getNewValue();
                if (oldModel != null) {
                    oldModel.removeListDataListener(listDataListener);
                }
                if (newModel != null) {
                    newModel.addListDataListener(listDataListener);
                }
                updateLayoutStateNeeded |= modelChanged;
                redrawList();
            }

            /* If the JList.selectionModel property changes, remove our listener,
             * listSelectionListener from the old selectionModel and add it to the new one.
             */
            else if (Objects.equals(propertyName, "selectionModel")) {
                ListSelectionModel oldModel = (ListSelectionModel) e.getOldValue();
                ListSelectionModel newModel = (ListSelectionModel) e.getNewValue();
                if (oldModel != null) {
                    oldModel.removeListSelectionListener(listSelectionListener);
                }
                if (newModel != null) {
                    newModel.addListSelectionListener(listSelectionListener);
                }
                updateLayoutStateNeeded |= modelChanged;
                redrawList();
            } else if (Objects.equals(propertyName, "cellRenderer")) {
                updateLayoutStateNeeded |= cellRendererChanged;
                redrawList();
            } else if (Objects.equals(propertyName, "font")
                    || SwingUtilities2.isScaleChanged(e)) {
                updateLayoutStateNeeded |= fontChanged;
                redrawList();
            } else if (Objects.equals(propertyName, "prototypeCellValue")) {
                updateLayoutStateNeeded |= prototypeCellValueChanged;
                redrawList();
            } else if (Objects.equals(propertyName, "fixedCellHeight")) {
                updateLayoutStateNeeded |= fixedCellHeightChanged;
                redrawList();
            } else if (Objects.equals(propertyName, "fixedCellWidth")) {
                updateLayoutStateNeeded |= fixedCellWidthChanged;
                redrawList();
            } else if (Objects.equals(propertyName, "selectionForeground")) {
                list.repaint();
            } else if (Objects.equals(propertyName, "selectionBackground")) {
                list.repaint();
            } else if ("layoutOrientation".equals(propertyName)) {
                updateLayoutStateNeeded |= layoutOrientationChanged;
                layoutOrientation = list.getLayoutOrientation();
                redrawList();
            } else if ("visibleRowCount".equals(propertyName)) {
                if (layoutOrientation != JList.VERTICAL) {
                    updateLayoutStateNeeded |= layoutOrientationChanged;
                    redrawList();
                }
            } else if ("componentOrientation".equals(propertyName)) {
                isLeftToRight = list.getComponentOrientation().isLeftToRight();
                updateLayoutStateNeeded |= componentOrientationChanged;
                redrawList();

                InputMap inputMap = getInputMap(JComponent.WHEN_FOCUSED);
                SwingUtilities.replaceUIInputMap(list, JComponent.WHEN_FOCUSED,
                                                 inputMap);
            } else if ("List.isFileList".equals(propertyName)) {
                updateIsFileList();
                redrawList();
            } else if ("dropLocation".equals(propertyName)) {
                JList.DropLocation oldValue = (JList.DropLocation) e.getOldValue();
                repaintDropLocation(oldValue);
                repaintDropLocation(list.getDropLocation());
            }
        }

        protected void repaintDropLocation(final JList.DropLocation loc) {
            if (loc == null) {
                return;
            }

            Rectangle r;

            if (loc.isInsert()) {
                r = getDropLineRect(loc);
            } else {
                r = getCellBounds(list, loc.getIndex());
            }

            if (r != null) {
                list.repaint(r);
            }
        }

        //
        // ListDataListener
        //
        public void intervalAdded(final ListDataEvent e) {
            updateLayoutStateNeeded = modelChanged;

            int minIndex = Math.min(e.getIndex0(), e.getIndex1());
            int maxIndex = Math.max(e.getIndex0(), e.getIndex1());

            /* Sync the SelectionModel with the DataModel.
             */

            ListSelectionModel sm = list.getSelectionModel();
            if (sm != null) {
                sm.insertIndexInterval(minIndex, maxIndex - minIndex + 1, true);
            }

            /* Repaint the entire list, from the origin of
             * the first added cell, to the bottom of the
             * component.
             */
            redrawList();
        }

        public void intervalRemoved(final ListDataEvent e) {
            updateLayoutStateNeeded = modelChanged;

            /* Sync the SelectionModel with the DataModel.
             */

            ListSelectionModel sm = list.getSelectionModel();
            if (sm != null) {
                sm.removeIndexInterval(e.getIndex0(), e.getIndex1());
            }

            /* Repaint the entire list, from the origin of
             * the first removed cell, to the bottom of the
             * component.
             */

            redrawList();
        }

        public void contentsChanged(final ListDataEvent e) {
            updateLayoutStateNeeded = modelChanged;
            redrawList();
        }

        //
        // ListSelectionListener
        //
        public void valueChanged(final ListSelectionEvent e) {
            maybeUpdateLayoutState();

            int size = list.getModel().getSize();
            int firstIndex = Math.min(size - 1, Math.max(e.getFirstIndex(), 0));
            int lastIndex = Math.min(size - 1, Math.max(e.getLastIndex(), 0));

            Rectangle bounds = getCellBounds(list, firstIndex, lastIndex);

            if (bounds != null) {
                list.repaint(bounds.x, bounds.y, bounds.width, bounds.height);
            }
        }

        //
        // MouseListener
        //
        public void mouseClicked(final MouseEvent e) {
        }

        public void mousePressed(final MouseEvent e) {
            if (SwingUtilities2.shouldIgnore(e, list)) {
                return;
            }

            boolean dragEnabled = list.getDragEnabled();
            boolean grabFocus = true;

            // different behavior if drag is enabled
            if (dragEnabled) {
                int row = SwingUtilities2.loc2IndexFileList(list, e.getPoint());
                // if we have a valid row and this is a drag initiating event
                if (row != -1 && DragRecognitionSupport.mousePressed(e)) {
                    dragPressDidSelection = false;

                    if (DarkUIUtil.isMenuShortcutKeyDown(e)) {
                        // do nothing for control - will be handled on release
                        // or when drag starts
                        return;
                    } else if (!e.isShiftDown() && list.isSelectedIndex(row)) {
                        // clicking on something that's already selected
                        // and need to make it the lead now
                        list.addSelectionInterval(row, row);
                        return;
                    }

                    // could be a drag initiating event - don't grab focus
                    grabFocus = false;

                    dragPressDidSelection = true;
                }
            } else {
                // When drag is enabled mouse drags won't change the selection
                // in the list, so we only set the isAdjusting flag when it's
                // not enabled
                list.setValueIsAdjusting(true);
            }

            if (grabFocus) {
                SwingUtilities2.adjustFocus(list);
            }

            adjustSelection(e);
        }

        protected void adjustSelection(final MouseEvent e) {
            int row = SwingUtilities2.loc2IndexFileList(list, e.getPoint());
            if (row < 0) {
                // If shift is down in multi-select, we should do nothing.
                // For single select or non-shift-click, clear the selection
                if (isFileList && e.getID() == MouseEvent.MOUSE_PRESSED &&
                        (!e.isShiftDown() ||
                                list.getSelectionMode() == ListSelectionModel.SINGLE_SELECTION)) {
                    list.clearSelection();
                }
            } else {
                int anchorIndex = adjustIndex(list.getAnchorSelectionIndex(), list);
                boolean anchorSelected;
                if (anchorIndex == -1) {
                    anchorIndex = 0;
                    anchorSelected = false;
                } else {
                    anchorSelected = list.isSelectedIndex(anchorIndex);
                }

                if (DarkUIUtil.isMenuShortcutKeyDown(e)) {
                    if (e.isShiftDown()) {
                        if (anchorSelected) {
                            list.addSelectionInterval(anchorIndex, row);
                        } else {
                            list.removeSelectionInterval(anchorIndex, row);
                            if (isFileList) {
                                list.addSelectionInterval(row, row);
                                list.getSelectionModel().setAnchorSelectionIndex(anchorIndex);
                            }
                        }
                    } else if (list.isSelectedIndex(row)) {
                        list.removeSelectionInterval(row, row);
                    } else {
                        list.addSelectionInterval(row, row);
                    }
                } else if (e.isShiftDown()) {
                    list.setSelectionInterval(anchorIndex, row);
                } else {
                    list.setSelectionInterval(row, row);
                }
            }
        }

        public void mouseReleased(final MouseEvent e) {
            if (SwingUtilities2.shouldIgnore(e, list)) {
                return;
            }

            if (list.getDragEnabled()) {
                MouseEvent me = DragRecognitionSupport.mouseReleased(e);
                if (me != null) {
                    SwingUtilities2.adjustFocus(list);
                    if (!dragPressDidSelection) {
                        adjustSelection(me);
                    }
                }
            } else {
                list.setValueIsAdjusting(false);
            }
        }

        public void mouseEntered(final MouseEvent e) {
        }

        public void mouseExited(final MouseEvent e) {
        }

        public void dragStarting(final MouseEvent me) {
            if (DarkUIUtil.isMenuShortcutKeyDown(me)) {
                int row = SwingUtilities2.loc2IndexFileList(list, me.getPoint());
                list.addSelectionInterval(row, row);
            }
        }

        public void mouseDragged(final MouseEvent e) {
            if (SwingUtilities2.shouldIgnore(e, list)) {
                return;
            }

            if (list.getDragEnabled()) {
                DragRecognitionSupport.mouseDragged(e, this);
                return;
            }

            if (e.isShiftDown() || DarkUIUtil.isMenuShortcutKeyDown(e)) {
                return;
            }

            int row = locationToIndex(list, e.getPoint());
            if (row != -1) {
                // 4835633.  Dragging onto a File should not select it.
                if (isFileList) {
                    return;
                }
                Rectangle cellBounds = getCellBounds(list, row, row);
                if (cellBounds != null) {
                    list.scrollRectToVisible(cellBounds);
                    list.setSelectionInterval(row, row);
                }
            }
        }

        public void mouseMoved(final MouseEvent e) {
        }

        public void focusGained(final FocusEvent e) {
            repaintCellFocus();
        }

        /* The focusGained() focusLost() methods run when the JList
         * focus changes.
         */

        //
        // FocusListener
        //
        protected void repaintCellFocus() {
            int leadIndex = adjustIndex(list.getLeadSelectionIndex(), list);
            if (leadIndex != -1) {
                Rectangle r = getCellBounds(list, leadIndex, leadIndex);
                if (r != null) {
                    list.repaint(r.x, r.y, r.width, r.height);
                }
            }
        }

        public void focusLost(final FocusEvent e) {
            repaintCellFocus();
        }
    }
}
