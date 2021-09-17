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
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;
import java.util.Objects;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.ui.HasRendererPane;
import com.github.weisj.darklaf.ui.cell.CellConstants;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.ui.cell.DarkCellRendererPane;
import com.github.weisj.darklaf.ui.cell.hint.CellHintPopupListener;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.SystemInfo;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTreeUI extends BasicTreeUI implements PropertyChangeListener, CellConstants, HasRendererPane {

    protected static final String KEY_PREFIX = "JTree.";
    public static final String KEY_ALTERNATE_ROW_COLOR = KEY_PREFIX + "alternateRowColor";
    public static final String KEY_RENDER_BOOLEAN_AS_CHECKBOX = KEY_PREFIX + "renderBooleanAsCheckBox";
    public static final String KEY_BOOLEAN_RENDER_TYPE = KEY_PREFIX + "booleanRenderType";
    public static final String KEY_LINE_STYLE = KEY_PREFIX + "lineStyle";
    public static final String KEY_MAC_ACTIONS_INSTALLED = "MacTreeUi.actionsInstalled";
    public static final String STYLE_LINE = "line";
    public static final String STYLE_DASHED = "dashed";
    public static final String STYLE_NONE = "none";
    public static final String KEY_IS_TREE_EDITOR = "JComponent.isTreeEditor";
    public static final String KEY_IS_TREE_RENDERER = "JComponent.isTreeRenderer";
    public static final String KEY_IS_TABLE_TREE = "JComponent.isTableTree";

    protected static final Rectangle boundsBuffer = new Rectangle();
    protected static final RotatableIcon paintingIcon = new RotatableIcon();

    protected MouseListener selectionListener;
    protected Color lineColor;
    protected Color focusSelectedLineColor;
    protected Color selectedLineColor;
    protected Icon expandedFocusSelected;
    protected Icon expandedSelected;
    protected Icon expandedFocus;
    protected Icon expanded;
    protected Icon expandedDisabled;
    protected Icon collapsedFocusSelected;
    protected Icon collapsedSelected;
    protected Icon collapsedFocus;
    protected Icon collapsed;
    protected Icon collapsedDisabled;

    protected Insets leadSelectionBorderInsets;

    private boolean oldRepaintAllRowValue;

    protected CellHintPopupListener<JTree, ?> popupListener;

    protected DarkTreeCellRendererDelegate rendererDelegate;

    private int dashLength;
    private int dashGapLength;
    private DarkTreeExpansionAnimationListener treeExpansionAnimationListener;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkTreeUI();
    }

    protected boolean isSelected(final MouseEvent e) {
        final JTree tree = (JTree) e.getSource();
        final int selected = tree.getClosestRowForLocation(e.getX(), e.getY());
        final int[] rows = tree.getSelectionRows();
        if (rows != null) {
            for (int row : rows) {
                if (row == selected) {
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    protected void completeUIInstall() {
        super.completeUIInstall();
        oldRepaintAllRowValue = UIManager.getBoolean("Tree.repaintWholeRow");
        UIManager.put("Tree.repaintWholeRow", true);
        tree.putClientProperty(DarkTreeUI.KEY_ALTERNATE_ROW_COLOR, UIManager.getBoolean("Tree.alternateRowColor"));
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        rendererDelegate = new DarkTreeCellRendererDelegate(null);
        rendererPane = createCellRendererPane();
        LookAndFeel.installColors(tree, "Tree.background", "Tree.foreground");
        focusSelectedLineColor = UIManager.getColor("Tree.lineFocusSelected");
        selectedLineColor = UIManager.getColor("Tree.lineSelected");
        lineColor = UIManager.getColor("Tree.lineUnselected");
        expandedFocusSelected = UIManager.getIcon("Tree.expanded.selected.focused.icon");
        expandedSelected = UIManager.getIcon("Tree.expanded.selected.unfocused.icon");
        expandedFocus = UIManager.getIcon("Tree.expanded.unselected.focused.icon");
        expanded = UIManager.getIcon("Tree.expanded.unselected.unfocused.icon");
        expandedDisabled = UIManager.getIcon("Tree.expanded.disabled.icon");
        collapsedFocusSelected = UIManager.getIcon("Tree.collapsed.selected.focused.icon");
        collapsedSelected = UIManager.getIcon("Tree.collapsed.selected.unfocused.icon");
        collapsedFocus = UIManager.getIcon("Tree.collapsed.unselected.focused.icon");
        collapsed = UIManager.getIcon("Tree.collapsed.unselected.unfocused.icon");
        collapsedDisabled = UIManager.getIcon("Tree.collapsed.disabled.icon");
        leadSelectionBorderInsets = UIManager.getInsets("Tree.leadSelectionBorderInsets");
        if (leadSelectionBorderInsets == null) leadSelectionBorderInsets = new Insets(1, 1, 1, 1);
        PropertyUtil.installBooleanProperty(tree, KEY_RENDER_BOOLEAN_AS_CHECKBOX, "Tree.renderBooleanAsCheckBox");
        PropertyUtil.installProperty(tree, KEY_BOOLEAN_RENDER_TYPE, UIManager.getString("Tree.booleanRenderType"));
        PropertyUtil.installProperty(tree, KEY_LINE_STYLE, UIManager.getString("Tree.defaultLineStyle"));
        LookAndFeel.installProperty(tree, JTree.SHOWS_ROOT_HANDLES_PROPERTY, true);
        dashLength = UIManager.getInt("Tree.dash.length");
        dashGapLength = UIManager.getInt("Tree.dash.gaplength");
    }

    protected CellRendererPane createCellRendererPane() {
        return new DarkCellRendererPane();
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        if (UIManager.getBoolean("Tree.showFullRowInPopup")) {
            popupListener = createPopupMouseListener();
            popupListener.install();
        }
        tree.addPropertyChangeListener(this);
        selectionListener = createMouseSelectionListener();
        tree.addMouseListener(selectionListener);
        this.treeExpansionAnimationListener = createExpansionAnimationListener();
        treeExpansionAnimationListener.install();
    }

    protected DarkTreeExpansionAnimationListener createExpansionAnimationListener() {
        return new DarkTreeExpansionAnimationListener(tree);
    }

    protected MouseListener createMouseSelectionListener() {
        return new DarkTreeMouseListener();
    }

    protected CellHintPopupListener<JTree, ?> createPopupMouseListener() {
        return new CellHintPopupListener<>(new TreeCellContainer(tree, this));
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        TreeUIAction.installActions(tree);
    }

    @Override
    protected FocusListener createFocusListener() {
        return new FocusListener() {

            @Override
            public void focusGained(final FocusEvent e) {
                tree.repaint();
            }

            @Override
            public void focusLost(final FocusEvent e) {
                boolean focused = hasFocus(e != null ? e.getOppositeComponent() : null);
                if (!focused) {
                    tree.stopEditing();
                    tree.repaint();
                }
            }
        };
    }

    protected boolean hasFocus() {
        return hasFocus(null);
    }

    protected boolean hasFocus(final Component other) {
        Component owner = other;
        if (!DarkUIUtil.hasFocus(tree)) {
            if (owner == null) {
                owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
            }
            boolean treeEditor = PropertyUtil.getBooleanProperty(owner, DarkTreeUI.KEY_IS_TREE_EDITOR);
            boolean treeRenderer =
                    !treeEditor && PropertyUtil.getBooleanProperty(owner, DarkTreeUI.KEY_IS_TREE_RENDERER);
            return treeEditor || treeRenderer;
        }
        return true;
    }

    @Override
    protected TreeCellEditor createDefaultCellEditor() {
        if (currentCellRenderer instanceof DefaultTreeCellRenderer) {
            return new DarkDefaultTreeEditor(tree, (DefaultTreeCellRenderer) currentCellRenderer);
        }
        return new DarkDefaultTreeEditor(tree, null);
    }

    @Override
    protected TreeCellRenderer createDefaultCellRenderer() {
        TreeCellRenderer renderer = super.createDefaultCellRenderer();
        rendererDelegate.setDelegate(renderer);
        return renderer;
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
        UIManager.put("Tree.repaintWholeRow", oldRepaintAllRowValue);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        if (popupListener != null) {
            popupListener.uninstall();
            popupListener = null;
        }
        tree.removeMouseListener(selectionListener);
        selectionListener = null;
        tree.removePropertyChangeListener(this);
        treeExpansionAnimationListener.uninstall();
        treeExpansionAnimationListener = null;
    }

    @Override
    protected void uninstallKeyboardActions() {
        super.uninstallKeyboardActions();
        TreeUIAction.uninstallActions(tree);
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        if (tree != c) {
            throw new InternalError("incorrect component");
        }

        // Should never happen if installed for a UI
        if (treeState == null) {
            return;
        }
        Rectangle paintBounds = g.getClipBounds();

        Insets insets = tree.getInsets();
        TreePath initialPath = getClosestPathForLocation(tree, 0, paintBounds.y);
        Enumeration<?> paintingEnumerator = treeState.getVisiblePathsFrom(initialPath);

        if (initialPath != null && paintingEnumerator != null) {
            int row = treeState.getRowForPath(initialPath);

            boolean done = false;
            while (!done && paintingEnumerator.hasMoreElements()) {
                TreePath path = (TreePath) paintingEnumerator.nextElement();
                Rectangle cellBounds = paintSingleRow(g, paintBounds, insets, path, row);
                if (cellBounds == null
                        || (cellBounds.y + cellBounds.height) >= paintBounds.y + paintBounds.height) {
                    done = true;
                }
                row++;
            }
        }
        paintDropLine(g);
        // Empty out the renderer pane, allowing renderers to be gc'ed.
        rendererPane.removeAll();
    }

    public void paintRow(final Graphics g, final int row) {
        TreePath path = getPathForRow(tree, row);
        Rectangle paintBounds = g.getClipBounds();
        Insets insets = tree.getInsets();
        Rectangle rowBounds = getPathBounds(path, insets, boundsBuffer);
        g.translate(0, -rowBounds.y);
        paintBounds.y += rowBounds.y;
        paintSingleRow(g, paintBounds, insets, path, row);
        rendererPane.removeAll();
        g.translate(0, rowBounds.y);
    }

    protected Rectangle paintSingleRow(final Graphics g, final Rectangle paintBounds, final Insets insets,
            final TreePath path, final int row) {
        if (path == null) return null;
        final int xOffset =
                tree.getParent() instanceof JViewport ? ((JViewport) tree.getParent()).getViewPosition().x : 0;
        final int containerWidth =
                tree.getParent() instanceof JViewport ? tree.getParent().getWidth() : tree.getWidth();
        final Rectangle cellBounds = getPathBounds(path, insets, boundsBuffer);
        if (cellBounds == null) return null;
        final int boundsX = cellBounds.x;
        final int boundsWidth = cellBounds.width;

        final boolean selected = tree.isPathSelected(path);

        cellBounds.x = xOffset;
        cellBounds.width = containerWidth;
        paintRowBackground(g, cellBounds, path, row, selected);
        cellBounds.x = boundsX;
        cellBounds.width = boundsWidth;

        if (path.getParentPath() != null) {
            paintVerticalLegs(g, paintBounds, cellBounds, insets, path);
        }

        boolean isLeaf = treeModel.isLeaf(path.getLastPathComponent());
        boolean isExpanded = !isLeaf && treeState.getExpandedState(path);
        boolean hasBeenExpanded = !isLeaf && tree.hasBeenExpanded(path);

        boolean isFocused = hasFocus();
        if (shouldPaintExpandControl(path, row, isExpanded, hasBeenExpanded, isLeaf)) {
            paintExpandControl(g, cellBounds, path, row, isExpanded, isLeaf, isFocused);
        }
        paintRow(g, paintBounds, insets, cellBounds, path, row, isExpanded, hasBeenExpanded, isLeaf);

        if (!selected && tree.getLeadSelectionRow() == row && isFocused) {
            g.setColor(CellUtil.getTreeBackground(tree, true, row));
            cellBounds.x = xOffset;
            cellBounds.width = containerWidth;
            PaintUtil.drawRect(g, cellBounds, leadSelectionBorderInsets);
        }

        return cellBounds;
    }

    protected void paintRowBackground(final Graphics g, final Rectangle bounds, final TreePath path, final int row,
            final boolean selected) {
        if (path != null) {
            g.setColor(CellUtil.getTreeBackground(tree, selected, row));
            g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
        }
    }

    /*
     * Paint all vertical legs for the whole tree in this row.
     */
    protected void paintVerticalLegs(final Graphics g, final Rectangle clipBounds, final Rectangle rowBounds,
            final Insets insets, final TreePath path) {
        if (!shouldPaintLines()) return;
        int depth = path.getPathCount() - 1;
        if (depth == 0 && (!isRootVisible() || !getShowsRootHandles())) {
            // Parent is the root, which isn't visible.
            return;
        }
        int clipLeft = clipBounds.x;
        int clipRight = clipBounds.x + (clipBounds.width - 1);

        TreePath parentPath = path;
        for (int currentDepth = depth - 1; currentDepth >= 0; currentDepth--) {
            if (currentDepth == 0 && !isRootVisible()) continue;

            int lineX = getRowX(-1, currentDepth);
            if (tree.getComponentOrientation().isLeftToRight()) {
                lineX = lineX - getRightChildIndent() + insets.left;
            } else {
                lineX = tree.getWidth() - lineX - insets.right + getRightChildIndent() - 1;
            }

            if (lineX > clipRight || lineX < clipLeft) continue;

            parentPath = parentPath.getParentPath();
            g.setColor(getLineColor(parentPath));
            paintVerticalLine(g, tree, lineX, rowBounds.y, rowBounds.y + rowBounds.height);
        }
    }

    @Override
    public TreeCellRenderer getCellRenderer() {
        return super.getCellRenderer();
    }

    public Component getEditingComponent() {
        return editingComponent;
    }

    public int getEditingRow() {
        return editingRow;
    }

    protected Rectangle getPathBounds(final TreePath path, final Insets insets, Rectangle bounds) {
        bounds = treeState.getBounds(path, bounds);
        if (bounds != null) {
            if (tree.getComponentOrientation().isLeftToRight()) {
                bounds.x += insets.left;
            } else {
                bounds.x = tree.getWidth() - (bounds.x + bounds.width) - insets.right;
            }
            bounds.y += insets.top;
        }
        return bounds;
    }

    @Override
    public void update(final Graphics g, final JComponent c) {
        if (popupListener != null) popupListener.repaint();
        super.update(g, c);
    }

    public CellHintPopupListener<JTree, ?> getPopupListener() {
        return popupListener;
    }

    @Override
    protected void updateRenderer() {
        super.updateRenderer();
        if (currentCellRenderer != rendererDelegate) {
            rendererDelegate.setDelegate(currentCellRenderer);
        }
        currentCellRenderer = rendererDelegate;
    }

    protected boolean shouldPaintLines() {
        return !STYLE_NONE.equals(getLineStyle());
    }

    protected Color getLineColor(final TreePath path) {
        if (isChildOfSelectionPath(path)) {
            if (tree.isEditing() || hasFocus()) {
                return focusSelectedLineColor;
            } else {
                return selectedLineColor;
            }
        }
        return lineColor;
    }

    protected Icon getExpandedIcon(final boolean selected, final boolean focused) {
        if (!tree.isEnabled()) return expandedDisabled;
        if (selected) {
            return focused ? expandedFocusSelected : expandedSelected;
        } else {
            return focused ? expandedFocus : expanded;
        }
    }

    protected Icon getCollapsedIcon(final boolean selected, final boolean focused) {
        if (!tree.isEnabled()) return collapsedDisabled;
        if (selected) {
            return focused ? collapsedFocusSelected : collapsedSelected;
        } else {
            return focused ? collapsedFocus : collapsed;
        }
    }

    protected String getLineStyle() {
        return PropertyUtil.getString(tree, KEY_LINE_STYLE, "");
    }

    protected boolean isChildOfSelectionPath(final TreePath path) {
        TreePath p = tree.isEditing() ? tree.getEditingPath() : tree.getSelectionPath();
        if (p == null) return false;
        if (Objects.equals(p, path)) return true;
        if (tree.isExpanded(p)) return false;
        TreePath parent = p.getParentPath();
        if (parent == null) return false;
        return Objects.equals(parent.getLastPathComponent(), path.getLastPathComponent());
    }

    protected boolean isDashedLine() {
        return STYLE_DASHED.equals(getLineStyle());
    }

    private void drawDashedLine(final Graphics g, final int x, int y1, final int y2) {
        if (y1 >= y2) {
            return;
        }
        y1 += (y1 % 2);
        Graphics2D g2d = (Graphics2D) g;
        Stroke oldStroke = g2d.getStroke();

        float[] dash = {dashLength, dashGapLength};
        float phase = y1 % (dashLength + dashGapLength);
        BasicStroke dashedStroke = new BasicStroke(2, BasicStroke.CAP_BUTT, BasicStroke.JOIN_ROUND, 0, dash, phase);
        g2d.setStroke(dashedStroke);
        g2d.drawLine(x, y1, x, y2);
        g2d.setStroke(oldStroke);
    }

    @Override
    protected void paintHorizontalPartOfLeg(final Graphics g, final Rectangle clipBounds, final Insets insets,
            final Rectangle bounds, final TreePath path, final int row, final boolean isExpanded,
            final boolean hasBeenExpanded, final boolean isLeaf) {}

    @Override
    protected void paintVerticalPartOfLeg(final Graphics g, final Rectangle clipBounds, final Insets insets,
            final TreePath path) {}

    @Override
    protected void paintExpandControl(Graphics g, Rectangle clipBounds, Insets insets, Rectangle bounds, TreePath path,
            int row, boolean isExpanded, boolean hasBeenExpanded, boolean isLeaf) {
        throw new UnsupportedOperationException("Use own implementation");
    }

    protected void paintExpandControl(final Graphics g, final Rectangle bounds, final TreePath path,
            final int row, final boolean isExpanded, final boolean isLeaf, final boolean isFocused) {
        if (!isLeaf(row)) {
            boolean isPathSelected = tree.isPathSelected(path);
            setExpandedIcon(getExpandedIcon(isPathSelected, isFocused || tree.isEditing()));
            setCollapsedIcon(getCollapsedIcon(isPathSelected, isFocused || tree.isEditing()));
        }
        // Draw icons if not a leaf and either hasn't been loaded,
        // or the model child count is > 0.
        if (!isLeaf) {
            int iconCenterX;
            if (tree.getComponentOrientation().isLeftToRight()) {
                iconCenterX = bounds.x - getRightChildIndent() + 1;
            } else {
                iconCenterX = bounds.x + bounds.width + getRightChildIndent() - 1;
            }
            int iconCenterY = bounds.y + (bounds.height / 2);

            if (isExpanded) {
                Icon expIcon = getExpandedIcon();
                if (expIcon != null) {
                    if (Objects.equals(treeExpansionAnimationListener.getAnimationPath(), path)) {
                        paintingIcon.setIcon(expIcon);
                        paintingIcon.setRotation(
                                (treeExpansionAnimationListener.getAnimationState() - 1) * Math.PI / 2.0);
                        expIcon = paintingIcon;
                    }
                    drawCentered(tree, g, expIcon, iconCenterX, iconCenterY);
                }
            } else {
                Icon collIcon = getCollapsedIcon();
                if (collIcon != null) {
                    if (Objects.equals(treeExpansionAnimationListener.getAnimationPath(), path)) {
                        paintingIcon.setIcon(collIcon);
                        paintingIcon.setRotation(
                                (1 - treeExpansionAnimationListener.getAnimationState()) * Math.PI / 2.0);
                        collIcon = paintingIcon;
                    }
                    drawCentered(tree, g, collIcon, iconCenterX, iconCenterY);
                }
            }
        }
    }

    @Override
    protected void paintVerticalLine(final Graphics g, final JComponent c, final int x, final int top,
            final int bottom) {
        if (isDashedLine()) {
            drawDashedLine(g, x, top, bottom);
        } else {
            g.fillRect(x, top, 1, bottom - top);
        }
    }

    @Override
    protected void completeEditing() {
        /* If should invoke stopCellEditing, try that */
        if (tree.getInvokesStopCellEditing() && stopEditingInCompleteEditing && editingComponent != null) {
            cellEditor.stopCellEditing();
        }
        /*
         * Invoke cancelCellEditing, this will do nothing if stopCellEditing was successful.
         */
        completeEditing(false, true, true);
    }

    @Override
    protected boolean isToggleSelectionEvent(final MouseEvent e) {
        return SwingUtilities.isLeftMouseButton(e) && (SystemInfo.isMac ? e.isMetaDown() : e.isControlDown())
                && !e.isPopupTrigger();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (KEY_ALTERNATE_ROW_COLOR.equals(key)) {
            tree.repaint();
        } else if (DarkTreeUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX.equals(key)) {
            tree.repaint();
        } else if (DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE.equals(key)) {
            tree.repaint();
        } else if (KEY_LINE_STYLE.equals(key)) {
            tree.repaint();
        }
    }

    protected static boolean isLeaf(final JTree tree, final int row) {
        TreePath path = tree.getPathForRow(row);
        if (path != null) return tree.getModel().isLeaf(path.getLastPathComponent());
        return true;
    }

    protected void ensureRowsAreVisible(int beginRow, int endRow) {
        if (tree != null && beginRow >= 0 && endRow < getRowCount(tree)) {
            boolean scrollVert = UIManager.getBoolean("Tree.scrollsHorizontallyAndVertically");
            if (beginRow == endRow) {
                Rectangle scrollBounds = getPathBounds(tree, getPathForRow(tree, beginRow));
                if (scrollBounds == null) return;
                if (!scrollVert) {
                    scrollBounds.x = tree.getVisibleRect().x;
                    scrollBounds.width = 1;
                }
                tree.scrollRectToVisible(scrollBounds);
            } else {
                Rectangle beginRect = getPathBounds(tree, getPathForRow(tree, beginRow));
                if (beginRect == null) return;

                Rectangle visRect = DarkUIUtil.applyInsets(tree.getVisibleRect(), tree.getInsets());
                Rectangle testRect = beginRect;
                int beginY = beginRect.y;
                int maxY = visRect.y + visRect.height;

                for (int i = beginRow + 1; i <= endRow; i++) {
                    testRect = getPathBounds(tree, getPathForRow(tree, i));
                    if (testRect == null) return;

                    if ((testRect.y + testRect.height) > maxY) {
                        i = endRow;
                    }
                }
                Rectangle targetRect = new Rectangle(visRect.x, beginY, 1,
                        testRect.y + testRect.height - beginY);
                tree.scrollRectToVisible(targetRect);
            }
        }
    }

    @Override
    public Container getRendererPane() {
        return rendererPane;
    }

    protected static class TreeUIAction extends AbstractAction implements UIResource {

        private final ActionListener actionListener;

        protected TreeUIAction(final ActionListener actionListener) {
            this.actionListener = actionListener;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            actionListener.actionPerformed(e);
        }

        protected static void uninstallActions(final JTree tree) {
            tree.putClientProperty(KEY_MAC_ACTIONS_INSTALLED, Boolean.FALSE);
            final InputMap inputMap = tree.getInputMap(JComponent.WHEN_FOCUSED);
            uninstallInputMap(inputMap);

            final ActionMap actionMap = tree.getActionMap();
            uninstallActionMap(actionMap);
        }

        protected static void installActions(final JTree tree) {
            if (PropertyUtil.getBooleanProperty(tree, KEY_MAC_ACTIONS_INSTALLED)) return;
            tree.putClientProperty(KEY_MAC_ACTIONS_INSTALLED, Boolean.TRUE);

            final InputMap inputMap = tree.getInputMap(JComponent.WHEN_FOCUSED);
            installInputMap(inputMap);

            final ActionMap actionMap = tree.getActionMap();
            installActionMap(tree, actionMap);
        }

        private static void installActionMap(final JTree tree, final ActionMap actionMap) {
            actionMap.put("expand_or_move_down", new TreeUIAction(e -> {
                JTree target = getTree(e);
                if (target == null) return;
                int selectionRow = target.getLeadSelectionRow();
                if (selectionRow == -1) return;

                if (isLeaf(target, selectionRow) || target.isExpanded(selectionRow)) {
                    if (!PropertyUtil.getBooleanProperty(target, KEY_IS_TABLE_TREE)) {
                        moveTo(target, selectionRow + 1);
                    }
                } else {
                    target.expandRow(selectionRow);
                }
                target.repaint();
            }));
            actionMap.put("collapse_or_move_up", new TreeUIAction(e -> {
                final JTree target = getTree(e);
                if (target == null) return;
                int selectionRow = target.getLeadSelectionRow();
                if (selectionRow == -1) return;

                if (!isLeaf(target, selectionRow) && target.isExpanded(selectionRow)) {
                    target.collapseRow(selectionRow);
                } else {
                    if (!PropertyUtil.getBooleanProperty(target, KEY_IS_TABLE_TREE)) {
                        TreePath parentPath = target.getPathForRow(selectionRow).getParentPath();
                        if (parentPath == null) {
                            // No node above. Move to the first node.
                            moveTo(target, 0);
                        } else {

                            moveTo(target, tree.getRowForPath(parentPath));
                        }
                    }
                }
                target.repaint();
            }));
            actionMap.put("move_down", new TreeUIAction(e -> move(getTree(e), 1)));
            actionMap.put("move_up", new TreeUIAction(e -> move(getTree(e), -1)));
            actionMap.put("toggle_edit", new TreeUIAction(e -> toggleEdit(getTree(e))));
        }

        protected static void uninstallActionMap(final ActionMap actionMap) {
            remove(actionMap, "expand_or_move_down");
            remove(actionMap, "collapse_or_move_up");
            remove(actionMap, "move_down");
            remove(actionMap, "move_up");
            remove(actionMap, "toggle_edit");
        }

        protected static void installInputMap(final InputMap inputMap) {
            inputMap.put(KeyStroke.getKeyStroke("pressed LEFT"), "collapse_or_move_up");
            inputMap.put(KeyStroke.getKeyStroke("pressed RIGHT"), "expand_or_move_down");
            inputMap.put(KeyStroke.getKeyStroke("pressed DOWN"), "move_down");
            inputMap.put(KeyStroke.getKeyStroke("pressed UP"), "move_up");
            inputMap.put(KeyStroke.getKeyStroke("pressed ENTER"), "toggle_edit");
        }

        protected static void uninstallInputMap(final InputMap inputMap) {
            remove(inputMap, KeyStroke.getKeyStroke("pressed LEFT"));
            remove(inputMap, KeyStroke.getKeyStroke("pressed RIGHT"));
            remove(inputMap, KeyStroke.getKeyStroke("pressed DOWN"));
            remove(inputMap, KeyStroke.getKeyStroke("pressed UP"));
            remove(inputMap, KeyStroke.getKeyStroke("pressed ENTER"));
        }

        private static void remove(final InputMap inputMap, final KeyStroke ks) {
            Object key = inputMap.get(ks);
            if (key == null || key instanceof UIResource) {
                inputMap.remove(ks);
            }
        }

        private static void remove(final ActionMap actionMap, final String key) {
            Object action = actionMap.get(key);
            if (action == null || action instanceof UIResource) {
                actionMap.remove(key);
            }
        }

        protected static JTree getTree(final ActionEvent e) {
            return DarkUIUtil.nullableCast(JTree.class, e.getSource());
        }

        protected static void toggleEdit(final JTree tree) {
            if (tree == null) return;
            if (tree.isEditing()) {
                tree.stopEditing();
                return;
            }
            int selectionRow = tree.getLeadSelectionRow();
            if (selectionRow == -1) return;
            tree.startEditingAtPath(tree.getPathForRow(selectionRow));
        }

        protected static void move(final JTree tree, final int offset) {
            if (tree == null) return;
            int selectionRow = tree.getLeadSelectionRow();
            if (selectionRow == -1) return;
            moveTo(tree, selectionRow + offset);
        }

        protected static void moveTo(final JTree tree, final int row) {
            int newRow = Math.max(Math.min(row, tree.getRowCount() - 1), 0);
            if (newRow < 0) return;
            tree.setSelectionRow(newRow);
            scrollRowToVisible(tree, newRow);
            tree.repaint();
        }

        protected static void scrollRowToVisible(final JTree tree, final int row) {
            Rectangle bounds = tree.getRowBounds(row);
            boolean expanded = tree.isExpanded(row);
            BasicTreeUI ui = DarkUIUtil.getUIOfType(tree.getUI(), BasicTreeUI.class);
            if (ui != null) {
                Icon icon = expanded ? ui.getExpandedIcon() : ui.getCollapsedIcon();
                boolean ltr = tree.getComponentOrientation().isLeftToRight();
                int ident = ui.getRightChildIndent();
                int extra = ident - 1 + icon.getIconWidth() / 2;
                if (ltr) bounds.x -= extra;
                bounds.width += extra;
            }
            tree.scrollRectToVisible(bounds);
        }
    }

    private class DarkTreeMouseListener extends MouseAdapter {
        boolean handled = false;

        @Override
        public void mousePressed(final MouseEvent e) {
            handled = false;
            tree.repaint();
            if (!isSelected(e)) {
                handled = true;
                handle(e);
            }
        }

        @Override
        public void mouseReleased(final MouseEvent e) {
            if (!handled) {
                handle(e);
            }
        }

        private void handle(final MouseEvent e) {
            final JTree tree = (JTree) e.getSource();
            if (SwingUtilities.isLeftMouseButton(e) && !e.isPopupTrigger()) {
                // if we can't stop any ongoing editing, do nothing
                if (isEditing(tree) && tree.getInvokesStopCellEditing() && !stopEditing(tree)) {
                    return;
                }
                final TreePath pressedPath = getClosestPathForLocation(tree, e.getX(), e.getY());
                if (pressedPath != null) {
                    Rectangle bounds = getPathBounds(tree, pressedPath);
                    if (e.getY() >= bounds.y + bounds.height) {
                        return;
                    }
                    if (bounds.contains(e.getPoint()) || isLocationInExpandControl(pressedPath, e.getX(), e.getY())) {
                        return;
                    }
                    if (tree.getDragEnabled() || !startEditing(pressedPath, e)) {
                        selectPathForEvent(pressedPath, e);
                    }
                }
            }
        }
    }
}
