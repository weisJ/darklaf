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
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreePath;

import com.github.weisj.darklaf.graphics.PaintUtil;
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
public class DarkTreeUI extends BasicTreeUI implements PropertyChangeListener, CellConstants {

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

    protected MouseListener selectionListener;
    protected Color lineColor;
    protected Color focusSelectedLineColor;
    protected Color selectedLineColor;
    protected Icon expandedFocusSelected;
    protected Icon expandedSelected;
    protected Icon expandedFocus;
    protected Icon expanded;
    protected Icon collapsedFocusSelected;
    protected Icon collapsedSelected;
    protected Icon collapsedFocus;
    protected Icon collapsed;

    protected Insets leadSelectionBorderInsets;

    private boolean oldRepaintAllRowValue;

    protected CellHintPopupListener<JTree, ?> popupListener;

    protected DarkTreeCellRendererDelegate rendererDelegate;

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
        tree.putClientProperty(DarkTreeUI.KEY_ALTERNATE_ROW_COLOR,
                               UIManager.getBoolean("Tree.alternateRowColor"));
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
        collapsedFocusSelected = UIManager.getIcon("Tree.collapsed.selected.focused.icon");
        collapsedSelected = UIManager.getIcon("Tree.collapsed.selected.unfocused.icon");
        collapsedFocus = UIManager.getIcon("Tree.collapsed.unselected.focused.icon");
        collapsed = UIManager.getIcon("Tree.collapsed.unselected.unfocused.icon");
        leadSelectionBorderInsets = UIManager.getInsets("Tree.leadSelectionBorderInsets");
        if (leadSelectionBorderInsets == null) leadSelectionBorderInsets = new Insets(1, 1, 1, 1);
        PropertyUtil.installBooleanProperty(tree, KEY_RENDER_BOOLEAN_AS_CHECKBOX, "Tree.renderBooleanAsCheckBox");
        PropertyUtil.installProperty(tree, KEY_BOOLEAN_RENDER_TYPE, UIManager.getString("Tree.booleanRenderType"));
        PropertyUtil.installProperty(tree, KEY_LINE_STYLE, UIManager.getString("Tree.defaultLineStyle"));
        LookAndFeel.installProperty(tree, JTree.SHOWS_ROOT_HANDLES_PROPERTY, true);
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

        if (PropertyUtil.getBooleanProperty(tree, KEY_MAC_ACTIONS_INSTALLED)) return;

        tree.putClientProperty(KEY_MAC_ACTIONS_INSTALLED, Boolean.TRUE);

        installInputMap(tree.getInputMap(JComponent.WHEN_FOCUSED));

        final ActionMap actionMap = tree.getActionMap();

        actionMap.put("expand_or_move_down", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                JTree tree = getTree(e);
                if (tree == null) return;
                int selectionRow = tree.getLeadSelectionRow();
                if (selectionRow == -1) return;

                if (isLeaf(selectionRow) || tree.isExpanded(selectionRow)) {
                    if (!PropertyUtil.getBooleanProperty(tree, KEY_IS_TABLE_TREE)) {
                        moveTo(tree, selectionRow + 1);
                    }
                } else {
                    tree.expandRow(selectionRow);
                }
                tree.repaint();
            }

            public boolean accept(final Object sender) {
                return acceptExpandCollapseAction(sender, false);
            }
        });

        actionMap.put("collapse_or_move_up", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final JTree tree = getTree(e);
                if (tree == null) return;
                int selectionRow = tree.getLeadSelectionRow();
                if (selectionRow == -1) return;

                if (isLeaf(selectionRow) || tree.isCollapsed(selectionRow)) {
                    if (!PropertyUtil.getBooleanProperty(tree, KEY_IS_TABLE_TREE)) {
                        moveTo(tree, selectionRow - 1);
                    }
                } else {
                    tree.collapseRow(selectionRow);
                }
                tree.repaint();
            }

            public boolean accept(final Object sender) {
                return acceptExpandCollapseAction(sender, true);
            }
        });

        actionMap.put("move_down", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                move(getTree(e), 1);
            }
        });

        actionMap.put("move_up", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                move(getTree(e), -1);
            }
        });

        actionMap.put("toggle_edit", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                toggleEdit(getTree(e));
            }
        });
    }

    protected boolean acceptExpandCollapseAction(final Object sender, final boolean collapsed) {
        JTree tree = DarkUIUtil.nullableCast(JTree.class, sender);
        if (tree == null) return false;
        int selectionRow = tree.getLeadSelectionRow();
        if (selectionRow == -1) return false;
        final boolean collapsedOrExpanded = collapsed
                ? tree.isCollapsed(selectionRow)
                : tree.isExpanded(selectionRow);
        if (isLeaf(selectionRow) || collapsedOrExpanded) {
            return !PropertyUtil.getBooleanProperty(tree, KEY_IS_TABLE_TREE);
        }
        return true;
    }

    protected void installInputMap(final InputMap inputMap) {
        inputMap.put(KeyStroke.getKeyStroke("pressed LEFT"), "collapse_or_move_up");
        inputMap.put(KeyStroke.getKeyStroke("pressed RIGHT"), "expand_or_move_down");
        inputMap.put(KeyStroke.getKeyStroke("pressed DOWN"), "move_down");
        inputMap.put(KeyStroke.getKeyStroke("pressed UP"), "move_up");
        inputMap.put(KeyStroke.getKeyStroke("pressed ENTER"), "toggle_edit");
    }

    protected JTree getTree(final ActionEvent e) {
        return DarkUIUtil.nullableCast(JTree.class, e.getSource());
    }

    protected void move(final JTree tree, final int offset) {
        if (tree == null) return;
        int selectionRow = tree.getLeadSelectionRow();
        if (selectionRow == -1) return;
        moveTo(tree, selectionRow + offset);
    }

    protected void moveTo(final JTree tree, final int row) {
        int newRow = Math.max(Math.min(row, tree.getRowCount() - 1), 0);
        tree.setSelectionRow(newRow);
        scrollRowToVisible(tree, newRow);
        tree.repaint();
    }

    protected void scrollRowToVisible(final JTree tree, final int row) {
        Rectangle bounds = tree.getRowBounds(row);
        if (!isLeaf(row)) {
            boolean expanded = tree.isExpanded(row);
            Icon icon = expanded ? getExpandedIcon() : getCollapsedIcon();
            boolean ltr = tree.getComponentOrientation().isLeftToRight();
            int ident = getRightChildIndent();
            int extra = ident - 1 + icon.getIconWidth() / 2;
            if (ltr) bounds.x -= extra;
            bounds.width += extra;
        }
        tree.scrollRectToVisible(bounds);
    }

    protected void toggleEdit(final JTree tree) {
        if (tree == null) return;
        if (tree.isEditing()) {
            stopEditing(tree);
            return;
        }
        int selectionRow = tree.getLeadSelectionRow();
        if (selectionRow == -1) return;
        startEditingAtPath(tree, getPathForRow(tree, selectionRow));
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
            boolean treeRenderer = !treeEditor
                                   && PropertyUtil.getBooleanProperty(owner, DarkTreeUI.KEY_IS_TREE_RENDERER);
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
    }

    @Override
    protected boolean startEditing(final TreePath path, final MouseEvent event) {
        boolean editing = super.startEditing(path, event);
        if (editing) tree.repaint();
        return editing;
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
                if (!paintSingleRow(g, paintBounds, insets, path, row)) {
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

    protected boolean paintSingleRow(final Graphics g, final Rectangle paintBounds, final Insets insets,
                                     final TreePath path, final int row) {
        if (path == null) return false;
        final int xOffset = tree.getParent() instanceof JViewport
                ? ((JViewport) tree.getParent()).getViewPosition().x
                : 0;
        final int containerWidth = tree.getParent() instanceof JViewport
                ? tree.getParent().getWidth()
                : tree.getWidth();
        final Rectangle cellBounds = getPathBounds(path, insets, boundsBuffer);
        if (cellBounds == null) return false;
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

        if (shouldPaintExpandControl(path, row, isExpanded, hasBeenExpanded, isLeaf)) {
            paintExpandControl(g, paintBounds, insets, cellBounds, path, row, isExpanded,
                               hasBeenExpanded, isLeaf);
        }
        paintRow(g, paintBounds, insets, cellBounds, path, row, isExpanded, hasBeenExpanded, isLeaf);

        if (!selected && tree.getLeadSelectionRow() == row && tree.hasFocus()) {
            g.setColor(CellUtil.getTreeBackground(tree, true, row));
            cellBounds.x = xOffset;
            cellBounds.width = containerWidth;
            PaintUtil.drawRect(g, cellBounds, leadSelectionBorderInsets);
        }

        return (cellBounds.y + cellBounds.height) < paintBounds.y + paintBounds.height;
    }

    protected void paintRowBackground(final Graphics g, final Rectangle bounds, final TreePath path,
                                      final int row, final boolean selected) {
        if (path != null) {
            g.setColor(CellUtil.getTreeBackground(tree, selected, row));
            g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
        }
    }

    /*
     * Paint all vertical legs for the whole tree in this row.
     */
    protected void paintVerticalLegs(final Graphics g, final Rectangle clipBounds,
                                     final Rectangle rowBounds,
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
        if (selectedChildOf(path)) {
            if (tree.hasFocus() || tree.isEditing()) {
                return focusSelectedLineColor;
            } else {
                return selectedLineColor;
            }
        }
        return lineColor;
    }

    protected Icon getExpandedIcon(final boolean selected, final boolean focused) {
        if (selected) {
            return focused ? expandedFocusSelected : expandedSelected;
        } else {
            return focused ? expandedFocus : expanded;
        }
    }

    protected Icon getCollapsedIcon(final boolean selected, final boolean focused) {
        if (selected) {
            return focused ? collapsedFocusSelected : collapsedSelected;
        } else {
            return focused ? collapsedFocus : collapsed;
        }
    }

    protected String getLineStyle() {
        return PropertyUtil.getString(tree, KEY_LINE_STYLE, "");
    }

    protected boolean selectedChildOf(final TreePath path) {
        TreePath p = tree.getSelectionPath();
        if (p == null) return false;
        if (p == path) return true;
        if (tree.isExpanded(p)) return false;
        return p.getParentPath() == path;
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

        BasicStroke dashedStroke = new BasicStroke(2, BasicStroke.CAP_BUTT,
                                                   BasicStroke.JOIN_ROUND, 0, new float[]{2.0f}, 0);
        g2d.setStroke(dashedStroke);
        g2d.drawLine(x, y1, x, y2);
        g2d.setStroke(oldStroke);
    }

    @Override
    protected void paintHorizontalPartOfLeg(final Graphics g, final Rectangle clipBounds, final Insets insets,
                                            final Rectangle bounds, final TreePath path, final int row,
                                            final boolean isExpanded, final boolean hasBeenExpanded,
                                            final boolean isLeaf) {

    }

    @Override
    protected void paintVerticalPartOfLeg(final Graphics g, final Rectangle clipBounds,
                                          final Insets insets, final TreePath path) {}

    @Override
    protected void paintExpandControl(final Graphics g, final Rectangle clipBounds, final Insets insets,
                                      final Rectangle bounds, final TreePath path, final int row,
                                      final boolean isExpanded, final boolean hasBeenExpanded, final boolean isLeaf) {
        if (!isLeaf(row)) {
            boolean isPathSelected = tree.isPathSelected(path);
            setExpandedIcon(getExpandedIcon(isPathSelected, tree.hasFocus() || tree.isEditing()));
            setCollapsedIcon(getCollapsedIcon(isPathSelected, tree.hasFocus() || tree.isEditing()));
        }
        super.paintExpandControl(g, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
    }

    @Override
    protected void paintVerticalLine(final Graphics g, final JComponent c, final int x,
                                     final int top, final int bottom) {
        if (isDashedLine()) {
            drawDashedLine(g, x, top, bottom);
        } else {
            g.fillRect(x, top, 1, bottom - top);
        }
    }

    @Override
    protected void completeEditing() {
        /* If should invoke stopCellEditing, try that */
        if (tree.getInvokesStopCellEditing() &&
            stopEditingInCompleteEditing && editingComponent != null) {
            cellEditor.stopCellEditing();
        }
        /*
         * Invoke cancelCellEditing, this will do nothing if stopCellEditing
         * was successful.
         */
        completeEditing(false, true, true);
    }

    @Override
    protected boolean isToggleSelectionEvent(final MouseEvent e) {
        return SwingUtilities.isLeftMouseButton(e)
               && (SystemInfo.isMac ? e.isMetaDown() : e.isControlDown()) && !e.isPopupTrigger();
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

    private abstract static class TreeUIAction extends AbstractAction implements UIResource {}

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
