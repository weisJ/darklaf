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
 */
package com.github.weisj.darklaf.ui.tree;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.SystemInfo;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicTreeUI;
import javax.swing.tree.TreeCellEditor;
import javax.swing.tree.TreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Enumeration;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTreeUI extends BasicTreeUI implements PropertyChangeListener {

    public static final String TREE_TABLE_TREE_KEY = "JTree.treeTableTree";
    public static final String STRIPED_CLIENT_PROPERTY = "JTree.alternateRowColor";

    private final MouseListener selectionListener = new MouseAdapter() {
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
    };
    protected Color alternativeBackground;
    protected Color lineColor;
    protected Color focusSelectedLineColor;
    protected Color selectedLineColor;
    protected Color selectionBackground;
    protected Color focusSelectionBackground;
    protected Icon expandedFocusSelected;
    protected Icon expandedSelected;
    protected Icon expandedFocus;
    protected Icon expanded;
    protected Icon collapsedFocusSelected;
    protected Icon collapsedSelected;
    protected Icon collapsedFocus;
    protected Icon collapsed;
    private boolean myOldRepaintAllRowValue;


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
        myOldRepaintAllRowValue = UIManager.getBoolean("Tree.repaintWholeRow");
        UIManager.put("Tree.repaintWholeRow", true);
        tree.putClientProperty("JTree.alternateRowColor", UIManager.getBoolean("Tree.alternateRowColor"));
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        selectionBackground = UIManager.getColor("Tree.unfocusedSelectionBackground");
        focusSelectionBackground = UIManager.getColor("Tree.selectionBackground");
        focusSelectedLineColor = UIManager.getColor("Tree.lineFocusSelected");
        selectedLineColor = UIManager.getColor("Tree.lineSelected");
        lineColor = UIManager.getColor("Tree.lineUnselected");
        alternativeBackground = UIManager.getColor("Tree.alternateRowBackground");
        expandedFocusSelected = UIManager.getIcon("Tree.expanded.selected.focused.icon");
        expandedSelected = UIManager.getIcon("Tree.expanded.selected.unfocused.icon");
        expandedFocus = UIManager.getIcon("Tree.expanded.unselected.focused.icon");
        expanded = UIManager.getIcon("Tree.expanded.unselected.unfocused.icon");
        collapsedFocusSelected = UIManager.getIcon("Tree.collapsed.selected.focused.icon");
        collapsedSelected = UIManager.getIcon("Tree.collapsed.selected.unfocused.icon");
        collapsedFocus = UIManager.getIcon("Tree.collapsed.unselected.focused.icon");
        collapsed = UIManager.getIcon("Tree.collapsed.unselected.unfocused.icon");
        tree.putClientProperty("JTree.renderBooleanAsCheckBox",
                               UIManager.getBoolean("Tree.renderBooleanAsCheckBox"));
        tree.putClientProperty("JTree.booleanRenderType", UIManager.getString("Tree.booleanRenderType"));
        tree.setShowsRootHandles(true);
        tree.putClientProperty("JTree.lineStyle", "Line");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        tree.addPropertyChangeListener(this);
        tree.addMouseListener(selectionListener);
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();

        if (Boolean.TRUE.equals(tree.getClientProperty("MacTreeUi.actionsInstalled"))) return;

        tree.putClientProperty("MacTreeUi.actionsInstalled", Boolean.TRUE);

        final InputMap inputMap = tree.getInputMap(JComponent.WHEN_FOCUSED);
        inputMap.put(KeyStroke.getKeyStroke("pressed LEFT"), "collapse_or_move_up");
        inputMap.put(KeyStroke.getKeyStroke("pressed RIGHT"), "expand_or_move_down");
        inputMap.put(KeyStroke.getKeyStroke("pressed DOWN"), "move_up");
        inputMap.put(KeyStroke.getKeyStroke("pressed UP"), "move_down");
        inputMap.put(KeyStroke.getKeyStroke("pressed ENTER"), "toggle_edit");

        final ActionMap actionMap = tree.getActionMap();

        final Action expandAction = actionMap.get("expand");
        if (expandAction != null) {
            actionMap.put("expand_or_move_down", new TreeUIAction() {
                @Override
                public void actionPerformed(final ActionEvent e) {
                    final Object source = e.getSource();
                    if (source instanceof JTree) {
                        JTree tree = (JTree) source;
                        int selectionRow = tree.getLeadSelectionRow();
                        if (selectionRow != -1) {
                            TreePath selectionPath = tree.getPathForRow(selectionRow);
                            if (selectionPath != null) {
                                boolean leaf = tree.getModel().isLeaf(selectionPath.getLastPathComponent());
                                if (leaf || tree.isExpanded(selectionRow)) {
                                    int newRow = Math.min(selectionRow + 1, tree.getRowCount() - 1);
                                    tree.setSelectionRow(newRow);
                                    tree.scrollRowToVisible(newRow);
                                    tree.repaint();
                                    return;
                                }
                            }
                        }
                    }
                    expandAction.actionPerformed(e);
                    tree.repaint();
                }
            });
        }

        actionMap.put("collapse_or_move_up", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final Object source = e.getSource();
                if (source instanceof JTree) {
                    JTree tree = (JTree) source;
                    int selectionRow = tree.getLeadSelectionRow();
                    if (selectionRow == -1) return;

                    TreePath selectionPath = tree.getPathForRow(selectionRow);
                    if (selectionPath == null) return;

                    if (tree.getModel().isLeaf(selectionPath.getLastPathComponent()) || tree.isCollapsed(selectionRow)) {
                        final TreePath parentPath = tree.getPathForRow(selectionRow).getParentPath();
                        if (parentPath != null) {
                            if (parentPath.getParentPath() != null || tree.isRootVisible()) {
                                final int parentRow = tree.getRowForPath(parentPath);
                                tree.scrollRowToVisible(parentRow);
                                tree.setSelectionRow(parentRow);
                            }
                        }
                    } else {
                        tree.collapseRow(selectionRow);
                    }
                    tree.repaint();
                }
            }
        });

        actionMap.put("move_up", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final Object source = e.getSource();
                if (source instanceof JTree) {
                    JTree tree = (JTree) source;
                    int selectionRow = tree.getLeadSelectionRow();
                    if (selectionRow == -1) return;
                    int newRow = Math.min(selectionRow + 1, tree.getRowCount() - 1);
                    tree.setSelectionRow(newRow);
                    tree.scrollRowToVisible(newRow);
                    tree.repaint();
                }
            }
        });

        actionMap.put("move_down", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final Object source = e.getSource();
                if (source instanceof JTree) {
                    JTree tree = (JTree) source;
                    int selectionRow = tree.getLeadSelectionRow();
                    if (selectionRow == -1) return;
                    int newRow = Math.max(selectionRow - 1, 0);
                    tree.setSelectionRow(newRow);
                    tree.scrollRowToVisible(newRow);
                    tree.repaint();
                }
            }
        });

        actionMap.put("toggle_edit", new TreeUIAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                final Object source = e.getSource();
                if (source instanceof JTree) {
                    JTree tree = (JTree) source;
                    if (tree.isEditing()) {
                        stopEditing(tree);
                        return;
                    }
                    int selectionRow = tree.getLeadSelectionRow();
                    if (selectionRow == -1) return;
                    startEditingAtPath(tree, getPathForRow(tree, selectionRow));
                }
            }
        });
    }

    @Override
    protected FocusListener createFocusListener() {
        return new FocusListener() {

            boolean focused = false;

            @Override
            public void focusGained(final FocusEvent e) {
                if (!focused) {
                    tree.repaint();
                }
                focused = true;
            }

            @Override
            public void focusLost(final FocusEvent e) {
                tree.stopEditing();
                if (!hasFocus()) {
                    tree.repaint();
                    focused = false;
                }
            }
        };
    }

    protected boolean hasFocus() {
        if (!DarkUIUtil.hasFocus(tree)) {
            Component owner = KeyboardFocusManager.getCurrentKeyboardFocusManager().getFocusOwner();
            boolean treeEditor = owner instanceof JComponent
                    && Boolean.TRUE.equals(((JComponent) owner).getClientProperty("JToggleButton.isTreeCellEditor"));
            boolean treeRenderer = owner instanceof JComponent
                    && Boolean.TRUE.equals(((JComponent) owner).getClientProperty("JToggleButton.isTreeCellRenderer"));
            return owner == null || treeEditor || treeRenderer;
        }
        return true;
    }

    @Override
    protected TreeCellEditor createDefaultCellEditor() {
        if (currentCellRenderer != null && (currentCellRenderer instanceof DarkTreeCellRenderer)) {
            return new DarkDefaultTreeEditor(tree, (DarkTreeCellRenderer) currentCellRenderer);
        }
        return new DarkDefaultTreeEditor(tree, null);
    }

    @Override
    protected TreeCellRenderer createDefaultCellRenderer() {
        return new DarkTreeCellRenderer();
    }

    @Override
    protected void uninstallDefaults() {
        super.uninstallDefaults();
        UIManager.put("Tree.repaintWholeRow", myOldRepaintAllRowValue);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        tree.removeMouseListener(selectionListener);
        tree.removePropertyChangeListener(this);
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
        int row = treeState.getRowForPath(initialPath);
        int endY = paintBounds.y + paintBounds.height;

        drawingCache.clear();

        if (initialPath != null && paintingEnumerator != null) {
            TreePath parentPath = initialPath;

            // Draw the lines, knobs, and rows

            boolean done = false;
            // Information for the node being rendered.
            boolean isExpanded;
            boolean hasBeenExpanded;
            boolean isLeaf;
            Rectangle boundsBuffer = new Rectangle();
            Rectangle bounds;
            TreePath path;
            boolean rootVisible = isRootVisible();

            //Paint row backgrounds
            Enumeration<?> backgroundEnumerator = treeState.getVisiblePathsFrom(initialPath);
            while (backgroundEnumerator != null && backgroundEnumerator.hasMoreElements()) {
                path = (TreePath) backgroundEnumerator.nextElement();
                if (path != null) {
                    isLeaf = treeModel.isLeaf(path.getLastPathComponent());
                    if (isLeaf) {
                        isExpanded = hasBeenExpanded = false;
                    } else {
                        isExpanded = treeState.getExpandedState(path);
                        hasBeenExpanded = tree.hasBeenExpanded(path);
                    }
                    bounds = getPathBounds(path, insets, boundsBuffer);
                    if (bounds == null) return;
                    paintRowBackground(g, paintBounds, insets, bounds, path,
                                       tree.getRowForPath(path), isExpanded, hasBeenExpanded, isLeaf);
                }
            }

            // Find each parent and have them draw a line to their last child
            while (parentPath != null) {
                paintVerticalPartOfLeg(g, paintBounds, insets, parentPath);
                drawingCache.put(parentPath, Boolean.TRUE);
                parentPath = parentPath.getParentPath();
            }

            while (!done && paintingEnumerator.hasMoreElements()) {
                path = (TreePath) paintingEnumerator.nextElement();
                if (path != null) {
                    isLeaf = treeModel.isLeaf(path.getLastPathComponent());
                    if (isLeaf) { isExpanded = hasBeenExpanded = false; } else {
                        isExpanded = treeState.getExpandedState(path);
                        hasBeenExpanded = tree.hasBeenExpanded(path);
                    }
                    bounds = getPathBounds(path, insets, boundsBuffer);
                    if (bounds == null)
                    // This will only happen if the model changes out
                    // from under us (usually in another thread).
                    // Swing isn't multithreaded, but I'll put this
                    // check in anyway.
                    { return; }

                    // See if the vertical line to the parent has been drawn.
                    parentPath = path.getParentPath();
                    if (parentPath != null) {
                        if (drawingCache.get(parentPath) == null) {
                            paintVerticalPartOfLeg(g, paintBounds, insets, parentPath);
                            drawingCache.put(parentPath, Boolean.TRUE);
                        }
                        paintHorizontalPartOfLeg(g, paintBounds, insets, bounds, path, row, isExpanded,
                                                 hasBeenExpanded, isLeaf);
                    } else if (rootVisible && row == 0) {
                        paintHorizontalPartOfLeg(g, paintBounds, insets, bounds, path, row, isExpanded,
                                                 hasBeenExpanded, isLeaf);
                    }
                    if (shouldPaintExpandControl(path, row, isExpanded, hasBeenExpanded, isLeaf)) {
                        paintExpandControl(g, paintBounds, insets, bounds, path, row, isExpanded,
                                           hasBeenExpanded, isLeaf);
                    }
                    paintRow(g, paintBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
                    if ((bounds.y + bounds.height) >= endY) { done = true; }
                } else {
                    done = true;
                }
                row++;
            }
        }
        paintDropLine(g);
        // Empty out the renderer pane, allowing renderers to be gc'ed.
        rendererPane.removeAll();
        drawingCache.clear();
    }

    protected Rectangle getPathBounds(final TreePath path, final Insets insets, Rectangle bounds) {
        bounds = treeState.getBounds(path, bounds);
        if (bounds != null) {
            if (tree.getComponentOrientation().isLeftToRight()) {
                bounds.x += insets.left;
            } else {
                bounds.x = tree.getWidth() - (bounds.x + bounds.width) -
                        insets.right;
            }
            bounds.y += insets.top;
        }
        return bounds;
    }

    protected void paintRowBackground(final Graphics g, final Rectangle clipBounds,
                                      final Insets insets, final Rectangle bounds, final TreePath path,
                                      final int row, final boolean isExpanded,
                                      final boolean hasBeenExpanded, final boolean isLeaf) {
        final int containerWidth = tree.getParent() instanceof JViewport
                                   ? tree.getParent().getWidth() : tree.getWidth();
        final int xOffset = tree.getParent() instanceof JViewport
                            ? ((JViewport) tree.getParent()).getViewPosition().x : 0;

        if (path != null) {
            boolean selected = tree.isPathSelected(path);
            Graphics2D rowGraphics = (Graphics2D) g.create();
            rowGraphics.setClip(clipBounds);

            rowGraphics.setColor(getRowBackground(row, selected));
            rowGraphics.fillRect(xOffset, bounds.y, containerWidth, bounds.height);

            super.paintRow(rowGraphics, clipBounds, insets, bounds, path, row, isExpanded, hasBeenExpanded, isLeaf);
            rowGraphics.dispose();
        }
    }

    protected Color getRowBackground(final int row, final boolean selected) {
        if (selected) {
            boolean isTableTree = Boolean.TRUE.equals(tree.getClientProperty(TREE_TABLE_TREE_KEY));
            return getTreeSelectionBackground(tree.hasFocus() || isTableTree || tree.isEditing());
        }
        if (Boolean.TRUE.equals(tree.getClientProperty(STRIPED_CLIENT_PROPERTY)) && row % 2 == 1) {
            return alternativeBackground;
        } else {
            return tree.getBackground();
        }
    }

    protected boolean shouldPaintLines() {
        return !"None".equals(getLineStyle());
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

    protected Color getTreeSelectionBackground(final boolean focused) {
        return focused ? focusSelectionBackground : selectionBackground;
    }

    protected String getLineStyle() {
        return String.valueOf(tree.getClientProperty("JTree.lineStyle"));
    }

    protected boolean selectedChildOf(final TreePath path) {
        TreePath p = tree.getSelectionPath();
        if (p == null) return false;
        if (p == path) return true;
        if (tree.isExpanded(p)) return false;
        return p.getParentPath() == path;
    }

    protected boolean isDashedLine() {
        return UIManager.getBoolean("Tree.lineTypeDashed") || "Dashed".equals(getLineStyle());
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
                                          final Insets insets, final TreePath path) {
        if (!shouldPaintLines()) return;
        int depth = path.getPathCount() - 1;
        if (depth == 0 && !getShowsRootHandles() && !isRootVisible()) {
            return;
        }
        if (treeModel.isLeaf(path.getLastPathComponent()) || !tree.isExpanded(path)) return;

        int clipLeft = clipBounds.x;
        int clipRight = clipBounds.x + (clipBounds.width - 1);
        int clipTop = clipBounds.y;
        int clipBottom = clipBounds.y + tree.getHeight();

        Rectangle parentBounds = getPathBounds(tree, path);

        int top;
        int bottom;
        int lineX = getRowX(-1, depth);

        if (tree.getComponentOrientation().isLeftToRight()) {
            lineX = lineX - getRightChildIndent() + insets.left;
        } else {
            lineX = tree.getWidth() - lineX - insets.right + getRightChildIndent() - 1;
        }

        if (lineX > clipRight || lineX < clipLeft) return;

        if (parentBounds == null) {
            top = Math.max(insets.top + getVerticalLegBuffer(), clipTop);
        } else {
            top = Math.max(parentBounds.y + parentBounds.height + getVerticalLegBuffer(), clipTop);
        }

        if (depth == 0 && !isRootVisible()) {
            TreeModel model = getModel();

            if (model != null) {
                Object root = model.getRoot();

                if (model.getChildCount(root) > 0) {
                    parentBounds = getPathBounds(tree, path.pathByAddingChild(model.getChild(root, 0)));
                    if (parentBounds != null) {
                        top = Math.max(insets.top + getVerticalLegBuffer(), parentBounds.y + parentBounds.height / 2);
                    }
                }
            }
        }

        int childCount = treeModel.getChildCount(path.getLastPathComponent());
        g.setColor(getLineColor(path));
        for (int i = 0; i < childCount - 1; i++) {
            TreePath childPath = path.pathByAddingChild(treeModel.getChild(path.getLastPathComponent(), i));
            Rectangle childBounds = getPathBounds(tree, childPath);
            if (childBounds != null) {
                bottom = Math.min(childBounds.y + childBounds.height, clipBottom);
                paintVerticalLine(g, tree, lineX, top, bottom);
                top = bottom;
                if (clipBottom < top) return;
            }
        }

        //Descend to deepest last child.
        TreePath lastChildPath = path.pathByAddingChild(treeModel.getChild(path.getLastPathComponent(), childCount - 1));
        while (tree.isExpanded(lastChildPath)) {
            int count = treeModel.getChildCount(lastChildPath.getLastPathComponent());
            lastChildPath = lastChildPath.pathByAddingChild(treeModel.getChild(lastChildPath.getLastPathComponent(),
                                                                               count - 1));
        }
        Rectangle childBounds = getPathBounds(tree, lastChildPath);
        if (childBounds != null) {
            bottom = Math.min(childBounds.y + childBounds.height, clipBottom);
            paintVerticalLine(g, tree, lineX, top, bottom);
        }
    }

    @Override
    protected void paintExpandControl(final Graphics g, final Rectangle clipBounds, final Insets insets,
                                      final Rectangle bounds, final TreePath path, final int row,
                                      final boolean isExpanded, final boolean hasBeenExpanded, final boolean isLeaf) {
        if (!isLeaf(row)) {
            boolean isPathSelected = tree.getSelectionModel().isPathSelected(path);
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
        /* Invoke cancelCellEditing, this will do nothing if stopCellEditing
           was successful. */
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
        if (STRIPED_CLIENT_PROPERTY.equals(key)) {
            tree.repaint();
        } else if ("JTree.renderBooleanAsCheckBox".equals(key)) {
            tree.repaint();
        } else if ("JTree.booleanRenderType".equals(key)) {
            tree.repaint();
        } else if ("JTree.lineStyle".equals(key)) {
            tree.repaint();
        }
    }

    private abstract static class TreeUIAction extends AbstractAction implements UIResource {
    }
}
