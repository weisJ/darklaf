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
import java.awt.*;
import java.awt.event.*;
import java.util.EventObject;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.CellEditorListener;
import javax.swing.tree.*;

public class Test {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame frame = new JFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setContentPane(createContentPane());
            frame.pack();
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }

    private static JComponent createContentPane() {
        return new DemoContent();
    }

    private static void drawRect(final Graphics g, final int x, final int y, final int w, final int h) {
        g.fillRect(x, y, w, 1);
        g.fillRect(x, y, 1, h);
        g.fillRect(x + w - 1, y, 1, h);
        g.fillRect(x, y + h - 1, w, 1);
    }

    private static class TreePopupManager {

        private static final TreePopupManager INSTANCE = new TreePopupManager();

        private final TreePopupContent contentPanel = new TreePopupContent(this::pin);
        private Popup popup;
        private Component owner;
        private Point lastPos;
        private JDialog dialog;

        public TreePopupManager() {
            Toolkit.getDefaultToolkit().addAWTEventListener(e -> {
                if (popup != null) {
                    if (e instanceof FocusEvent) {
                        Component oppositeComp = ((FocusEvent) e).getOppositeComponent();
                        Component comp = ((FocusEvent) e).getComponent();
                        int id = e.getID();
                        if (id == FocusEvent.FOCUS_LOST) {
                            if (oppositeComp == null
                                || !SwingUtilities.isDescendingFrom(oppositeComp, contentPanel)) {
                                hidePopup();
                            }
                        } else if (id == FocusEvent.FOCUS_GAINED) {
                            if (comp == null
                                || !SwingUtilities.isDescendingFrom(comp, contentPanel)) {
                                hidePopup();
                            }
                        }
                    } else if (e instanceof MouseEvent && e.getID() == MouseEvent.MOUSE_PRESSED) {
                        Component component = ((MouseEvent) e).getComponent();
                        if (!SwingUtilities.isDescendingFrom(component, contentPanel)) {
                            hidePopup();
                        }
                    }
                }
            }, AWTEvent.FOCUS_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK);
        }

        private void pin() {
            hidePopup();
            if (dialog == null) {
                dialog = new JDialog(owner != null ? SwingUtilities.getWindowAncestor(owner) : null);
                dialog.setContentPane(contentPanel);
                contentPanel.setPinned(true);
                dialog.pack();
                dialog.setLocation(lastPos);
                dialog.setVisible(true);
            }
        }

        private void hidePopup() {
            if (popup != null) {
                popup.hide();
                popup = null;
            }
        }

        private void hideDialog() {
            if (dialog != null) {
                dialog.setVisible(false);
                dialog.dispose();
                dialog = null;
            }
        }

        public static TreePopupManager getInstance() {
            return INSTANCE;
        }

        public boolean isPopupVisible() {
            return popup != null || dialog != null;
        }

        public void showPopup(final Component parent, final JTree content, final Point p, final String title) {
            contentPanel.setTitle(title);
            contentPanel.setContent(content);
            if (dialog != null && !dialog.isVisible()) {
                hideDialog();
            }
            if (popup == null && dialog == null) {
                contentPanel.setPinned(false);
                lastPos = p;
                owner = parent;
                popup = PopupFactory.getSharedInstance().getPopup(parent, contentPanel, p.x, p.y);
                popup.show();
            }
        }
    }

    private static class TreePopupContent extends JPanel {

        private final JPanel view = new JPanel(new BorderLayout());
        private final JLabel label = new JLabel();
        private final JButton pinButton = new JButton("Pin");

        public TreePopupContent(final Runnable onPin) {
            super(new BorderLayout());
            JScrollPane scrollPane = new JScrollPane(view) {
                @Override
                public void setBorder(final Border border) {}
            };
            add(scrollPane);
            Box box = Box.createHorizontalBox();
            box.add(Box.createHorizontalStrut(5));
            box.add(label);
            box.add(Box.createHorizontalGlue());

            pinButton.setMargin(new Insets(0, 0, 0, 0));
            pinButton.setFocusable(false);
            pinButton.setFocusPainted(false);
            pinButton.addActionListener(e -> onPin.run());

            box.add(pinButton);
            box.add(Box.createHorizontalStrut(5));
            add(box, BorderLayout.NORTH);
        }

        public void setPinned(final boolean pinned) {
            pinButton.setVisible(!pinned);
        }

        @Override
        public void updateUI() {
            super.updateUI();
            setBorder(new JPopupMenu().getBorder());
        }

        public void setTitle(final String title) {
            label.setText(title);
        }

        public void setContent(final JComponent content) {
            setBackground(content.getBackground());
            view.removeAll();
            view.add(content);
            revalidate();
            repaint();
            Dimension pref = view.getPreferredSize();
            setPreferredSize(new Dimension(Math.max(pref.width, 100) + 10,
                                           Math.max(pref.height, 200) + 10));
        }
    }

    private static class DemoContent extends JPanel {

        private DemoContent() {
            setPreferredSize(new Dimension(300, 300));
            setLayout(new BorderLayout());
            setLayout(new GridLayout(2, 2));
            add(new DemoContentPanel(new CheckBoxTree(populate(new TristateNode("root1"), 1, 1))));
            add(new DemoContentPanel(new CheckBoxTree(populate(new TristateNode("root2"), 2, 2))));
            add(new DemoContentPanel(new CheckBoxTree(populate(new TristateNode("root3"), 3, 3))));
            add(new DemoContentPanel(new CheckBoxTree(populate(new TristateNode("root4"), 4, 4))));
        }

        private TristateNode populate(final TristateNode node, final int count, final int depth) {
            if (depth == 0) return node;
            for (int i = 0; i < count; i++) {
                node.add(populate(new TristateNode("Node-" + depth + "-" + i), count, depth - 1));
            }
            return node;
        }
    }

    private static class DemoContentPanel extends JPanel {

        private final CheckBoxTree tree;

        private DemoContentPanel(final CheckBoxTree tree) {
            this.tree = tree;
            setFocusable(true);
            addMouseListener(new MouseAdapter() {
                @Override
                public void mousePressed(final MouseEvent e) {
                    requestFocus();
                    onMousePressed(e);
                }
            });
            addFocusListener(new FocusListener() {
                @Override
                public void focusGained(final FocusEvent e) {
                    repaint();
                }

                @Override
                public void focusLost(final FocusEvent e) {
                    repaint();
                }
            });
        }

        @Override
        protected void paintComponent(final Graphics g) {
            super.paintComponent(g);
            if (hasFocus()) {
                g.setColor(Color.RED);
            } else {
                g.setColor(Color.BLACK);
            }
            drawRect(g, 0, 0, getWidth(), getHeight());
        }

        protected void onMousePressed(final MouseEvent e) {
            SwingUtilities.invokeLater(() -> {
                TreePopupManager manager = TreePopupManager.getInstance();
                if (SwingUtilities.isRightMouseButton(e) || manager.isPopupVisible()) {
                    Point p = e.getPoint();
                    SwingUtilities.convertPointToScreen(p, this);
                    manager.showPopup(this, tree, p, "Tree");
                }
            });
        }
    }

    private static class CheckBoxTree extends JTree {

        public CheckBoxTree(final TristateNode root) {
            super(root);
            setShowsRootHandles(true);
            setCellRenderer(new CheckBoxTreeRenderer());
            setCellEditor(new CheckBoxTreeEditor(this));
            setEditable(true);
        }
    }

    private static class CheckBoxTreeRenderer extends DefaultTreeCellRenderer {

        private final TristateIcon icon = new TristateIcon();

        @Override
        public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel,
                                                      final boolean expanded, final boolean leaf,
                                                      final int row, final boolean hasFocus) {
            Component component = super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
            JLabel label = (JLabel) component; // DefaultTreeCellRenderer uses JLabel as its renderer;
            TristateNode node = (TristateNode) value;
            label.setText(node.getLabel());
            label.setIcon(icon);
            icon.setState(node.getState());
            return label;
        }
    }

    private static class CheckBoxTreeEditor implements TreeCellEditor {

        private final JTree tree;

        public CheckBoxTreeEditor(final JTree tree) {
            this.tree = tree;
        }

        @Override
        public Component getTreeCellEditorComponent(final JTree tree, final Object value,
                                                    final boolean isSelected,
                                                    final boolean expanded,
                                                    final boolean leaf, final int row) {
            return null;
        }

        @Override
        public Object getCellEditorValue() {
            return ((TristateNode) tree.getEditingPath().getLastPathComponent()).getState();
        }

        @Override
        public boolean isCellEditable(final EventObject anEvent) {
            if (!(anEvent instanceof InputEvent)) return false;
            InputEvent event = (InputEvent) anEvent;
            Object source = event.getSource();
            if (!(source instanceof JTree)) return false;
            JTree tree = (JTree) source;
            Object value = null;
            if (event instanceof MouseEvent) {
                Point p = ((MouseEvent) event).getPoint();
                TreePath path = tree.getPathForLocation(p.x, p.y);
                if (path != null) {
                    value = path.getLastPathComponent();
                }
            } else {
                value = tree.getLeadSelectionPath().getLastPathComponent();
            }
            if (value instanceof TristateNode) {
                ((TristateNode) value).iterateState();
                tree.repaint();
            }
            return false;
        }

        @Override
        public boolean shouldSelectCell(final EventObject anEvent) {
            return false;
        }

        @Override
        public boolean stopCellEditing() {
            return false;
        }

        @Override
        public void cancelCellEditing() {}

        @Override
        public void addCellEditorListener(final CellEditorListener l) {}

        @Override
        public void removeCellEditorListener(final CellEditorListener l) {}
    }

    private static class TristateNode extends DefaultMutableTreeNode {
        private String label;

        public TristateNode() {
            this(null);
        }

        public TristateNode(final String label) {
            this(label, TristateState.DESELECTED);
        }

        public TristateNode(final String label, final TristateState state) {
            this(label, state, true);
        }

        public TristateNode(final String label, final TristateState state, final boolean allowsChildren) {
            super();
            parent = null;
            this.allowsChildren = allowsChildren;
            this.userObject = state;
            this.label = label;
        }

        @Override
        public void add(final MutableTreeNode newChild) {
            if (!(newChild instanceof TristateNode)) {
                throw new IllegalArgumentException("Only children of type TristateTreeNode are allowed.");
            }
            super.add(newChild);
        }

        public TristateState getState() {
            return getUserObject();
        }

        public void setState(final TristateState state) {
            setState(state, false, false);
        }

        private void setState(final TristateState state, final boolean invokedByParent, final boolean invokedByChild) {
            if (isLeaf() && state == TristateState.INDETERMINATE) {
                throw new IllegalArgumentException("Leaf nodes cannot have an indeterminate state");
            }
            super.setUserObject(state);
            if (!isLeaf() && !invokedByChild) {
                if (state != TristateState.INDETERMINATE) {
                    for (TreeNode node : children) {
                        if (node instanceof TristateNode) {
                            ((TristateNode) node).setState(state, true, false);
                        }
                    }
                }
            }
            if (!invokedByParent) {
                Object treeNode = getParent();
                if (treeNode instanceof TristateNode) {
                    ((TristateNode) treeNode).setState(((TristateNode) treeNode).getEffectiveState(), false, true);
                }
            }
        }

        public void setSelected(final boolean selected) {
            this.userObject = selected;
        }

        @Override
        public TristateState getUserObject() {
            return (TristateState) super.getUserObject();
        }

        @Override
        public void setUserObject(final Object userObject) {
            if (!(userObject instanceof TristateState)) {
                throw new IllegalArgumentException("Only values of type TristateState are allowed but got "
                                                   + userObject);
            }
            setState((TristateState) userObject);
        }

        public TristateState getEffectiveState() {
            if (isLeaf()) return getState();
            TristateState state = null;
            for (TreeNode node : children) {
                if (node instanceof TristateNode) {
                    TristateState nodeState = ((TristateNode) node).getState();
                    if (state == null) state = nodeState;
                    if (state != nodeState) {
                        state = TristateState.INDETERMINATE;
                        break;
                    }
                }
            }
            return state != null ? state : TristateState.DESELECTED;
        }

        public String getLabel() {
            return label;
        }

        public void setLabel(final String label) {
            this.label = label;
        }

        public void iterateState() {
            TristateState state = getState();
            switch (state) {
                case DESELECTED :
                    setState(TristateState.SELECTED);
                    break;
                case SELECTED :
                    setState(TristateState.DESELECTED);
                    break;
                case INDETERMINATE :
                    setState(state.next());
                    break;
            }
        }
    }

    private enum TristateState {
        DESELECTED() {
            @Override
            public TristateState next() {
                return INDETERMINATE;
            }
        },
        SELECTED() {
            @Override
            public TristateState next() {
                return DESELECTED;
            }
        },
        INDETERMINATE() {
            @Override
            public TristateState next() {
                return SELECTED;
            }
        };

        public abstract TristateState next();
    }

    private static class TristateIcon implements Icon {

        private TristateState state = TristateState.DESELECTED;

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
            switch (state) {
                case DESELECTED :
                    g.setColor(Color.RED);
                    break;
                case SELECTED :
                    g.setColor(Color.GREEN);
                    break;
                case INDETERMINATE :
                    g.setColor(Color.ORANGE);
                    break;
                default :
                    return;
            }
            g.fillRect(x, y, getIconWidth(), getIconHeight());
        }

        public void setState(final TristateState state) {
            this.state = state;
        }

        @Override
        public int getIconWidth() {
            return 16;
        }

        @Override
        public int getIconHeight() {
            return 16;
        }
    }
}
