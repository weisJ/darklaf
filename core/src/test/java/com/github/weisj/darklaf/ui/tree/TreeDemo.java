/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;

import net.miginfocom.swing.MigLayout;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.SelectableTreeNode;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public class TreeDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new TreeDemo());
    }

    @Override
    public JComponent createComponent() {
        JTree tree = createTree();
        JSplitPane splitPane = new JSplitPane();
        splitPane.setLeftComponent(new OverlayScrollPane(tree));
        splitPane.setRightComponent(new JPanel());
        SwingUtilities.invokeLater(() -> splitPane.setDividerLocation(0.5));
        DemoPanel panel = new DemoPanel(splitPane, new BorderLayout(), 0);
        JPanel controlPanel = panel.addControls();
        controlPanel.setLayout(new MigLayout("fillx, wrap 2", "[][grow]"));
        controlPanel.add(new JCheckBox(PropertyKey.ENABLED) {
            {
                setSelected(tree.isEnabled());
                addActionListener(e -> tree.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(PropertyKey.EDITABLE) {
            {
                setSelected(tree.isEditable());
                addActionListener(e -> {
                    tree.setEditable(isSelected());
                    tree.repaint();
                });
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(tree.getComponentOrientation().isLeftToRight());
                addActionListener(e -> tree.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox("show root") {
            {
                setSelected(tree.isRootVisible());
                addActionListener(e -> tree.setRootVisible(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("show root handles") {
            {
                setSelected(tree.getShowsRootHandles());
                addActionListener(e -> tree.setShowsRootHandles(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTreeUI.KEY_ALTERNATE_ROW_COLOR) {
            {
                setSelected(PropertyUtil.getBooleanProperty(tree, DarkTreeUI.KEY_ALTERNATE_ROW_COLOR));
                addActionListener(e -> tree.putClientProperty(DarkTreeUI.KEY_ALTERNATE_ROW_COLOR, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTreeUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX) {
            {
                setSelected(PropertyUtil.getBooleanProperty(tree, DarkTreeUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX));
                addActionListener(e -> tree.putClientProperty(DarkTreeUI.KEY_RENDER_BOOLEAN_AS_CHECKBOX, isSelected()));
            }
        }, "span");

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel(DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE + ":", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                addItem(DarkTreeUI.RENDER_TYPE_CHECKBOX);
                addItem(DarkTreeUI.RENDER_TYPE_RADIOBUTTON);
                setSelectedItem(tree.getClientProperty(DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE));
                addItemListener(e -> tree.putClientProperty(DarkTreeUI.KEY_BOOLEAN_RENDER_TYPE, e.getItem()));
            }
        });
        controlPanel.add(new JLabel(DarkTreeUI.KEY_LINE_STYLE + ":", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                addItem(DarkTreeUI.STYLE_LINE);
                addItem(DarkTreeUI.STYLE_DASHED);
                addItem(DarkTreeUI.STYLE_NONE);
                setSelectedItem(DarkTreeUI.STYLE_LINE);
                addItemListener(e -> tree.putClientProperty(DarkTreeUI.KEY_LINE_STYLE, e.getItem()));
            }
        });
        tree.setLargeModel(true);
        return panel;
    }

    private JTree createTree() {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Root");
        DefaultMutableTreeNode parent1 = new DefaultMutableTreeNode("Very very very very very long node A");
        DefaultMutableTreeNode child = new DefaultMutableTreeNode("A loooooooooooooooooooooooooooooooooooooong leaf A");
        DefaultMutableTreeNode child1 = new SelectableTreeNode("Leaf B (boolean)", true);
        DefaultMutableTreeNode parent2 = new DefaultMutableTreeNode("Node B");
        DefaultMutableTreeNode child2 =
                new DefaultMutableTreeNode("Leaf that is unnecessary verbose and ridiculously long C");
        DefaultMutableTreeNode parent3 = new DefaultMutableTreeNode("Nested");

        DefaultMutableTreeNode current = parent3;
        for (int i = 0; i < 10; i++) {
            DefaultMutableTreeNode node1 = new DefaultMutableTreeNode("Nested1 " + i);
            DefaultMutableTreeNode node2 = new DefaultMutableTreeNode("Nested2 " + i);
            current.add(node1);
            current.add(node2);
            current = node1;
        }

        parent1.add(child);
        parent1.add(child1);
        parent2.add(child2);
        root.add(parent1);
        root.add(parent2);
        root.add(parent3);

        for (int i = 0; i < 100; i++) {
            root.add(new DefaultMutableTreeNode("Leaf " + i));
        }

        JTree tree = new JTree(root);
        tree.setCellRenderer(new DefaultTreeCellRenderer() {
            @Override
            public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean sel,
                    final boolean expanded, final boolean leaf, final int row, final boolean hasFocus) {
                Component component =
                        super.getTreeCellRendererComponent(tree, value, sel, expanded, leaf, row, hasFocus);
                component.setEnabled(tree.isEnabled() && value != parent1 && value != child);
                return component;
            }
        });
        return tree;
    }

    @Override
    public String getName() {
        return "Tree Demo";
    }
}
