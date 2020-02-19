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
package ui.tree;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.SelectableTreeNode;
import net.miginfocom.swing.MigLayout;
import ui.ComponentDemo;
import ui.DemoPanel;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import java.awt.*;

public class TreeDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TreeDemo());
    }

    @Override
    public JComponent createComponent() {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("Root");
        DefaultMutableTreeNode parent1 = new DefaultMutableTreeNode("Node A");
        DefaultMutableTreeNode child = new DefaultMutableTreeNode("Leaf A");
        DefaultMutableTreeNode child1 = new SelectableTreeNode("Leaf B (boolean)", true);
        DefaultMutableTreeNode parent2 = new DefaultMutableTreeNode("Node B");
        DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Leaf C");

        parent1.add(child);
        parent1.add(child1);
        parent2.add(child2);
        root.add(parent1);
        root.add(parent2);

        JTree tree = new JTree(root);
        DemoPanel panel = new DemoPanel(new OverlayScrollPane(tree), new BorderLayout());
        JPanel controlPanel = panel.getControls();
        controlPanel.setLayout(new MigLayout("fillx, wrap 2", "[][grow]"));
        controlPanel.add(new JCheckBox("editable") {{
            setSelected(tree.isEditable());
            addActionListener(e -> tree.setEditable(isSelected()));
        }});
        controlPanel.add(new JCheckBox("LeftToRight") {{
            setSelected(tree.getComponentOrientation().isLeftToRight());
            addActionListener(e -> tree.setComponentOrientation(isSelected() ? ComponentOrientation.LEFT_TO_RIGHT
                                                                             : ComponentOrientation.RIGHT_TO_LEFT));
        }});
        controlPanel.add(new JCheckBox("show root handles") {{
            setSelected(tree.getShowsRootHandles());
            addActionListener(e -> tree.setShowsRootHandles(isSelected()));
        }});
        controlPanel.add(new JCheckBox("JTree.alternateRowColor") {{
            setSelected(Boolean.TRUE.equals(tree.getClientProperty("JTree.alternateRowColor")));
            addActionListener(e -> tree.putClientProperty("JTree.alternateRowColor", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JTree.renderBooleanAsCheckBox") {{
            setSelected(Boolean.TRUE.equals(tree.getClientProperty("JTree.renderBooleanAsCheckBox")));
            addActionListener(e -> tree.putClientProperty("JTree.renderBooleanAsCheckBox", isSelected()));
        }}, "span");
        controlPanel.add(new JLabel("JTree.booleanRenderType:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {{
            addItem("checkBox");
            addItem("radioButton");
            setSelectedItem(tree.getClientProperty("JTree.booleanRenderType"));
            addItemListener(e -> tree.putClientProperty("JTree.booleanRenderType", e.getItem()));
        }});
        controlPanel.add(new JLabel("JTree.lineStyle:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {{
            addItem("Dashed");
            addItem("None");
            addItem("Line");
            setSelectedItem("Line");
            addItemListener(e -> tree.putClientProperty("JTree.lineStyle", e.getItem()));
        }});
        tree.setLargeModel(true);
        return panel;
    }

    @Override
    public String getTitle() {
        return "Tree Demo";
    }
}
