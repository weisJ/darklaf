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
package ui.treetable;

import java.awt.*;
import java.util.Date;
import java.util.Random;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.components.treetable.JTreeTable;
import com.github.weisj.darklaf.components.treetable.model.*;

public class TreeTableDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TreeTableDemo());
    }

    @Override
    public JComponent createComponent() {
        AbstractTreeTableModel treeTableModel = new DemoModel(createDataStructure());
        JTreeTable treeTable = new JTreeTable(treeTableModel);
        return new DemoPanel(new JScrollPane(treeTable), new BorderLayout(), 0);
    }

    private DemoNode createDataStructure() {
        Random r = new Random();
        DemoNode root = new DemoNode(null, "R1", "R1", new Date(r.nextLong()), 10);
        for (int i = 0; i < 16; i++) {
            DefaultTreeTableNode node = new DemoNode(root, "N" + i, "C1", new Date(r.nextLong()), 10);
            if (i % 2 == 0) {
                node.addChild(new DemoNode(node, "N12N12N12", "C12", new Date(r.nextLong()), 50));
                node.addChild(new DemoNode(node, "N13N12N12", "C13", new Date(r.nextLong()), 60));
                node.addChild(new DemoNode(node, "N14N12N12", "C14", new Date(r.nextLong()), 70));
                node.addChild(new DemoNode(node, "N15N12N12", "C15", new Date(r.nextLong()), 80));
            } else {
                node.addChild(new DemoNode(node, "N12N12N12", "C12", new Date(r.nextLong()), 10));
                node.addChild(new DemoNode(node, "N13N12N12", "C13", new Date(r.nextLong()), 20));
                node.addChild(new DemoNode(node, "N14N12N12", "C14", new Date(r.nextLong()), 30));
                node.addChild(new DemoNode(node, "N15N12N12", "C15", new Date(r.nextLong()), 40));
            }
            root.addChild(node);
        }
        return root;
    }

    @Override
    public String getTitle() {
        return "TreeTable Demo";
    }

    protected static class DemoNode extends DefaultTreeTableNode {

        public DemoNode(final TreeTableNode parent, final String name, final String capital, final Date declared,
                final Integer area) {
            super(parent, new Object[] {name, capital, declared, area});
        }
    }

    protected static class DemoModel extends DefaultTreeTableModel {
        protected static String[] columnNames = {"TreeNode", "String", "Date", "Integer"};
        protected static Class<?>[] columnTypes = {TreeTableModel.class, String.class, Date.class, Integer.class};

        public DemoModel(final TreeTableNode rootNode) {
            super(rootNode, columnNames);
            setColumnClasses(columnTypes);
            root = rootNode;
        }
    }
}
