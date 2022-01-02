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

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.tree.TristateTreeNode;
import com.github.weisj.darklaf.components.tristate.TristateState;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.util.StringUtil;

public class CheckBoxTreeDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new CheckBoxTreeDemo());
    }

    @Override
    public JComponent createComponent() {
        TristateTreeNode root = populate(new TristateTreeNode("Root"), 5, 5);
        JTree tree = new JTree(root);
        tree.setEditable(true);
        root.setState(TristateState.SELECTED);
        return new DemoPanel(new OverlayScrollPane(tree), new BorderLayout(), 0);
    }

    private TristateTreeNode populate(final TristateTreeNode node, final int count, final int depth) {
        if (depth == 0) return node;
        for (int i = 0; i < count; i++) {
            node.add(populate(new TristateTreeNode(StringUtil.repeat("Node", depth)), count, depth - 1));
        }
        return node;
    }

    @Override
    public String getName() {
        return "Checkbox Tree Demo";
    }
}
