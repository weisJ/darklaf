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
package ui.tree;

import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.components.filetree.FileTree;
import com.github.weisj.darklaf.components.filetree.WatchFileTree;
import com.github.weisj.darklaf.ui.button.ButtonConstants;
import com.github.weisj.darklaf.ui.tree.DarkTreeUI;

public class FileTreeDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new FileTreeDemo());
    }

    @Override
    public JComponent createComponent() {
        FileTree tree = new WatchFileTree(null, false);
        DemoPanel panel = new DemoPanel(new OverlayScrollPane(tree), new BorderLayout(), 0);
        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("Show hidden files") {
            {
                setSelected(tree.isShowHiddenFiles());
                addActionListener(e -> tree.setShowHiddenFiles(isSelected()));
            }
        });
        controlPanel.add(new JButton("Reload") {
            {
                putClientProperty(ButtonConstants.KEY_THIN, true);
                addActionListener(e -> tree.reload());
            }
        });
        controlPanel = panel.addControls();
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
        return panel;
    }

    @Override
    public String getTitle() {
        return "File Tree Demo";
    }

}
