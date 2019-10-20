import com.weis.darklaf.LafManager;
import com.weis.darklaf.components.OverlayScrollPane;
import com.weis.darklaf.components.SelectableTreeNode;
import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.tabframe.JTabFrame;
import com.weis.darklaf.components.tabframe.TabbedPopup;
import com.weis.darklaf.icons.IconLoader;

import javax.swing.*;
import javax.swing.tree.DefaultMutableTreeNode;
import java.awt.*;

/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
public class TabFrameDemo {

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(() -> {
            LafManager.install();

            final JFrame frame = new JFrame();
            Icon folderIcon = IconLoader.get().getUIAwareIcon("files/folder.svg", 19, 19);

            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

            var tabFrame = new JTabFrame();
            for (var o : Alignment.values()) {
                if (o != Alignment.CENTER) {
                    for (int i = 0; i < 2; i++) {
                        var pcc = new JPanel();
                        pcc.setOpaque(true);
                        pcc.add(new JLabel(o.toString() + "_" + i + " Popup"));
                        tabFrame.addTab(pcc, o.toString() + "_" + i, folderIcon, o);
                    }
                }
            }
            var tabbedPopup = new TabbedPopup("Tabbed Popup:");
            tabFrame.setTabAt(tabbedPopup, "NORTH (Tabbed Pane Tab)", null, Alignment.NORTH, 0);
            for (int i = 0; i < 5; i++) {
                var panel = new JPanel();
                var label = new JLabel("Tab Number " + i);
                panel.add(label);
                tabbedPopup.getTabbedPane().addTab("Tab " + i, panel);
            }
            tabFrame.setComponentAt(new JScrollPane(createTree()), Alignment.NORTH_WEST, 0);
            /* Activate for custom tab demo.
            tabFrame.setUserTabComponentAt(new JLabel("NORTH (custom tab)") {{
                setBorder(new EmptyBorder(0, 5, 0, 5));
                setOpaque(false);
                setForeground(Color.RED);
                setFont(new Font(Font.SERIF, Font.ITALIC, 12));
            }}, Alignment.NORTH, 1);
             */
            tabFrame.setAcceleratorAt(1, Alignment.NORTH_WEST, 0);

            var contentPane = new JPanel(new BorderLayout());
            var overlayScroll = new OverlayScrollPane(new JTextPane() {{
                setText(TestResources.LOREM_IPSUM);
                setFont(Font.getFont(Font.MONOSPACED));
            }});
            contentPane.add(overlayScroll, BorderLayout.CENTER);
            frame.setContentPane(tabFrame);
            tabFrame.setContent(contentPane);

            frame.pack();
            frame.setSize(1000, 500);
            frame.setLocationRelativeTo(null);
            frame.setVisible(true);
        });
    }

    protected static JTree createTree() {
        DefaultMutableTreeNode root = new DefaultMutableTreeNode("States");
        DefaultMutableTreeNode parent1 = new DefaultMutableTreeNode("Andhra Pradesh");
        DefaultMutableTreeNode child = new DefaultMutableTreeNode("Vijayawada");
        DefaultMutableTreeNode child1 = new SelectableTreeNode("This node can be selected", true);
        DefaultMutableTreeNode parent2 = new DefaultMutableTreeNode("Telangana");
        DefaultMutableTreeNode child2 = new DefaultMutableTreeNode("Hyderabad");

        // Adding child nodes to parent
        parent1.add(child);
        parent1.add(child1);
        parent2.add(child2);

        // Adding parent nodes to root
        root.add(parent1);
        root.add(parent2);

        // Adding root to JTree
        JTree tree = new JTree(root);
        tree.setEditable(true);
//        tree.setComponentOrientation(ComponentOrientation.RIGHT_TO_LEFT);
        return tree;
    }
}
