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
package ui.tabbedPane;

import com.github.weisj.darklaf.util.StringUtil;
import ui.ComponentDemo;
import ui.DemoPanel;

import javax.swing.*;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

public class TabbedPaneDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TabbedPaneDemo());
    }

    @Override
    public JComponent createComponent() {
        JTabbedPane tabbedPane = createTabbedPane();
        tabbedPane.setName("DemoTabbedPane");
        for (int i = 0; i < 10; i++) {
            JTextPane editor = new JTextPane();
            editor.setText(StringUtil.repeat("Demo Content" + "\n", i + 1));
            tabbedPane.addTab("Tab (" + i + ")", editor);
        }
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

        DemoPanel panel = new DemoPanel(tabbedPane, new BorderLayout(), 0);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JLabel("TabLayoutPolicy:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {{
            Map<String, Integer> mapping = new HashMap<String, Integer>() {{
                put("SCROLL_TAB_LAYOUT", JTabbedPane.SCROLL_TAB_LAYOUT);
                put("WRAP_TAB_LAYOUT", JTabbedPane.WRAP_TAB_LAYOUT);
            }};
            addItem("SCROLL_TAB_LAYOUT");
            addItem("WRAP_TAB_LAYOUT");
            setSelectedItem("SCROLL_TAB_LAYOUT");
            //noinspection MagicConstant
            addItemListener(e -> tabbedPane.setTabLayoutPolicy(mapping.get(e.getItem().toString())));
        }}, "sgx");
        controlPanel.add(new JLabel("TabPlacement:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {{
            Map<String, Integer> mapping = new HashMap<String, Integer>() {{
                put("TOP", JTabbedPane.TOP);
                put("BOTTOM", JTabbedPane.BOTTOM);
                put("LEFT", JTabbedPane.LEFT);
                put("RIGHT", JTabbedPane.RIGHT);
            }};
            addItem("TOP");
            addItem("BOTTOM");
            addItem("LEFT");
            addItem("RIGHT");
            setSelectedItem("TOP");
            //noinspection MagicConstant
            addItemListener(e -> tabbedPane.setTabPlacement(mapping.get(e.getItem().toString())));
        }}, "sgx");

        controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("LeftToRight") {{
            setSelected(tabbedPane.getComponentOrientation().isLeftToRight());
            addActionListener(e -> tabbedPane.setComponentOrientation(isSelected() ? ComponentOrientation.LEFT_TO_RIGHT
                                                                                   : ComponentOrientation.RIGHT_TO_LEFT));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.dndEnabled") {{
            setSelected(false);
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.dndEnabled", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.showNewTabButton") {{
            setSelected(false);
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.showNewTabButton", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.leadingComponent") {{
            setSelected(false);
            JLabel leading = new PlaceholderLabel("Leading");
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.leadingComponent",
                                                                isSelected() ? leading : null));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.trailingComponent") {{
            setSelected(false);
            JLabel trailing = new PlaceholderLabel("Trailing");
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.trailingComponent",
                                                                isSelected() ? trailing : null));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.northComponent") {{
            setSelected(false);
            JLabel north = new PlaceholderLabel("North");
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.northComponent",
                                                                isSelected() ? north : null));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.eastComponent") {{
            setSelected(false);
            JLabel east = new PlaceholderLabel("East");
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.eastComponent",
                                                                isSelected() ? east : null));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.southComponent") {{
            setSelected(false);
            JLabel south = new PlaceholderLabel("South");
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.southComponent",
                                                                isSelected() ? south : null));
        }});
        controlPanel.add(new JCheckBox("JTabbedPane.westComponent") {{
            setSelected(false);
            JLabel west = new PlaceholderLabel("West");
            addActionListener(e -> tabbedPane.putClientProperty("JTabbedPane.westComponent",
                                                                isSelected() ? west : null));
        }});
        return panel;
    }

    protected JTabbedPane createTabbedPane() {
        return new JTabbedPane();
    }

    @Override
    public String getTitle() {
        return "TabbPane Demo";
    }

    private static class PlaceholderLabel extends JLabel implements UIResource {
        private PlaceholderLabel(final String title) {
            super(title);
            setBorder(new CompoundBorder(new LineBorder(Color.RED),
                                         new EmptyBorder(5, 5, 5, 5)));
        }
    }
}

