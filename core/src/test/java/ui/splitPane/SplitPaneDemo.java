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
package ui.splitPane;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.graphics.ThemedColor;
import com.github.weisj.darklaf.ui.splitpane.DividerStyle;
import com.github.weisj.darklaf.ui.splitpane.SplitPaneConstants;

public final class SplitPaneDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new SplitPaneDemo());
    }

    @Override
    public JComponent createComponent() {
        JSplitPane splitPane = new JSplitPane();
        JPanel leftPanel = new JPanel() {
            {
                setBackground(new ThemedColor("glowError"));
            }
        };
        JPanel rightPanel = new JPanel() {
            {
                setBackground(new ThemedColor("glowFocus"));
            }
        };
        splitPane.setLeftComponent(leftPanel);
        splitPane.setRightComponent(rightPanel);

        DemoPanel panel = new DemoPanel(splitPane, new BorderLayout(), 0);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("ContinuousLayout") {
            {
                setSelected(splitPane.isContinuousLayout());
                addActionListener(e -> splitPane.setContinuousLayout(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("OneTouchExpandable") {
            {
                setSelected(splitPane.isOneTouchExpandable());
                addActionListener(e -> splitPane.setOneTouchExpandable(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(splitPane.isEnabled());
                addActionListener(e -> splitPane.setEnabled(isSelected()));
            }
        });

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel("Orientation:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                Map<String, Integer> mapping = new HashMap<String, Integer>() {
                    {
                        put("VERTICAL_SPLIT", JSplitPane.VERTICAL_SPLIT);
                        put("HORIZONTAL_SPLIT", JSplitPane.HORIZONTAL_SPLIT);
                    }
                };
                addItem("VERTICAL_SPLIT");
                addItem("HORIZONTAL_SPLIT");
                setSelectedItem("HORIZONTAL_SPLIT");
                addItemListener(e -> splitPane.setOrientation(mapping.get(e.getItem().toString())));
            }
        }, "sgx");
        controlPanel.add(new JLabel(SplitPaneConstants.KEY_STYLE + ":", JLabel.RIGHT));
        controlPanel.add(new JComboBox<DividerStyle>() {
            {
                for (DividerStyle style : DividerStyle.values()) {
                    addItem(style);
                }
                setSelectedItem(DividerStyle.get(UIManager.get("SplitPane.defaultDividerStyle")));
                addItemListener(e -> splitPane.putClientProperty(SplitPaneConstants.KEY_STYLE, e.getItem()));
            }
        }, "sgx");
        return panel;
    }

    @Override
    public String getTitle() {
        return "SplitPane Demo";
    }
}
