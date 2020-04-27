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
package ui.list;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.ui.list.DarkListUI;
import com.github.weisj.darklaf.util.PropertyUtil;

public final class ListDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ListDemo());
    }

    @Override
    public JComponent createComponent() {
        String[] week = {"Monday", "Tuesday", "Wednesday",
                         "Thursday", "Friday", "Saturday", "Sunday"};
        JList<String> list = new JList<>(week);
        list.setSelectedIndex(2);
        DemoPanel panel = new DemoPanel(list, new BorderLayout(), 0);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox(DarkListUI.KEY_ALTERNATE_ROW_COLOR) {
            {
                setSelected(PropertyUtil.getBooleanProperty(list, DarkListUI.KEY_ALTERNATE_ROW_COLOR));
                addActionListener(e -> list.putClientProperty(DarkListUI.KEY_ALTERNATE_ROW_COLOR, isSelected()));
            }
        }, "span");

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel("Layout orientation:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                Map<String, Integer> mapping = new HashMap<String, Integer>() {
                    {
                        put("VERTICAL", JList.VERTICAL);
                        put("VERTICAL_WRAP", JList.VERTICAL_WRAP);
                        put("HORIZONTAL_WRAP", JList.HORIZONTAL_WRAP);
                    }
                };
                addItem("VERTICAL");
                addItem("VERTICAL_WRAP");
                addItem("HORIZONTAL_WRAP");
                setSelectedItem("VERTICAL");
                // noinspection MagicConstant
                addItemListener(e -> list.setLayoutOrientation(mapping.get(e.getItem().toString())));
            }
        }, "sgx");
        controlPanel.add(new JLabel("Selection mode:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                Map<String, Integer> mapping = new HashMap<String, Integer>() {
                    {
                        put("SINGLE_SELECTION", ListSelectionModel.SINGLE_SELECTION);
                        put("MULTIPLE_INTERVAL_SELECTION", ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
                        put("SINGLE_INTERVAL_SELECTION", ListSelectionModel.SINGLE_INTERVAL_SELECTION);
                    }
                };
                addItem("SINGLE_SELECTION");
                addItem("MULTIPLE_INTERVAL_SELECTION");
                addItem("SINGLE_INTERVAL_SELECTION");
                setSelectedItem("SINGLE_SELECTION");
                list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
                // noinspection MagicConstant
                addItemListener(e -> list.setSelectionMode(mapping.get(e.getItem().toString())));
            }
        }, "sgx");
        return panel;
    }

    @Override
    public String getTitle() {
        return "List Demo";
    }
}
