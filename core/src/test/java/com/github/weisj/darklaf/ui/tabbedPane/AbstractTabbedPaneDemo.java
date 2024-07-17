/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
package com.github.weisj.darklaf.ui.tabbedPane;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.border.LineBorder;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.tabbedpane.DarkTabbedPaneUI;
import com.github.weisj.darklaf.util.StringUtil;

public abstract class AbstractTabbedPaneDemo<T extends JTabbedPane> extends BaseComponentDemo {

    @Override
    public JComponent createComponent() {
        T tabbedPane = createTabbedPane();
        setupTabbedPane(tabbedPane);
        DemoPanel panel = new DemoPanel(tabbedPane, new BorderLayout(), 0);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JLabel("TabLayoutPolicy:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                Map<String, Integer> mapping = new HashMap<>() {
                    {
                        put("SCROLL_TAB_LAYOUT", JTabbedPane.SCROLL_TAB_LAYOUT);
                        put("WRAP_TAB_LAYOUT", JTabbedPane.WRAP_TAB_LAYOUT);
                    }
                };
                addItem("SCROLL_TAB_LAYOUT");
                addItem("WRAP_TAB_LAYOUT");
                setSelectedItem("SCROLL_TAB_LAYOUT");
                addItemListener(e -> tabbedPane.setTabLayoutPolicy(mapping.get(e.getItem().toString())));
            }
        }, "sgx");
        controlPanel.add(new JLabel("TabPlacement:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                Map<String, Integer> mapping = new HashMap<>() {
                    {
                        put("TOP", JTabbedPane.TOP);
                        put("BOTTOM", JTabbedPane.BOTTOM);
                        put("LEFT", JTabbedPane.LEFT);
                        put("RIGHT", JTabbedPane.RIGHT);
                    }
                };
                addItem("TOP");
                addItem("BOTTOM");
                addItem("LEFT");
                addItem("RIGHT");
                setSelectedItem("TOP");
                addItemListener(e -> tabbedPane.setTabPlacement(mapping.get(e.getItem().toString())));
            }
        }, "sgx");

        controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(tabbedPane.isEnabled());
                addActionListener(e -> tabbedPane.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(tabbedPane.getComponentOrientation().isLeftToRight());
                addActionListener(e -> tabbedPane.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_CENTER_TABS) {
            {
                setSelected(false);
                addActionListener(e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_CENTER_TABS, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_DND) {
            {
                setSelected(false);
                addActionListener(e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_DND, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_SHOW_NEW_TAB_BUTTON) {
            {
                setSelected(false);
                addActionListener(
                        e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_SHOW_NEW_TAB_BUTTON, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_LEADING_COMP) {
            {
                setSelected(false);
                JLabel leading = new PlaceholderLabel("Leading");
                addActionListener(e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_LEADING_COMP,
                        isSelected() ? leading : null));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_TRAILING_COMP) {
            {
                setSelected(false);
                JLabel trailing = new PlaceholderLabel("Trailing");
                addActionListener(e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_TRAILING_COMP,
                        isSelected() ? trailing : null));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_NORTH_COMP) {
            {
                setSelected(false);
                JLabel north = new PlaceholderLabel("North");
                addActionListener(e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_NORTH_COMP,
                        isSelected() ? north : null));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_EAST_COMP) {
            {
                setSelected(false);
                JLabel east = new PlaceholderLabel("East");
                addActionListener(
                        e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_EAST_COMP, isSelected() ? east : null));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_SOUTH_COMP) {
            {
                setSelected(false);
                JLabel south = new PlaceholderLabel("South");
                addActionListener(e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_SOUTH_COMP,
                        isSelected() ? south : null));
            }
        });
        controlPanel.add(new JCheckBox(DarkTabbedPaneUI.KEY_WEST_COMP) {
            {
                setSelected(false);
                JLabel west = new PlaceholderLabel("West");
                addActionListener(
                        e -> tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_WEST_COMP, isSelected() ? west : null));
            }
        });
        return panel;
    }

    protected abstract int getTabCount();

    protected void setupTabbedPane(final T tabbedPane) {
        tabbedPane.setName("DemoTabbedPane");
        for (int i = 0; i < getTabCount(); i++) {
            tabbedPane.addTab(getTabTitle(i), createTabContentPane(i));
        }

        JLabel label = new JLabel("Custom Tab");
        label.setForeground(Color.RED);
        tabbedPane.setTabComponentAt(0, label);

        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        tabbedPane.putClientProperty(DarkTabbedPaneUI.KEY_NEW_TAB_ACTION, createNewTabAction(tabbedPane));
    }

    protected JComponent createTabContentPane(final int tab) {
        JTextPane editor = new JTextPane();
        editor.setText(getTabContent(tab));
        return editor;
    }

    protected Action createNewTabAction(final T tabbedPane) {
        return new AbstractAction() {

            private int tabCounter = tabbedPane.getTabCount();

            @Override
            public void actionPerformed(final ActionEvent e) {
                tabbedPane.addTab(getTabTitle(tabCounter), createTabContentPane(tabCounter));
                tabCounter++;
            }
        };
    }

    protected String getTabContent(final int tab) {
        return StringUtil.repeat("Demo Content" + "\n", tab + 1);
    }

    protected String getTabTitle(final int tab) {
        return "Tab (" + tab + ")";
    }

    protected abstract T createTabbedPane();

    private static class PlaceholderLabel extends JLabel implements UIResource {
        private PlaceholderLabel(final String title) {
            super(title);
            setBorder(new CompoundBorder(new LineBorder(Color.RED), new EmptyBorder(5, 5, 5, 5)));
        }
    }
}
