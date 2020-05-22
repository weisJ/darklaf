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
package ui.popupMenu;

import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoResources;

public class PopupMenuDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new PopupMenuDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel leftPanel = new JPanel();
        leftPanel.setPreferredSize(new Dimension(200, 200));
        leftPanel.setLayout(new GridBagLayout());
        leftPanel.add(new JLabel("<html>Right click anywhere <br> to open menu.") {
            {
                setInheritsPopupMenu(true);
            }
        });

        JPanel rightPanel = new JPanel();
        rightPanel.setPreferredSize(new Dimension(200, 200));
        rightPanel.setLayout(new GridBagLayout());
        rightPanel.add(new JLabel("<html>Right click anywhere <br> to open large menu.") {
            {
                setInheritsPopupMenu(true);
            }
        });

        Icon icon = DemoResources.FOLDER_ICON;
        leftPanel.setComponentPopupMenu(new JPopupMenu() {
            {
                for (int i = 0; i < 3; i++) {
                    add(new JMenu("Menu " + i) {
                        {
                            setIcon(icon);
                            for (int j = 0; j < 2; j++) {
                                add(new JMenu("SubMenu " + j) {
                                    {
                                        add(new JMenuItem("Item", icon));
                                    }
                                });
                                add(new JMenuItem("Item 1"));
                                add(new JMenuItem("Item 2", icon));
                                addSeparator();
                            }
                            add(new JMenuItem("Item 1", icon));
                            add(new JMenuItem("Item 2"));
                            addSeparator();
                        }
                    });
                    addSeparator();
                    add(new JMenuItem("Item 1", icon) {
                        {
                            setAccelerator(KeyStroke.getKeyStroke("alt A"));
                        }
                    });
                    add(new JCheckBoxMenuItem("CheckBox"));
                    add(new JMenuItem("Item 2") {
                        {
                            setAccelerator(KeyStroke.getKeyStroke("alt shift B"));
                        }
                    });
                    add(new JRadioButtonMenuItem("RadioButton"));
                    add(new JMenuItem("Item 3", icon) {
                        {
                            setAccelerator(KeyStroke.getKeyStroke("alt control shift C"));
                        }
                    });
                }
            }
        });
        rightPanel.setComponentPopupMenu(new JPopupMenu() {
            {
                setName("Right Menu");
                for (int i = 0; i < 70; i++) {
                    if (i % 20 == 0) {
                        add(new JMenu("Menu " + i) {
                            {
                                setName(getText());
                                for (int j = 0; j < 5; j++) {
                                    add(new JMenuItem("Item " + j));
                                }
                            }
                        });
                    } else {
                        add(new JMenuItem("Item " + i) {
                            {
                                setName(getText());
                            }
                        });
                    }
                }

            }
        });

        return new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, leftPanel, rightPanel);
    }

    @Override
    public JMenuBar createMenuBar() {
        JMenuBar menuBar = ComponentDemo.getDefaultMenuBar();
        menuBar.add(new JMenu("CheckBoxes") {
            {
                for (int i = 0; i < 10; i++) {
                    add(new JCheckBoxMenuItem("Item " + i));
                }
            }
        });
        menuBar.add(new JMenu("Many Items") {
            {
                for (int i = 0; i < 70; i++) {
                    add(new JMenuItem("Item " + i));
                }
            }
        });
        return menuBar;
    }

    @Override
    public String getTitle() {
        return "PopupMenu Demo";
    }
}
