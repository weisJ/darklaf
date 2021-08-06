/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.popupMenu;

import java.awt.*;
import java.util.Arrays;
import java.util.List;

import javax.swing.*;

import com.github.weisj.darklaf.ui.DemoResources;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class PopupMenuDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new PopupMenuDemo());
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
    public List<JMenu> createMenus() {
        return Arrays.asList(
                new JMenu("Disabled Menus") {
                    {
                        add(new JMenu("No Items"));
                        add(new JMenu("Menu") {
                            {
                                add(new JMenuItem());
                                add(new JMenuItem());
                            }

                            @Override
                            public boolean isEnabled() {
                                return false;
                            }
                        });
                        add(new JMenuItem("Item") {
                            @Override
                            public boolean isEnabled() {
                                return false;
                            }
                        });
                    }
                },
                new JMenu("CheckBoxes") {
                    {
                        for (int i = 0; i < 10; i++) {
                            add(new JCheckBoxMenuItem("Item " + i));
                        }
                    }
                },
                new JMenu("Many Items") {
                    {
                        for (int i = 0; i < 70; i++) {
                            add(new JMenuItem("Item " + i));
                        }
                    }
                });
    }

    @Override
    public String getName() {
        return "PopupMenu Demo";
    }
}
