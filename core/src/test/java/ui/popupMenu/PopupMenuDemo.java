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
package ui.popupMenu;

import ui.ComponentDemo;
import ui.DemoResources;

import javax.swing.*;
import java.awt.*;

public class PopupMenuDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new PopupMenuDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        Icon icon = DemoResources.FOLDER_ICON;
        panel.add(new JLabel("Right click anywhere to open menu.") {{
            setInheritsPopupMenu(true);
        }});
        panel.setPreferredSize(new Dimension(200, 200));
        panel.setComponentPopupMenu(new JPopupMenu() {{
            for (int i = 0; i < 3; i++) {
                add(new JMenu("Menu " + i) {{
                    setIcon(icon);
                    for (int j = 0; j < 2; j++) {
                        add(new JMenu("SubMenu " + j) {{
                            add(new JMenuItem("Item", icon));
                        }});
                        add(new JMenuItem("Item 1"));
                        add(new JMenuItem("Item 2", icon));
                        addSeparator();
                    }
                    add(new JMenuItem("Item 1", icon));
                    add(new JMenuItem("Item 2"));
                    addSeparator();
                }});
                add(new JMenuItem("Item 1", icon) {{
                    setAccelerator(KeyStroke.getKeyStroke("alt A"));
                }});
                add(new JCheckBoxMenuItem("CheckBox"));
                add(new JMenuItem("Item 2") {{
                    setAccelerator(KeyStroke.getKeyStroke("alt shift B"));
                }});
                add(new JRadioButtonMenuItem("RadioButton"));
                add(new JMenuItem("Item 3", icon) {{
                    setAccelerator(KeyStroke.getKeyStroke("alt control shift C"));
                }});
            }
        }});
        return panel;
    }

    @Override
    public JMenuBar createMenuBar() {
        JMenuBar menuBar = new JMenuBar();
        menuBar.add(ComponentDemo.createThemeMenu());
        menuBar.add(new JMenu("CheckBoxes") {{
            for (int i = 0; i < 10; i++) {
                add(new JCheckBoxMenuItem("Item " + i));
            }
        }});
        return menuBar;
    }

    @Override
    public String getTitle() {
        return "PopupMenu Demo";
    }
}
