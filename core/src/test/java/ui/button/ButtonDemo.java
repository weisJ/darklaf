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
package ui.button;

import com.github.weisj.darklaf.icons.IconLoader;
import ui.ComponentDemo;
import ui.DemoPanel;
import ui.QuickColorChooser;

import javax.swing.*;
import java.awt.*;

public class ButtonDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ButtonDemo());
    }

    @Override
    public JComponent createComponent() {
        Icon icon = IconLoader.get().getIcon("files/folder.svg", 19, 19, true);
        JButton button = new JButton("Test Button", icon);
        DemoPanel panel = new DemoPanel(button);
        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {{
            setSelected(button.isEnabled());
            addActionListener(e -> button.setEnabled(isSelected()));
        }});
        controlPanel.add(new JCheckBox("default") {{
            setSelected(button.isDefaultButton());
            addActionListener(e -> SwingUtilities.getRootPane(button).setDefaultButton(isSelected() ? button
                                                                                                    : null));
        }});
        controlPanel.add(new JCheckBox("LeftToRight") {{
            setSelected(button.getComponentOrientation().isLeftToRight());
            addActionListener(e -> button.setComponentOrientation(isSelected() ? ComponentOrientation.LEFT_TO_RIGHT
                                                                               : ComponentOrientation.RIGHT_TO_LEFT));
        }});
        controlPanel.add(new JCheckBox("Rollover") {{
            setSelected(button.isRolloverEnabled());
            addActionListener(e -> button.setRolloverEnabled(isSelected()));
        }});
        controlPanel.add(new JCheckBox("JButton.square") {{
            setSelected(false);
            addActionListener(e -> button.putClientProperty("JButton.square", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JButton.thin") {{
            setSelected(false);
            addActionListener(e -> button.putClientProperty("JButton.thin", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JButton.alternativeArc") {{
            setSelected(false);
            addActionListener(e -> button.putClientProperty("JButton.alternativeArc", isSelected()));
        }});
        controlPanel.add(new JCheckBox("Button.defaultButtonFollowsFocus") {{
            setSelected(UIManager.getBoolean("Button.defaultButtonFollowsFocus"));
            addActionListener(e -> UIManager.put("Button.defaultButtonFollowsFocus", isSelected()));
        }});

        controlPanel = panel.addControls();
        controlPanel.add(new QuickColorChooser("JButton.shadow.hover", Color.BLACK,
                                               (b, c) -> button.putClientProperty("JButton.shadow.hover", b ? c : null)));
        controlPanel.add(new QuickColorChooser("JButton.shadow.click", Color.BLACK,
                                               (b, c) -> button.putClientProperty("JButton.shadow.click", b ? c : null)));

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel("JButton.variant:"));
        controlPanel.add(new JComboBox<String>() {{
            addItem("onlyLabel");
            addItem("shadow");
            addItem("fullShadow");
            addItem("none");
            setSelectedItem("no JButton.variant");
            addItemListener(e -> {
                if (e.getItem().equals("none")) {
                    button.putClientProperty("JButton.variant", null);
                } else {
                    button.putClientProperty("JButton.variant", e.getItem());
                }
            });
        }});
        return panel;
    }

    @Override
    public String getTitle() {
        return "Button Demo";
    }

}
