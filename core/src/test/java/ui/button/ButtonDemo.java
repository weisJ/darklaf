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
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.util.AlignmentExt;
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
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_SQUARE) {{
            setSelected(false);
            addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_SQUARE, isSelected()));
        }});
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_THIN) {{
            setSelected(false);
            addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_THIN, isSelected()));
        }});
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_ALT_ARC) {{
            setSelected(false);
            addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_ALT_ARC, isSelected()));
        }});
        controlPanel.add(new JCheckBox("Button.defaultButtonFollowsFocus") {{
            setSelected(UIManager.getBoolean("Button.defaultButtonFollowsFocus"));
            addActionListener(e -> UIManager.put("Button.defaultButtonFollowsFocus", isSelected()));
        }});

        controlPanel = panel.addControls();
        controlPanel.add(new QuickColorChooser(DarkButtonUI.KEY_HOVER_COLOR, Color.BLACK,
                                               (b, c) -> button.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, b ? c : null)));
        controlPanel.add(new QuickColorChooser(DarkButtonUI.KEY_HOVER_COLOR, Color.BLACK,
                                               (b, c) -> button.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, b ? c : null)));

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel(DarkButtonUI.KEY_VARIANT + ":"));
        controlPanel.add(new JComboBox<String>() {{
            addItem(DarkButtonUI.VARIANT_NONE);
            addItem(DarkButtonUI.VARIANT_SHADOW);
            addItem(DarkButtonUI.VARIANT_FULL_SHADOW);
            addItem(DarkButtonUI.VARIANT_ONLY_LABEL);
            setSelectedItem(DarkButtonUI.VARIANT_NONE);
            addItemListener(e -> button.putClientProperty(DarkButtonUI.KEY_VARIANT, e.getItem()));
        }});
        controlPanel.add(new JLabel(DarkButtonUI.KEY_CORNER + ":"));
        controlPanel.add(new JComboBox<String>() {{
            addItem("None");
            for (AlignmentExt a : AlignmentExt.values()) {
                addItem(a.name());
            }
            setSelectedItem("None");
            addItemListener(e -> {
                if ("None".equals(e.getItem())) {
                    button.putClientProperty(DarkButtonUI.KEY_CORNER, null);
                } else {
                    button.putClientProperty(DarkButtonUI.KEY_CORNER, AlignmentExt.valueOf(e.getItem().toString()));
                }
            });
        }});
        controlPanel.add(new JCheckBox("Icon Only") {{
            setSelected(false);
            addActionListener(e -> button.setText(isSelected() ? null : "Test Button"));
        }});
        return panel;
    }

    @Override
    public String getTitle() {
        return "Button Demo";
    }

}
