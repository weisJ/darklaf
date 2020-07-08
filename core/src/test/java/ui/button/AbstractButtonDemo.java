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
package ui.button;

import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.util.AlignmentExt;

public abstract class AbstractButtonDemo<T extends AbstractButton> implements ComponentDemo {

    @Override
    public JComponent createComponent() {
        T button = createButton();
        DemoPanel panel = new DemoPanel(button);
        addControls(panel, button);
        return panel;
    }

    protected void addCheckBoxControls(final JPanel controlPanel, final T button) {}

    protected abstract T createButton();

    protected void addControls(final DemoPanel panel, final T button) {
        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(button.isEnabled());
                addActionListener(e -> button.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("focusable") {
            {
                setSelected(button.isFocusable());
                addActionListener(e -> button.setFocusable(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(button.getComponentOrientation().isLeftToRight());
                addActionListener(e -> button.setComponentOrientation(isSelected() ? ComponentOrientation.LEFT_TO_RIGHT
                        : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox("Rollover") {
            {
                setSelected(button.isRolloverEnabled());
                addActionListener(e -> button.setRolloverEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_SQUARE) {
            {
                setSelected(false);
                addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_SQUARE, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_ROUND) {
            {
                setSelected(false);
                addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_ROUND, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_THIN) {
            {
                setSelected(false);
                addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_THIN, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkButtonUI.KEY_ALT_ARC) {
            {
                setSelected(false);
                addActionListener(e -> button.putClientProperty(DarkButtonUI.KEY_ALT_ARC, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Content area filled") {
            {
                setSelected(button.isContentAreaFilled());
                addActionListener(e -> button.setContentAreaFilled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Border painted") {
            {
                setSelected(button.isBorderPainted());
                addActionListener(e -> button.setBorderPainted(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Focus Painted") {
            {
                setSelected(button.isFocusPainted());
                addActionListener(e -> button.setFocusPainted(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Button.defaultButtonFollowsFocus") {
            {
                setSelected(UIManager.getBoolean("Button.defaultButtonFollowsFocus"));
                addActionListener(e -> UIManager.put("Button.defaultButtonFollowsFocus", isSelected()));
            }
        });
        addCheckBoxControls(controlPanel, button);

        controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("Text enabled") {
            {
                setSelected(true);
                addActionListener(e -> button.setText(isSelected() ? "Test Button" : null));
            }
        });
        controlPanel.add(new JCheckBox("Icon enabled") {
            final Icon icon = button.getIcon();
            {
                setEnabled(icon != null);
                setSelected(icon != null);
                addActionListener(e -> button.setIcon(isSelected() ? icon : null));
            }
        });

        controlPanel = panel.addControls();
        controlPanel.add(new QuickColorChooser(DarkButtonUI.KEY_HOVER_COLOR, Color.BLACK,
                                               (b, c) -> button.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR,
                                                                                  b ? c : null)));
        controlPanel.add(new QuickColorChooser(DarkButtonUI.KEY_CLICK_COLOR, Color.BLACK,
                                               (b, c) -> button.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR,
                                                                                  b ? c : null)));

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel(DarkButtonUI.KEY_VARIANT + ":"));
        controlPanel.add(new JComboBox<String>() {
            {
                addItem(DarkButtonUI.VARIANT_NONE);
                addItem(DarkButtonUI.VARIANT_BORDERLESS);
                addItem(DarkButtonUI.VARIANT_BORDERLESS_RECTANGULAR);
                setSelectedItem(DarkButtonUI.VARIANT_NONE);
                addItemListener(e -> button.putClientProperty(DarkButtonUI.KEY_VARIANT, e.getItem()));
            }
        });
        controlPanel.add(new JLabel(DarkButtonUI.KEY_CORNER + ":"));
        controlPanel.add(new JComboBox<String>() {
            {
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
            }
        });
    }
}
