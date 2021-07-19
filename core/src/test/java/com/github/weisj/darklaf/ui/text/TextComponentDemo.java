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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.StringUtil;

public abstract class TextComponentDemo<T extends JTextComponent> extends BaseComponentDemo {

    @Override
    public JComponent createComponent() {
        T text = createTextComponent();
        String txt = text.getText();

        if (txt == null || txt.isEmpty()) {
            text.setText(StringUtil.repeat(StringUtil.repeat("Word ", 5) + "\n", 5));
        }

        DemoPanel panel = new DemoPanel(new JScrollPane(text), new BorderLayout(), 10);
        Border textBorder = new DarkTextBorder();
        Border border = text.getBorder();

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(text.isEnabled());
                addActionListener(e -> text.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(PropertyKey.EDITABLE) {
            {
                setSelected(text.isEditable());
                addActionListener(e -> text.setEditable(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(text.getComponentOrientation().isLeftToRight());
                addActionListener(e -> text.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox("text border") {
            {
                setSelected(text.getBorder() instanceof DarkTextBorder);
                addActionListener(e -> text.setBorder(isSelected() ? textBorder : border));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_ROUNDED_SELECTION) {
            {
                setSelected(PropertyUtil.getBooleanProperty(text, DarkTextUI.KEY_ROUNDED_SELECTION));
                addActionListener(e -> text.putClientProperty(DarkTextUI.KEY_ROUNDED_SELECTION, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_EXTEND_LINE_SELECTION) {
            {
                setSelected(PropertyUtil.getBooleanProperty(text, DarkTextUI.KEY_EXTEND_LINE_SELECTION));
                addActionListener(e -> text.putClientProperty(DarkTextUI.KEY_EXTEND_LINE_SELECTION, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_HAS_ERROR) {
            {
                setSelected(PropertyUtil.getBooleanProperty(text, DarkTextUI.KEY_HAS_ERROR));
                addActionListener(e -> text.putClientProperty(DarkTextUI.KEY_HAS_ERROR, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_HAS_WARNING) {
            {
                setSelected(PropertyUtil.getBooleanProperty(text, DarkTextUI.KEY_HAS_WARNING));
                addActionListener(e -> text.putClientProperty(DarkTextUI.KEY_HAS_WARNING, isSelected()));
            }
        });
        return panel;
    }

    protected abstract T createTextComponent();
}
