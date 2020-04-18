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
package ui.text;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.text.JTextComponent;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.ui.text.DarkTextBorder;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.StringUtil;

public abstract class TextComponentDemo<T extends JTextComponent> implements ComponentDemo {

    @Override
    public JComponent createComponent() {
        T text = createTextComponent();
        text.setText(StringUtil.repeat(StringUtil.repeat("Word ", 5) + "\n", 5));
        DemoPanel panel = new DemoPanel(text);
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
        controlPanel.add(new JCheckBox("text border") {
            {
                setSelected(false);
                addActionListener(e -> text.setBorder(isSelected() ? textBorder : border));
            }
        });
        controlPanel.add(new JCheckBox("JTextComponent.roundedSelection") {
            {
                setSelected(true);
                addActionListener(e -> text.putClientProperty("JTextComponent.roundedSelection", isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("JTextComponent.hasError") {
            {
                addActionListener(e -> text.putClientProperty("JTextComponent.hasError", isSelected()));
            }
        });
        return panel;
    }

    protected abstract T createTextComponent();
}
