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
package ui.text;

import ui.ComponentDemo;
import ui.DemoPanel;

import javax.swing.*;
import java.awt.*;

public class PasswordFieldDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new PasswordFieldDemo());
    }

    @Override
    public JComponent createComponent() {
        JPasswordField textField = new JPasswordField("Demo PasswordField");
        DemoPanel panel = new DemoPanel(textField);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {{
            setSelected(textField.isEnabled());
            addActionListener(e -> textField.setEnabled(isSelected()));
        }});
        controlPanel.add(new JCheckBox("editable") {{
            setSelected(textField.isEditable());
            addActionListener(e -> textField.setEditable(isSelected()));
        }});
        controlPanel.add(new JCheckBox("LeftToRight") {{
            setEnabled(true);
            addActionListener(e -> textField.setComponentOrientation(isSelected() ? ComponentOrientation.LEFT_TO_RIGHT
                                                                                  : ComponentOrientation.RIGHT_TO_LEFT));
        }});
        controlPanel.add(new JCheckBox("JTextComponent.roundedSelection") {{
            setSelected(true);
            addActionListener(e -> textField.putClientProperty("JTextComponent.roundedSelection", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JPasswordField.showViewIcon") {{
            addActionListener(e -> textField.putClientProperty("JPasswordField.showViewIcon", isSelected()));
        }});
        controlPanel.add(new JCheckBox("JTextComponent.hasError") {{
            addActionListener(e -> textField.putClientProperty("JTextComponent.hasError", isSelected()));
        }});
        return panel;
    }

    @Override
    public String getTitle() {
        return "PasswordField Demo";
    }
}
