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
package ui.text;

import java.awt.*;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.ui.text.DarkTextFieldUI;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public class TextFieldDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TextFieldDemo());
    }

    @Override
    public JComponent createComponent() {
        JTextField textField = createTextField();
        textField.putClientProperty(DarkTextUI.KEY_DEFAULT_TEXT, getDefaultText());
        // textField.setFont(new Font("Roboto", Font.PLAIN, 13));
        DemoPanel panel = new DemoPanel(textField);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(textField.isEnabled());
                addActionListener(e -> textField.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(PropertyKey.EDITABLE) {
            {
                setSelected(textField.isEditable());
                addActionListener(e -> textField.setEditable(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(textField.getComponentOrientation().isLeftToRight());
                addActionListener(e -> textField.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_ROUNDED_SELECTION) {
            {
                setSelected(PropertyUtil.getBooleanProperty(textField, DarkTextUI.KEY_ROUNDED_SELECTION));
                addActionListener(e -> textField.putClientProperty(DarkTextUI.KEY_ROUNDED_SELECTION, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextFieldUI.KEY_VARIANT + " = " + DarkTextFieldUI.VARIANT_SEARCH) {
            {
                addActionListener(e -> textField.putClientProperty(DarkTextFieldUI.KEY_VARIANT,
                        isSelected() ? DarkTextFieldUI.VARIANT_SEARCH : ""));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextFieldUI.KEY_SHOW_CLEAR) {
            {
                textField.addPropertyChangeListener(
                        e -> setSelected(PropertyUtil.getBooleanProperty(textField, DarkTextFieldUI.KEY_SHOW_CLEAR)));
                addActionListener(e -> textField.putClientProperty(DarkTextFieldUI.KEY_SHOW_CLEAR, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_HAS_ERROR) {
            {
                setSelected(PropertyUtil.getBooleanProperty(textField, DarkTextUI.KEY_HAS_ERROR));
                addActionListener(e -> textField.putClientProperty(DarkTextUI.KEY_HAS_ERROR, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(DarkTextUI.KEY_HAS_WARNING) {
            {
                setSelected(PropertyUtil.getBooleanProperty(textField, DarkTextUI.KEY_HAS_WARNING));
                addActionListener(e -> textField.putClientProperty(DarkTextUI.KEY_HAS_WARNING, isSelected()));
            }
        });
        return panel;
    }

    protected String getDefaultText() {
        return "Default Text";
    }

    protected JTextField createTextField() {
        return new JTextField("Demo TextField");
    }

    @Override
    public String getTitle() {
        return "TextField Demo";
    }
}
