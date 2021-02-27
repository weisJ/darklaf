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
package ui.button;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

import com.github.weisj.darklaf.components.help.HelpButton;
import com.github.weisj.darklaf.components.help.HelpMenuItem;

public class HelpButtonDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new HelpButtonDemo());
    }

    @Override
    public JComponent createComponent() {
        HelpButton button = new HelpButton();

        DemoPanel panel = new DemoPanel(button);
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
        controlPanel.add(new JCheckBox("Rollover") {
            {
                setSelected(button.isRolloverEnabled());
                addActionListener(e -> button.setRolloverEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Colored Icon") {
            {
                setSelected(button.isUseColoredIcon());
                addActionListener(e -> button.setUseColoredIcon(isSelected()));
            }
        });
        return panel;
    }

    @Override
    public JMenuBar createMenuBar() {
        JMenuBar menuBar = ComponentDemo.super.createMenuBar();
        menuBar.add(new JMenu("Help") {
            {
                add(new HelpMenuItem("View Help"));
            }
        });
        return menuBar;
    }

    @Override
    public String getTitle() {
        return "Help Button Demo";
    }
}
