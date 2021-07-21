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
package com.github.weisj.darklaf.ui.spinner;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class SpinnerDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new SpinnerDemo());
    }

    @Override
    public JComponent createComponent() {
        JSpinner spinner = new JSpinner() {

            @Override
            public void updateUI() {
                super.updateUI();
                ((JFormattedTextField) getEditor().getComponent(0)).setColumns(3);
            }
        };
        DemoPanel panel = new DemoPanel(spinner);
        JPanel controlPanel = panel.addControls(1);
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(spinner.isEnabled());
                addActionListener(e -> spinner.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(spinner.getComponentOrientation().isLeftToRight());
                addActionListener(e -> spinner.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox(SpinnerConstants.KEY_VARIANT + " = " + SpinnerConstants.VARIANT_PLUS_MINUS) {
            {
                addActionListener(e -> spinner.putClientProperty(SpinnerConstants.KEY_VARIANT,
                        isSelected() ? SpinnerConstants.VARIANT_PLUS_MINUS : null));
            }
        });
        controlPanel.add(new JCheckBox(SpinnerConstants.KEY_IS_TREE_EDITOR) {
            {
                setSelected(false);
                addActionListener(e -> spinner.putClientProperty(SpinnerConstants.KEY_IS_TREE_EDITOR, isSelected()));
            }
        });
        controlPanel.add(new JCheckBox(SpinnerConstants.KEY_IS_TABLE_EDITOR) {
            {
                setSelected(false);
                addActionListener(e -> spinner.putClientProperty(SpinnerConstants.KEY_IS_TABLE_EDITOR, isSelected()));
            }
        });
        return panel;
    }

    @Override
    public String getName() {
        return "Spinner Demo";
    }
}
