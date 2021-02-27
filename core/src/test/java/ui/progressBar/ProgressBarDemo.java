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
package ui.progressBar;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

public class ProgressBarDemo implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ProgressBarDemo());
    }

    @Override
    public JComponent createComponent() {
        JProgressBar progressBar = new JProgressBar();
        progressBar.setString("Test ProgressBar");
        progressBar.setMinimum(0);
        progressBar.setMaximum(100);
        progressBar.setValue(50);
        DemoPanel panel = new DemoPanel(progressBar);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("enabled") {
            {
                setSelected(progressBar.isEnabled());
                addActionListener(e -> progressBar.setEnabled(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("indeterminate") {
            {
                setSelected(progressBar.isIndeterminate());
                addActionListener(e -> progressBar.setIndeterminate(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(progressBar.getComponentOrientation().isLeftToRight());
                addActionListener(e -> progressBar.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        });
        controlPanel.add(new JCheckBox("String painted") {
            {
                setSelected(progressBar.isStringPainted());
                addActionListener(e -> progressBar.setStringPainted(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("JProgressBar.failed") {
            {
                addActionListener(e -> progressBar.putClientProperty("JProgressBar.failed", isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("JProgressBar.passed") {
            {
                addActionListener(e -> progressBar.putClientProperty("JProgressBar.passed", isSelected()));
            }
        });

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel("Orientation:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<String>() {
            {
                Map<String, Integer> mapping = new HashMap<String, Integer>() {
                    {
                        put("HORIZONTAL", JProgressBar.HORIZONTAL);
                        put("VERTICAL", JProgressBar.VERTICAL);
                    }
                };
                addItem("HORIZONTAL");
                addItem("VERTICAL");
                setSelectedItem("HORIZONTAL");
                addItemListener(e -> progressBar.setOrientation(mapping.get(e.getItem().toString())));
            }
        }, "sgx");
        controlPanel.add(new JLabel("Value:", JLabel.RIGHT));
        controlPanel.add(new JPanel() {
            {
                // Wrap in JPanel. Otherwise, the slider appears too low.
                add(new JSlider() {
                    {
                        setMinimum(progressBar.getMinimum());
                        setMaximum(progressBar.getMaximum());
                        setValue(progressBar.getValue());
                        addChangeListener(e -> progressBar.setValue(getValue()));
                    }
                });
            }
        }, "sgx");
        return panel;
    }

    @Override
    public String getTitle() {
        return "ProgressBar Demo";
    }
}
