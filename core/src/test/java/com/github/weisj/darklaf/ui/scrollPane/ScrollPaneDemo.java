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
package com.github.weisj.darklaf.ui.scrollPane;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.DemoResources;
import com.github.weisj.darklaf.ui.SolidColorComponent;
import com.github.weisj.darklaf.util.StringUtil;

/**
 * @author Jannis Weis
 * @since 2019
 */
public final class ScrollPaneDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new ScrollPaneDemo());
    }

    @Override
    public JComponent createComponent() {
        JScrollPane scrollPane = new JScrollPane(new JTextArea() {
            {
                setText(StringUtil.repeat(DemoResources.LOREM_IPSUM, 10));
            }
        });
        JPanel upperLeft = new SolidColorComponent(Color.RED, 20, 20);
        JPanel upperRight = new SolidColorComponent(Color.RED, 20, 20);
        JPanel lowerLeft = new SolidColorComponent(Color.RED, 20, 20);
        JPanel lowerRight = new SolidColorComponent(Color.RED, 20, 20);

        DemoPanel panel = new DemoPanel(scrollPane, new BorderLayout(), 10);

        JPanel controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("LeftToRight") {
            {
                setSelected(scrollPane.getComponentOrientation().isLeftToRight());
                addActionListener(e -> scrollPane.setComponentOrientation(
                        isSelected() ? ComponentOrientation.LEFT_TO_RIGHT : ComponentOrientation.RIGHT_TO_LEFT));
            }
        }, "span");

        controlPanel = panel.addControls();
        controlPanel.add(new JCheckBox("UpperLeft corner") {
            {
                addActionListener(
                        e -> scrollPane.setCorner(JScrollPane.UPPER_LEFT_CORNER, isSelected() ? upperLeft : null));
            }
        });
        controlPanel.add(new JCheckBox("UpperRight corner") {
            {
                addActionListener(
                        e -> scrollPane.setCorner(JScrollPane.UPPER_RIGHT_CORNER, isSelected() ? upperRight : null));
            }
        });
        controlPanel.add(new JCheckBox("LowerLeft corner") {
            {
                addActionListener(
                        e -> scrollPane.setCorner(JScrollPane.LOWER_LEFT_CORNER, isSelected() ? lowerLeft : null));
            }
        });
        controlPanel.add(new JCheckBox("LowerRight corner") {
            {
                addActionListener(
                        e -> scrollPane.setCorner(JScrollPane.LOWER_RIGHT_CORNER, isSelected() ? lowerRight : null));
            }
        });
        return panel;
    }

    @Override
    public String getName() {
        return "ScrollPane Demo";
    }
}
