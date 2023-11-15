/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.toolTip;

import javax.swing.*;

import com.github.weisj.darklaf.components.alignment.AlignmentStrategy;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.DemoResources;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.tooltip.DarkToolTipUI;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.StringUtil;

public class ToolTipDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new ToolTipDemo());
    }

    @Override
    public JComponent createComponent() {
        JButton button = new JButton("Demo Button", DemoResources.FOLDER_ICON);
        DemoPanel panel = new DemoPanel(button);
        ToolTipContext context =
                new ToolTipContext(button).setAlignment(Alignment.CENTER).setCenterAlignment(Alignment.SOUTH);
        ToolTipContext.setDefaultContext(context);
        button.setToolTipText(StringUtil.toHtml("""
                <p style="color:red;">This is the ToolTip demo text!
                <p style="color:blue;">This is the ToolTip demo text!
                <p style="color:green;">This is the ToolTip demo text!
                """));
        button.putClientProperty(DarkToolTipUI.KEY_STYLE, DarkToolTipUI.VARIANT_BALLOON);
        button.putClientProperty(DarkToolTipUI.KEY_CONTEXT, context);

        JPanel controlPanel = panel.addControls(3);

        controlPanel.add(new JCheckBox("Align inside") {
            {
                setSelected(context.isAlignInside());
                addActionListener(e -> context.setAlignInside(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Ignore Border") {
            {
                setSelected(context.isIgnoreBorder());
                addActionListener(e -> context.setIgnoreBorder(isSelected()));
            }
        });
        controlPanel.add(new JCheckBox("Use best fit") {
            {
                setSelected(context.isBestFit());
                addActionListener(e -> context.setUseBestFit(isSelected()));
            }
        });

        controlPanel = panel.addControls();
        controlPanel.add(new JLabel("Tooltip Style:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<>(ToolTipStyle.values()) {
            {
                setSelectedItem(ToolTipStyle.BALLOON);
                addItemListener(e -> button.putClientProperty(DarkToolTipUI.KEY_STYLE, e.getItem()));
            }
        }, "sgx");
        controlPanel.add(new JLabel("Alignment:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<>(Alignment.values()) {
            {
                setSelectedItem(context.getAlignment());
                addItemListener(e -> context.setAlignment((Alignment) e.getItem()));
            }
        }, "sgx");
        controlPanel.add(new JLabel("Center Alignment:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<>(Alignment.values()) {
            {
                setSelectedItem(context.getCenterAlignment());
                addItemListener(e -> context.setCenterAlignment((Alignment) e.getItem()));
            }
        }, "sgx");
        controlPanel.add(new JLabel("Alignment Strategy:", JLabel.RIGHT));
        controlPanel.add(new JComboBox<>(AlignmentStrategy.values()) {
            {
                setSelectedItem(context.getAlignmentStrategy());
                addItemListener(e -> context.setAlignmentStrategy((AlignmentStrategy) e.getItem()));
            }
        }, "sgx");
        return panel;
    }

    @Override
    public String getName() {
        return "ToolTip Demo";
    }
}
