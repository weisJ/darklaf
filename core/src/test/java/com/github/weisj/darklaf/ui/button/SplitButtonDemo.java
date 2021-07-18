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
package com.github.weisj.darklaf.ui.button;

import java.awt.event.ActionListener;

import javax.swing.*;

import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.core.test.DarklafOnly;
import com.github.weisj.darklaf.ui.ComponentDemo;
import com.github.weisj.darklaf.ui.DemoResources;

@DarklafOnly
public class SplitButtonDemo extends ButtonDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new SplitButtonDemo());
    }

    @Override
    protected JButton createButton() {
        Icon icon = DemoResources.FOLDER_ICON;
        JSplitButton button = new JSplitButton("Split Button", icon);
        JPopupMenu menu = button.getActionMenu();
        for (int i = 0; i < 5; i++) {
            menu.add("Item " + i);
        }
        return button;
    }

    @Override
    protected void addCheckBoxControls(final JPanel controlPanel, final JButton button) {
        super.addCheckBoxControls(controlPanel, button);
        controlPanel.add(new JCheckBox("Default action set") {
            private final ActionListener l = ee -> {
            };

            {
                addActionListener(e -> {
                    if (isSelected()) {
                        button.addActionListener(l);
                    } else {
                        button.removeActionListener(l);
                    }
                });
            }
        });

    }

    @Override
    public String getTitle() {
        return "Split Button Demo";
    }
}
