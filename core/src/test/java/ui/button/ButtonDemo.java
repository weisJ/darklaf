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
package ui.button;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoResources;

public class ButtonDemo extends AbstractButtonDemo<JButton> {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ButtonDemo());
    }

    @Override
    protected JButton createButton() {
        Icon icon = DemoResources.FOLDER_ICON;
        JButton button = new JButton("Test Button", icon);
        button.setToolTipText("TipText");
        return button;
    }

    @Override
    protected void addCheckBoxControls(final JPanel controlPanel, final JButton button) {
        controlPanel.add(new JCheckBox("default") {
            {
                setSelected(button.isDefaultButton());
                addActionListener(e -> {
                    JRootPane rootPane = SwingUtilities.getRootPane(button);
                    rootPane.setDefaultButton(isSelected() ? button : null);
                    button.getParent().doLayout();
                    button.getParent().repaint();
                });
            }
        });
    }

    @Override
    public String getTitle() {
        return "Button Demo";
    }
}
