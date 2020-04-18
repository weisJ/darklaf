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
package ui.tabbedPane;

import java.awt.*;
import java.awt.event.KeyEvent;

import javax.swing.*;

import ui.ComponentDemo;

public class TabbedPaneKeyboardShortcut implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new TabbedPaneKeyboardShortcut());
    }

    @Override
    public JComponent createComponent() {
        JPanel panel = new JPanel();
        panel.setLayout(new BorderLayout());
        panel.setPreferredSize(new Dimension(500, 200));

        JTabbedPane tabbedPane = new JTabbedPane();
        tabbedPane.addTab("A Tab", new JPanel());
        tabbedPane.addTab("B Tab", new JPanel());
        tabbedPane.addTab("C Tab", new JPanel());
        tabbedPane.addTab("D Tab", new JPanel());

        tabbedPane.setMnemonicAt(0, KeyEvent.VK_A);
        tabbedPane.setMnemonicAt(1, KeyEvent.VK_B);
        tabbedPane.setMnemonicAt(2, KeyEvent.VK_C);
        tabbedPane.setMnemonicAt(3, KeyEvent.VK_D);

        panel.add(tabbedPane, BorderLayout.CENTER);
        return panel;
    }

    @Override
    public String getTitle() {
        return "Tabbed Pane Keyboard Shortcut Demo";
    }
}
