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
package ui.tabbedPane;

import com.github.weisj.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;

public class TabbedPaneKeyboardShortcut extends JPanel {
    public TabbedPaneKeyboardShortcut() {
        initializeUI();
    }

    private void initializeUI() {
        LafManager.install();
        this.setLayout(new BorderLayout());
        this.setPreferredSize(new Dimension(500, 200));

        JTabbedPane pane = new JTabbedPane();
        pane.addTab("A Tab", new JPanel());
        pane.addTab("B Tab", new JPanel());
        pane.addTab("C Tab", new JPanel());
        pane.addTab("D Tab", new JPanel());

        pane.setMnemonicAt(0, KeyEvent.VK_A);
        pane.setMnemonicAt(1, KeyEvent.VK_B);
        pane.setMnemonicAt(2, KeyEvent.VK_C);
        pane.setMnemonicAt(3, KeyEvent.VK_D);

        this.add(pane, BorderLayout.CENTER);
    }

    public static void main(final String[] args) {
        SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                TabbedPaneKeyboardShortcut.showFrame();
            }
        });
    }

    public static void showFrame() {
        JPanel panel = new TabbedPaneKeyboardShortcut();
        panel.setOpaque(true);

        JFrame frame = new JFrame("JTabbedPane Demo");
        frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
        frame.setContentPane(panel);
        frame.pack();
        frame.setVisible(true);
    }
}
