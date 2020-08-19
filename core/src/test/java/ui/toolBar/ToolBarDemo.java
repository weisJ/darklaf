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
package ui.toolBar;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;

import ui.ComponentDemo;

public class ToolBarDemo implements ActionListener, ComponentDemo {
    private static final String PREVIOUS = "previous";
    private static final String UP = "up";
    private static final String NEXT = "next";
    private JTextArea textArea;

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new ToolBarDemo());
    }

    @Override
    public JComponent createComponent() {
        JToolBar toolBar = new JToolBar("Demo ToolBar");
        addToolbarButtons(toolBar);

        textArea = new JTextArea(5, 30);
        textArea.setEditable(false);
        JScrollPane scrollPane = new JScrollPane(textArea);

        JPanel panel = new JPanel(new BorderLayout());

        panel.setPreferredSize(new Dimension(450, 130));
        panel.add(toolBar, BorderLayout.PAGE_START);
        panel.add(scrollPane, BorderLayout.CENTER);

        return panel;
    }

    private void addToolbarButtons(final JToolBar toolBar) {
        JButton button;
        button = makeNavigationButton(PREVIOUS, "Back to previous something-or-other", "Previous");
        toolBar.add(button);
        toolBar.addSeparator();
        button = makeNavigationButton(UP, "Up to something-or-other", "Up");
        toolBar.add(button);
        toolBar.addSeparator();
        button = makeNavigationButton(NEXT, "Forward to something-or-other", "Next");
        toolBar.add(button);
    }

    private JButton makeNavigationButton(final String actionCommand,
                                         final String toolTipText,
                                         final String altText) {
        JButton button = new JButton();
        button.setActionCommand(actionCommand);
        button.setToolTipText(toolTipText);
        button.addActionListener(this);
        button.setText(altText);
        return button;
    }

    public void actionPerformed(final ActionEvent e) {
        String cmd = e.getActionCommand();
        String description = null;

        if (PREVIOUS.equals(cmd)) {
            description = "taken you to the previous <something>.";
        } else if (UP.equals(cmd)) {
            description = "taken you up one level to <something>.";
        } else if (NEXT.equals(cmd)) {
            description = "taken you to the next <something>.";
        }

        textArea.append("If this were a real app, it would have " + description + "\n");
        textArea.setCaretPosition(textArea.getDocument().getLength());
    }

    @Override
    public String getTitle() {
        return "ToolBar Demo";
    }
}
