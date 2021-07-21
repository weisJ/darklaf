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
package com.github.weisj.darklaf.ui.internalFrame;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.beans.PropertyVetoException;
import java.util.Collections;
import java.util.List;

import javax.swing.*;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;

import com.github.weisj.darklaf.components.border.DarkBorders;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class InternalFrameDemo extends BaseComponentDemo implements ActionListener {

    private JDesktopPane desktop;

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new InternalFrameDemo());
    }

    @Override
    public Dimension getWindowSize() {
        return new Dimension(1000, 500);
    }

    @Override
    public JComponent createComponent() {
        desktop = new JDesktopPane();
        JPanel panel = new JPanel();
        panel.setBorder(new CompoundBorder(new EmptyBorder(20, 20, 20, 20), DarkBorders.createLineBorder(1, 1, 1, 1)));
        panel.setLayout(new BorderLayout());
        panel.add(desktop, BorderLayout.CENTER);

        createFrame();

        desktop.setDragMode(JDesktopPane.LIVE_DRAG_MODE);
        return new DemoPanel(desktop, new BorderLayout(), 10);
    }

    @Override
    public List<JMenu> createMenus() {
        JMenu menu = new JMenu("Document");
        menu.setMnemonic(KeyEvent.VK_D);

        // Set up the first menu item.
        JMenuItem menuItem = new JMenuItem("New");
        menuItem.setMnemonic(KeyEvent.VK_N);
        menuItem.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_N, InputEvent.ALT_DOWN_MASK));
        menuItem.setActionCommand("new");
        menuItem.addActionListener(this);
        menu.add(menuItem);
        return Collections.singletonList(menu);
    }

    @Override
    public String getName() {
        return "Internal Frame Demo";
    }

    // Create a new internal frame.
    private void createFrame() {
        DemoFrame frame = new DemoFrame();
        frame.setVisible(true);
        desktop.add(frame);
        try {
            frame.setSelected(true);
        } catch (final PropertyVetoException ignored) {
        }
    }

    // React to menu selections.
    public void actionPerformed(final ActionEvent e) {
        if ("new".equals(e.getActionCommand())) {
            createFrame();
        }
    }
}
