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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagLayout;

import javax.swing.JComponent;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JSplitPane;
import javax.swing.SwingUtilities;

import com.github.weisj.darklaf.theme.ColorPalette;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;

public class DetachedInternalFrameDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new DetachedInternalFrameDemo());
    }

    @Override
    protected JComponent createComponent() {
        JSplitPane splitPane = new JSplitPane();
        JInternalFrame left = new JInternalFrame("Left Frame");
        left.setContentPane(makeContent("Left Content", ColorPalette.FOREST));
        left.show();
        JInternalFrame right = new JInternalFrame("Right Frame");
        right.setContentPane(makeContent("Right Content", ColorPalette.PINK));
        right.show();

        SwingUtilities.invokeLater(() -> splitPane.setDividerLocation(0.5f));
        splitPane.setLeftComponent(wrap(left));
        splitPane.setRightComponent(wrap(right));
        return splitPane;
    }

    private JComponent wrap(final JComponent c) {
        JPanel p = new JPanel(new BorderLayout());
        p.add(c);
        return p;
    }

    private JComponent makeContent(final String text, final Color c) {
        JPanel p = new JPanel();
        p.setPreferredSize(new Dimension(200, 200));
        p.setBackground(c);
        p.setLayout(new GridBagLayout());
        p.add(new JLabel(text), null);
        return p;
    }

    @Override
    public String getName() {
        return "DetachedInternalFrameDemo";
    }
}
