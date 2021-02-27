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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.util.DarkUIUtil;

public abstract class TabButtonContainer extends JPanel implements UIResource {

    protected final JButton button;
    protected final DarkTabbedPaneUI ui;
    private final Insets borderInsets;

    protected TabButtonContainer(final DarkTabbedPaneUI ui) {
        this.ui = ui;
        borderInsets = UIManager.getInsets("TabbedPane.tabArea.buttonInsets");
        button = createButton();
        add(button);
        setOpaque(false);
        setLayout(null);
    }

    protected abstract JButton createButton();

    @Override
    public void doLayout() {
        Dimension b = button.getPreferredSize();
        int w = Math.min(getWidth(), b.width);
        int h = Math.min(getHeight(), b.height);
        int x = (getWidth() - w) / 2;
        int y = (getHeight() - h) / 2;
        button.setBounds(x, y, w, h);
    }

    @Override
    public void setBounds(final int x, final int y, final int width, final int height) {
        super.setBounds(x, y, width, height);
        doLayout();
    }

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        ui.paintTabAreaBorder(g, ui.tabPane.getTabPlacement(), 0, 0, getWidth(), getHeight());
    }

    @Override
    public Dimension getPreferredSize() {
        return DarkUIUtil.addInsets(button.getPreferredSize(), getInsets());
    }

    @Override
    public Insets getInsets() {
        if (ui.isHorizontalTabPlacement()) {
            return new Insets(0, borderInsets.left, 0, borderInsets.right);
        } else {
            return new Insets(borderInsets.top, 0, borderInsets.bottom, 0);
        }
    }
}
