/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.internalframe;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicDesktopPaneUI;

public class DarkDesktopPaneUI extends BasicDesktopPaneUI {

    protected InternalFrameMenuBar menuBarHolder = new InternalFrameMenuBar();

    public static ComponentUI createUI(final JComponent c) {
        return new DarkDesktopPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        if (UIManager.getBoolean("InternalFrame.useExternalMenuBar")) {
            desktop.add(menuBarHolder);
            desktop.setLayer(menuBarHolder, Integer.MAX_VALUE);
            desktop.setLayout(new DarkDesktopPaneLayout());
            setActiveJMenuBar(null);
        }
    }

    public void setActiveJMenuBar(final JMenuBar menuBar) {
        menuBarHolder.setActiveJMenuBar(menuBar);
    }

    public Rectangle getContentRegion() {
        return new Rectangle(0, menuBarHolder.getHeight(), desktop.getWidth(),
                desktop.getHeight() - menuBarHolder.getHeight());
    }

    protected class DarkDesktopPaneLayout implements LayoutManager {

        @Override
        public void addLayoutComponent(final String name, final Component comp) {}

        @Override
        public void removeLayoutComponent(final Component comp) {}

        @Override
        public Dimension preferredLayoutSize(final Container parent) {
            return null;
        }

        @Override
        public Dimension minimumLayoutSize(final Container parent) {
            return null;
        }

        @Override
        public void layoutContainer(final Container parent) {
            menuBarHolder.setBounds(0, 0, parent.getWidth(), menuBarHolder.getPreferredSize().height);
        }
    }
}
