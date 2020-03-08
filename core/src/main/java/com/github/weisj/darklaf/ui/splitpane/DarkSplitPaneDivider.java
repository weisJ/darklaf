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
package com.github.weisj.darklaf.ui.splitpane;

import com.github.weisj.darklaf.icons.EmptyIcon;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class DarkSplitPaneDivider extends BasicSplitPaneDivider {

    protected Icon leftOneTouch;
    protected Icon rightOneTouch;
    protected Icon topOneTouch;
    protected Icon bottomOneTouch;
    protected Icon verticalSplit;
    protected Icon horizontalSplit;
    protected Color borderColor;

    public DarkSplitPaneDivider(final BasicSplitPaneUI ui) {
        super(ui);
        leftOneTouch = UIManager.getIcon("SplitPaneDivider.leftOneTouch.icon");
        rightOneTouch = UIManager.getIcon("SplitPaneDivider.rightOneTouch.icon");
        topOneTouch = UIManager.getIcon("SplitPaneDivider.topOneTouch.icon");
        bottomOneTouch = UIManager.getIcon("SplitPaneDivider.bottomOneTouch.icon");
        verticalSplit = UIManager.getIcon("SplitPane.verticalGlue.icon");
        horizontalSplit = UIManager.getIcon("SplitPane.horizontalGlue.icon");
        borderColor = UIManager.getColor("SplitPane.dividerLineColor");
    }


    @Override
    public void paint(final Graphics g) {
        super.paint(g);
        if (splitPane.getOrientation() == JSplitPane.VERTICAL_SPLIT) {
            Icon icon = getVerticalSplitIcon();
            icon.paintIcon(this, g, (getWidth() - icon.getIconWidth()) / 2,
                           (getHeight() - icon.getIconHeight()) / 2);
            g.setColor(borderColor);
            g.fillRect(0, 0, getWidth(), 1);
            g.fillRect(0, getHeight() - 1, getWidth(), 1);
        } else {
            Icon icon = getHorizontalSplitIcon();
            icon.paintIcon(this, g, (getWidth() - icon.getIconWidth()) / 2,
                           (getHeight() - icon.getIconHeight()) / 2);
            g.setColor(borderColor);
            g.fillRect(0, 0, 1, getHeight());
            g.fillRect(getWidth() - 1, 0, 1, getHeight());
        }
    }

    protected Icon getVerticalSplitIcon() {
        return verticalSplit;
    }

    protected Icon getHorizontalSplitIcon() {
        return horizontalSplit;
    }

    @Override
    protected JButton createLeftOneTouchButton() {
        return new OneTouchLeftButton();
    }

    @Override
    protected JButton createRightOneTouchButton() {
        return new OneTouchRightButton();
    }

    protected Icon getLeftOneTouchIcon() {
        return leftOneTouch;
    }

    protected Icon getRightOneTouchIcon() {
        return rightOneTouch;
    }

    protected Icon getTopOneTouchIcon() {
        return topOneTouch;
    }

    protected Icon getBottomOneTouchIcon() {
        return bottomOneTouch;
    }

    protected static class OneTouchButton extends JButton implements UIResource {
        protected OneTouchButton() {
            putClientProperty("JButton.variant", "onlyLabel");
            setMinimumSize(new Dimension(ONE_TOUCH_SIZE, ONE_TOUCH_SIZE));
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            setRequestFocusEnabled(false);
            setBorderPainted(false);
            setFocusPainted(false);
        }


        @SuppressWarnings("deprecation")
        public boolean isFocusTraversable() {
            return false;
        }

        @Override
        public Icon getPressedIcon() {
            return getIcon();
        }

        @Override
        public Icon getDisabledIcon() {
            return getIcon();
        }
    }

    protected class OneTouchRightButton extends OneTouchButton {
        @Override
        public Icon getIcon() {
            if (splitPane != null) {
                return splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT
                       ? getRightOneTouchIcon() : getBottomOneTouchIcon();
            }
            return EmptyIcon.create(0);
        }
    }

    protected class OneTouchLeftButton extends OneTouchButton {
        @Override
        public Icon getIcon() {
            if (splitPane != null) {
                return splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT
                       ? getLeftOneTouchIcon() : getTopOneTouchIcon();
            }
            return EmptyIcon.create(0);
        }
    }
}
