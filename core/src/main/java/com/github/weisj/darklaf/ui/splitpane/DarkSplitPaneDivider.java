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
package com.github.weisj.darklaf.ui.splitpane;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSplitPaneDivider;

import com.github.weisj.darklaf.properties.icons.EmptyIcon;

/** @author Jannis Weis */
public class DarkSplitPaneDivider extends BasicSplitPaneDivider {

    protected final Icon leftOneTouch;
    protected final Icon rightOneTouch;
    protected final Icon topOneTouch;
    protected final Icon bottomOneTouch;
    private final DarkSplitPaneDividerPainter dividerPainter;

    public DarkSplitPaneDivider(final DarkSplitPaneUI ui) {
        super(ui);
        this.dividerPainter = createPainter(ui);
        leftOneTouch = UIManager.getIcon("SplitPaneDivider.leftOneTouch.icon");
        rightOneTouch = UIManager.getIcon("SplitPaneDivider.rightOneTouch.icon");
        topOneTouch = UIManager.getIcon("SplitPaneDivider.topOneTouch.icon");
        bottomOneTouch = UIManager.getIcon("SplitPaneDivider.bottomOneTouch.icon");
    }

    protected DarkSplitPaneDividerPainter createPainter(final DarkSplitPaneUI ui) {
        return new DarkSplitPaneDividerPainter(ui);
    }

    @Override
    public void paint(final Graphics g) {
        super.paint(g);
        dividerPainter.paint(this, g, 0, 0, getWidth(), getHeight());
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
            setMinimumSize(new Dimension(ONE_TOUCH_SIZE, ONE_TOUCH_SIZE));
            setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            setContentAreaFilled(false);
            setRequestFocusEnabled(false);
            setBorderPainted(false);
            setFocusPainted(false);
        }

        @Override
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
                return splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT ? getRightOneTouchIcon()
                        : getBottomOneTouchIcon();
            }
            return EmptyIcon.create(0);
        }
    }

    protected class OneTouchLeftButton extends OneTouchButton {
        @Override
        public Icon getIcon() {
            if (splitPane != null) {
                return splitPane.getOrientation() == JSplitPane.HORIZONTAL_SPLIT ? getLeftOneTouchIcon()
                        : getTopOneTouchIcon();
            }
            return EmptyIcon.create(0);
        }
    }
}
