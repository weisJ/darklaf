package com.weis.darklaf.ui.splitpane;

import com.weis.darklaf.icons.EmptyIcon;
import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;

public class DarkSplitPaneDivider extends BasicSplitPaneDivider {

    public DarkSplitPaneDivider(final BasicSplitPaneUI ui) {
        super(ui);
    }

    @Override
    public void paint(final Graphics g) {
        super.paint(g);
        if (splitPane.getOrientation() == JSplitPane.VERTICAL_SPLIT) {
            Icon icon = getVerticalSplitIcon();
            icon.paintIcon(this, g, (getWidth() - icon.getIconWidth()) / 2,
                           (getHeight() - icon.getIconHeight()) / 2);
        } else {
            Icon icon = getHorizontalSplitIcon();
            icon.paintIcon(this, g, (getWidth() - icon.getIconWidth()) / 2,
                           (getHeight() - icon.getIconHeight()) / 2);
        }
    }

    protected Icon getVerticalSplitIcon() {
        return UIManager.getIcon("SplitPane.verticalGlue.icon");
    }

    protected Icon getHorizontalSplitIcon() {
        return UIManager.getIcon("SplitPane.horizontalGlue.icon");
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
        return UIManager.getIcon("SplitPaneDivider.leftOneTouch.icon");
    }

    protected Icon getRightOneTouchIcon() {
        return UIManager.getIcon("SplitPaneDivider.rightOneTouch.icon");
    }

    protected Icon getTopOneTouchIcon() {
        return UIManager.getIcon("SplitPaneDivider.topOneTouch.icon");
    }

    protected Icon getBottomOneTouchIcon() {
        return UIManager.getIcon("SplitPaneDivider.bottomOneTouch.icon");
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

        @Contract(pure = true)
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
