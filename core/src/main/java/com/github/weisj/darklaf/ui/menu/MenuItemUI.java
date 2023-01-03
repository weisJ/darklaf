/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.menu;

import java.awt.*;

import javax.swing.AbstractButton;
import javax.swing.ButtonModel;
import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;

import com.github.weisj.darklaf.compatibility.MenuItemLayoutHelper;
import com.github.weisj.darklaf.compatibility.SwingUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.StringUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public interface MenuItemUI {

    MenuItemLayoutHelper getMenuItemLayoutHelper(final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap,
            final JMenuItem mi, final Rectangle viewRect);

    String getPropertyPrefix();

    default Color getForeground(final JMenuItem item) {
        boolean enabled = item.isEnabled();
        boolean armed = item.isArmed()
                || (item instanceof JMenu && item.isSelected())
                || (item.isRolloverEnabled() && item.getModel().isRollover());
        if (enabled) {
            return armed ? getSelectionForeground() : item.getForeground();
        } else {
            return getDisabledForeground();
        }
    }

    Color getDisabledForeground();

    Color getSelectionForeground();

    Color getAcceleratorSelectionForeground();

    Color getAcceleratorForeground();

    default Color getBackground(final JMenuItem item) {
        boolean enabled = item.isEnabled();
        boolean armed = item.isArmed() || (item instanceof JMenu && item.isSelected());
        if (enabled) {
            return armed ? getSelectionBackground() : item.getBackground();
        } else {
            return getDisabledBackground();
        }
    }

    Color getSelectionBackground();

    Color getDisabledBackground();

    int getAcceleratorTextOffset();

    int getArc();

    boolean forceOddMenuHeight();

    default void paintMenuItemImpl(final Graphics g, final JComponent c, final Icon checkIcon,
            final Icon arrowIcon, final int defaultTextIconGap) {
        // Save original graphics font and color
        GraphicsContext context = new GraphicsContext(g);

        JMenuItem mi = (JMenuItem) c;
        g.setFont(mi.getFont());

        Rectangle viewRect = new Rectangle(0, 0, mi.getWidth(), mi.getHeight());
        DarkUIUtil.applyInsets(viewRect, mi.getInsets());
        DarkUIUtil.applyInsets(viewRect, mi.getMargin());

        MenuItemLayoutHelper lh = getMenuItemLayoutHelper(checkIcon, arrowIcon, defaultTextIconGap, mi, viewRect);
        MenuItemLayoutHelper.MILayoutResult lr = lh.layoutMenuItem();

        Color background = getBackground(mi);
        Color foreground = getForeground(mi);

        paintBackgroundImpl(g, mi, background);
        context.restore();

        paintCheckIcon(g, mi, lh, lr, foreground);
        context.restore();

        paintIcon(g, mi, lh, lr);

        g.setColor(foreground);
        paintText(g, mi, lh, lr);
        paintAccText(g, mi, lh, lr);
        paintArrowIcon(g, mi, lh, lr, foreground);
        context.restore();
    }

    default void paintBackgroundImpl(final Graphics g, final JMenuItem menuItem, final Color bgColor) {
        int menuWidth = menuItem.getWidth();
        int menuHeight = menuItem.getHeight() + 1;
        int arc = getArc();

        boolean armed = menuItem.isArmed()
                || (menuItem instanceof JMenu && menuItem.isSelected())
                || (menuItem.isRolloverEnabled() && menuItem.getModel().isRollover());
        boolean parentOpaque = menuItem.getParent().isOpaque();

        if (armed || (menuItem.isOpaque() && parentOpaque)) {
            if (arc > 0) {
                g.setColor(menuItem.getBackground());
                g.fillRect(0, 0, menuWidth, menuHeight);
            }
            g.setColor(bgColor);
            PaintUtil.fillRoundRect((Graphics2D) g, 0, 0, menuWidth, menuHeight, arc);
        }
    }

    default void paintCheckIcon(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.MILayoutResult lr, final Color foreground) {
        if (lh.getCheckIcon() != null) {
            ButtonModel model = mi.getModel();
            if (model.isArmed() || (mi instanceof JMenu && model.isSelected())) {
                g.setColor(foreground);
            }
            if (lh.useCheckAndArrow()) {
                lh.getCheckIcon().paintIcon(mi, g, lr.getCheckRect().x, lr.getCheckRect().y);
            }
        }
    }

    default void paintIcon(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.MILayoutResult lr) {
        if (lh.getIcon() != null) {
            Icon icon;
            ButtonModel model = mi.getModel();
            if (!model.isEnabled()) {
                icon = mi.getDisabledIcon();
            } else if (model.isPressed() && model.isArmed()) {
                icon = mi.getPressedIcon();
                if (icon == null) {
                    // Use default icon
                    icon = mi.getIcon();
                }
            } else {
                icon = mi.getIcon();
            }

            if (icon != null) {
                icon.paintIcon(mi, g, lr.getIconRect().x, lr.getIconRect().y);
            }
        }
    }

    default void paintText(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.MILayoutResult lr) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        if (!StringUtil.isBlank(lh.getText())) {
            if (lh.getHtmlView() != null) {
                // Text is HTML
                lh.getHtmlView().paint(g, lr.getTextRect());
            } else {
                // Text isn't HTML
                paintItemText(g, mi, lr.getTextRect(), lh.getText());
            }
        }
        config.restore();
    }

    default void paintItemText(final Graphics g, final JMenuItem menuItem, final Rectangle textRect,
            final String text) {
        ButtonModel model = menuItem.getModel();
        FontMetrics fm = SwingUtil.getFontMetrics(menuItem, g);
        int mnemIndex = menuItem.getDisplayedMnemonicIndex();
        if (!model.isEnabled() || !menuItem.isEnabled()) {
            g.setColor(getDisabledForeground());
        } else {
            if (model.isArmed() || (menuItem instanceof JMenu && model.isSelected())) {
                g.setColor(getSelectionForeground());
            }
        }
        SwingUtil.drawStringUnderlineCharAt(menuItem, g, text,
                mnemIndex, textRect.x, textRect.y + fm.getAscent());
    }

    default void paintAccText(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.MILayoutResult lr) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        rightAlignAccText(lh, lr);
        if (!StringUtil.isBlank(lh.getAccText())) {
            g.setFont(lh.getAccFontMetrics().getFont());
            g.setColor(getAcceleratorForeground(mi));
            SwingUtil.drawString(mi, g, lh.getAccText(), lr.getAccRect().x,
                    lr.getAccRect().y + lh.getAccFontMetrics().getAscent());
        }
        config.restore();
    }

    default Color getAcceleratorForeground(final AbstractButton b) {
        ButtonModel model = b.getModel();
        if (!model.isEnabled() || !b.isEnabled()) return getDisabledForeground();
        if (model.isArmed() || (b instanceof JMenu && model.isSelected())) {
            return getAcceleratorSelectionForeground();
        } else {
            return getAcceleratorForeground();
        }
    }

    static void rightAlignAccText(final MenuItemLayoutHelper lh, final MenuItemLayoutHelper.MILayoutResult lr) {
        Rectangle accRect = lr.getAccRect();
        ButtonModel model = lh.getMenuItem().getModel();
        if (model.isEnabled()) {
            accRect.x = lh.getViewRect().x + lh.getViewRect().width - lh.getMenuItem().getIconTextGap()
                    - lr.getAccRect().width;
        }
    }

    default void paintArrowIcon(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.MILayoutResult lr, final Color foreground) {
        if (lh.getArrowIcon() != null) {
            ButtonModel model = mi.getModel();
            if (model.isArmed() || (mi instanceof JMenu && model.isSelected())) {
                g.setColor(foreground);
            }
            if (lh.useCheckAndArrow()) {
                lh.getArrowIcon().paintIcon(mi, g, lr.getArrowRect().x, lr.getArrowRect().y);
            }
        }
    }

    default Dimension getPreferredMenuItemSizeImpl(final JComponent c, final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap) {

        JMenuItem mi = (JMenuItem) c;
        MenuItemLayoutHelper lh = getMenuItemLayoutHelper(checkIcon, arrowIcon, defaultTextIconGap, mi,
                MenuItemLayoutHelper.createMaxRect());
        Dimension result = new Dimension();

        // Calculate the result width
        result.width = lh.getLeadingGap();
        MenuItemLayoutHelper.addMaxWidth(lh.getCheckSize(), lh.getAfterCheckIconGap(), result);
        // Take into account minimal text offset.
        if (!lh.isTopLevelMenu() && lh.getMinTextOffset() > 0) {
            result.width = Math.max(lh.getMinTextOffset(), result.width);
        }
        int acceleratorTextOffset = getAcceleratorTextOffset();
        MenuItemLayoutHelper.addMaxWidth(lh.getLabelSize(), 0, result);
        MenuItemLayoutHelper.addMaxWidth(lh.getAccSize(), acceleratorTextOffset, result);
        MenuItemLayoutHelper.addMaxWidth(lh.getArrowSize(), lh.getGap(), result);

        if (lh.isTopLevelMenu()) result.width += lh.getLeadingGap();

        // Calculate the result height
        result.height = Integer.MIN_VALUE;
        result.height = Math.max(result.height, lh.getCheckSize().getHeight());
        result.height = Math.max(result.height, lh.getLabelSize().getHeight());
        result.height = Math.max(result.height, lh.getAccSize().getHeight());
        result.height = Math.max(result.height, lh.getArrowSize().getHeight());

        // Take into account menu item insets
        Insets insets = mi.getInsets();
        if (insets != null) {
            result.width += insets.left + insets.right;
            result.height += insets.top + insets.bottom;
        }

        Insets margin = mi.getMargin();
        if (margin != null) {
            result.width += margin.left + margin.right;
            result.height += margin.top + margin.bottom;
        }

        // If the height is even, bump it up by one. This is critical for the text to center properly
        if (result.height % 2 == 0 && forceOddMenuHeight()) {
            result.height++;
        }

        return result;
    }
}
