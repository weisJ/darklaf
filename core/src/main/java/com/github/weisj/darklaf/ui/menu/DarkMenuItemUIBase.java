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
package com.github.weisj.darklaf.ui.menu;

import java.awt.*;
import java.awt.event.ActionEvent;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuItemUI;

import sun.swing.MenuItemLayoutHelper;
import sun.swing.UIAction;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LazyActionMap;
import com.github.weisj.darklaf.util.StringUtil;
import com.github.weisj.darklaf.util.SwingUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkMenuItemUIBase extends BasicMenuItemUI {

    protected int acceleratorTextOffset;
    protected boolean useEvenHeight;
    private boolean closeOnClick;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkMenuItemUIBase();
    }

    public static void loadActionMap(final LazyActionMap map) {
        map.put(new Actions(Actions.CLICK));
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        closeOnClick = !UIManager.getBoolean(getPropertyPrefix() + ".doNotCloseOnMouseClick");
        useEvenHeight = !Boolean.TRUE.equals(UIManager.get(getPropertyPrefix() + ".evenHeight"));
        acceleratorTextOffset = UIManager.getInt(getPropertyPrefix() + ".acceleratorTextOffset");
        acceleratorFont = UIManager.getFont("MenuItem.font");
        acceleratorForeground = UIManager.getColor("MenuItem.foreground");
        acceleratorSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
    }

    @Override
    protected void installKeyboardActions() {
        super.installKeyboardActions();
        LazyActionMap.installLazyActionMap(menuItem, DarkMenuItemUIBase.class, getPropertyPrefix() + ".actionMap");
    }

    public void paint(final Graphics g, final JComponent c) {
        paintMenuItem(g, c, checkIcon, arrowIcon, selectionBackground,
                isSelected(c) ? selectionForeground : c.getForeground(), defaultTextIconGap);
    }

    protected boolean isSelected(final JComponent menuItem) {
        if (!(menuItem instanceof JMenuItem)) return false;
        return menuItem.isEnabled() && ((JMenuItem) menuItem).isArmed();
    }

    private static void rightAlignAccText(final MenuItemLayoutHelper lh, final MenuItemLayoutHelper.LayoutResult lr) {
        Rectangle accRect = lr.getAccRect();
        ButtonModel model = lh.getMenuItem().getModel();
        if (model.isEnabled()) {
            accRect.x = lh.getViewRect().x + lh.getViewRect().width - lh.getMenuItem().getIconTextGap()
                    - lr.getAccRect().width;
        }
    }

    protected void paintMenuItem(final Graphics g, final JComponent c, final Icon checkIcon, final Icon arrowIcon,
            final Color background, final Color foreground, final int defaultTextIconGap) {
        // Save original graphics font and color
        GraphicsContext context = new GraphicsContext(g);

        JMenuItem mi = (JMenuItem) c;
        g.setFont(mi.getFont());

        Rectangle viewRect = new Rectangle(0, 0, mi.getWidth(), mi.getHeight());
        DarkUIUtil.applyInsets(viewRect, mi.getInsets());

        MenuItemLayoutHelper lh = getMenuItemLayoutHelper(checkIcon, arrowIcon, defaultTextIconGap, mi, viewRect);
        MenuItemLayoutHelper.LayoutResult lr = lh.layoutMenuItem();

        paintBackground(g, mi, background);
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

    protected MenuItemLayoutHelper getMenuItemLayoutHelper(final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap, final JMenuItem mi, final Rectangle viewRect) {
        return new MenuItemLayoutHelper(mi, checkIcon, arrowIcon, viewRect, defaultTextIconGap, acceleratorDelimiter,
                mi.getComponentOrientation().isLeftToRight(), mi.getFont(), acceleratorFont,
                MenuItemLayoutHelper.useCheckAndArrow(menuItem), getPropertyPrefix());
    }

    protected void paintCheckIcon(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.LayoutResult lr, final Color foreground) {
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

    protected void paintAccText(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.LayoutResult lr) {
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

    @Override
    protected void paintText(final Graphics g, final JMenuItem menuItem, final Rectangle textRect, final String text) {
        super.paintText(g, menuItem, textRect, text);
    }

    protected Color getAcceleratorForeground(final AbstractButton b) {
        ButtonModel model = b.getModel();
        if (!model.isEnabled()) return disabledForeground;
        if (model.isArmed() || (b instanceof JMenu && model.isSelected())) {
            return acceleratorSelectionForeground;
        } else {
            return acceleratorForeground;
        }
    }

    protected void paintIcon(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.LayoutResult lr) {
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

    protected void paintText(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.LayoutResult lr) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        if (!StringUtil.isBlank(lh.getText())) {
            if (lh.getHtmlView() != null) {
                // Text is HTML
                lh.getHtmlView().paint(g, lr.getTextRect());
            } else {
                // Text isn't HTML
                paintText(g, mi, lr.getTextRect(), lh.getText());
            }
        }
        config.restore();
    }

    protected void paintArrowIcon(final Graphics g, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.LayoutResult lr, final Color foreground) {
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

    @Override
    protected void paintBackground(final Graphics g, final JMenuItem menuItem, final Color bgColor) {
        ButtonModel model = menuItem.getModel();
        Color oldColor = g.getColor();
        int menuWidth = menuItem.getWidth();
        int menuHeight = menuItem.getHeight() + 1;

        boolean parentOpaque = menuItem.getParent().isOpaque();
        if (menuItem.isOpaque() && parentOpaque) {
            if (model.isArmed() || (menuItem instanceof JMenu && model.isSelected())) {
                g.setColor(bgColor);
            } else {
                g.setColor(menuItem.getBackground());
            }
            g.fillRect(0, 0, menuWidth, menuHeight);
            g.setColor(oldColor);
        } else if (model.isArmed() || (menuItem instanceof JMenu && model.isSelected())) {
            g.setColor(bgColor);
            g.fillRect(0, 0, menuWidth, menuHeight);
            g.setColor(oldColor);
        }
    }

    @Override
    protected void doClick(final MenuSelectionManager selectionManager) {
        if (isCloseOnClick()) {
            MenuSelectionManager msm = selectionManager;
            // Visual feedback
            if (msm == null) {
                msm = MenuSelectionManager.defaultManager();
            }
            msm.clearSelectedPath();
        }
        JMenuItem item = menuItem;
        item.doClick(0);
        item.setArmed(!isCloseOnClick());
    }

    protected boolean isCloseOnClick() {
        return closeOnClick;
    }

    @Override
    protected Dimension getPreferredMenuItemSize(final JComponent c, final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap) {

        JMenuItem mi = (JMenuItem) c;
        MenuItemLayoutHelper lh = getMenuItemLayoutHelper(checkIcon, arrowIcon, defaultTextIconGap, mi,
                MenuItemLayoutHelper.createMaxRect());
        Dimension result = new Dimension();

        // Calculate the result width
        result.width = lh.getLeadingGap();
        MenuItemLayoutHelper.addMaxWidth(lh.getCheckSize(), lh.getAfterCheckIconGap(), result);
        // Take into account minimal text offset.
        if ((!lh.isTopLevelMenu()) && (lh.getMinTextOffset() > 0) && (result.width < lh.getMinTextOffset())) {
            result.width = lh.getMinTextOffset();
        }
        MenuItemLayoutHelper.addMaxWidth(lh.getLabelSize(), acceleratorTextOffset, result);
        MenuItemLayoutHelper.addMaxWidth(lh.getAccSize(), acceleratorTextOffset, result);
        MenuItemLayoutHelper.addMaxWidth(lh.getArrowSize(), lh.getGap(), result);

        // Calculate the result height
        result.height = MenuItemLayoutHelper.max(lh.getCheckSize().getHeight(), lh.getLabelSize().getHeight(),
                lh.getAccSize().getHeight(), lh.getArrowSize().getHeight());

        // Take into account menu item insets
        Insets insets = mi.getInsets();
        if (insets != null) {
            result.width += insets.left + insets.right;
            result.height += insets.top + insets.bottom;
        }

        // if the height is even, bump it up one. This is critical.
        // for the text to center properly
        if (result.height % 2 == 0 && useEvenHeight) {
            result.height++;
        }

        return result;
    }

    protected static class Actions extends UIAction {
        protected static final String CLICK = "doClick";

        protected Actions(final String key) {
            super(key);
        }

        public void actionPerformed(final ActionEvent e) {
            JMenuItem mi = (JMenuItem) e.getSource();
            MenuSelectionManager.defaultManager().clearSelectedPath();
            mi.doClick();
        }
    }
}
