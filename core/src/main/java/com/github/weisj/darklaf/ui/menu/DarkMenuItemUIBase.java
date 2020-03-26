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
package com.github.weisj.darklaf.ui.menu;

import com.github.weisj.darklaf.util.*;
import sun.swing.MenuItemLayoutHelper;
import sun.swing.SwingUtilities2;
import sun.swing.UIAction;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicMenuItemUI;
import java.awt.*;
import java.awt.event.ActionEvent;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkMenuItemUIBase extends BasicMenuItemUI {


    public static ComponentUI createUI(final JComponent c) {
        return new DarkMenuItemUIBase();
    }

    public static void loadActionMap(final LazyActionMap map) {
        map.put(new Actions(Actions.CLICK));
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        acceleratorFont = UIManager.getFont("MenuItem.font");
        acceleratorForeground = UIManager.getColor("MenuItem.foreground");
        acceleratorSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
    }

    public void paint(final Graphics g, final JComponent c) {
        paintMenuItem(g, c, checkIcon, arrowIcon,
                      selectionBackground, isSelected(c) ? selectionForeground : c.getForeground(),
                      defaultTextIconGap);
    }

    protected boolean isSelected(final JComponent menuItem) {
        if (!(menuItem instanceof JMenuItem)) return false;
        return menuItem.isEnabled() && ((JMenuItem) menuItem).isArmed();
    }

    private static void rightAlignAccText(final MenuItemLayoutHelper lh,
                                          final MenuItemLayoutHelper.LayoutResult lr) {
        Rectangle accRect = lr.getAccRect();
        ButtonModel model = lh.getMenuItem().getModel();
        if (model.isEnabled()) {
            accRect.x = lh.getViewRect().x + lh.getViewRect().width
                        - lh.getMenuItem().getIconTextGap() - lr.getAccRect().width;
        }
    }

    protected void paintMenuItem(final Graphics g, final JComponent c,
                                 final Icon checkIcon, final Icon arrowIcon,
                                 final Color background, final Color foreground,
                                 final int defaultTextIconGap) {
        // Save original graphics font and color
        Font holdf = g.getFont();
        Color holdc = g.getColor();

        JMenuItem mi = (JMenuItem) c;
        g.setFont(mi.getFont());

        Rectangle viewRect = new Rectangle(0, 0, mi.getWidth(), mi.getHeight());
        DarkUIUtil.applyInsets(viewRect, mi.getInsets());

        MenuItemLayoutHelper lh = new MenuItemLayoutHelper(mi, checkIcon,
                                                           arrowIcon, viewRect, defaultTextIconGap,
                                                           acceleratorDelimiter,
                                                           mi.getComponentOrientation().isLeftToRight(), mi.getFont(),
                                                           acceleratorFont,
                                                           MenuItemLayoutHelper.useCheckAndArrow(menuItem),
                                                           getPropertyPrefix());
        MenuItemLayoutHelper.LayoutResult lr = lh.layoutMenuItem();

        paintBackground(g, mi, background);
        paintCheckIcon(g, lh, lr, holdc, foreground);
        paintIcon(g, lh, lr, holdc);
        g.setColor(foreground);
        paintText(g, lh, lr);
        paintAccText(g, lh, lr);
        paintArrowIcon(g, lh, lr, foreground);

        // Restore original graphics font and color
        g.setColor(holdc);
        g.setFont(holdf);
    }

    protected void paintCheckIcon(final Graphics g, final MenuItemLayoutHelper lh,
                                  final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color holdc, final Color foreground) {
        if (lh.getCheckIcon() != null) {
            ButtonModel model = lh.getMenuItem().getModel();
            if (model.isArmed() || (lh.getMenuItem() instanceof JMenu
                                    && model.isSelected())) {
                g.setColor(foreground);
            } else {
                g.setColor(holdc);
            }
            if (lh.useCheckAndArrow()) {
                lh.getCheckIcon().paintIcon(lh.getMenuItem(), g,
                                            lr.getCheckRect().x, lr.getCheckRect().y);
            }
            g.setColor(holdc);
        }
    }

    protected void paintAccText(final Graphics g, final MenuItemLayoutHelper lh,
                                final MenuItemLayoutHelper.LayoutResult lr) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        rightAlignAccText(lh, lr);
        if (!StringUtil.isBlank(lh.getAccText())) {
            ButtonModel model = lh.getMenuItem().getModel();
            g.setFont(lh.getAccFontMetrics().getFont());
            if (!model.isEnabled()) {
                // *** paint the accText disabled
                if (disabledForeground != null) {
                    g.setColor(disabledForeground);
                    SwingUtilities2.drawString(lh.getMenuItem(), g,
                                               lh.getAccText(), lr.getAccRect().x,
                                               lr.getAccRect().y + lh.getAccFontMetrics().getAscent());
                } else {
                    g.setColor(lh.getMenuItem().getBackground().brighter());
                    SwingUtilities2.drawString(lh.getMenuItem(), g,
                                               lh.getAccText(), lr.getAccRect().x,
                                               lr.getAccRect().y + lh.getAccFontMetrics().getAscent());
                    g.setColor(lh.getMenuItem().getBackground().darker());
                    SwingUtilities2.drawString(lh.getMenuItem(), g,
                                               lh.getAccText(), lr.getAccRect().x - 1,
                                               lr.getAccRect().y + lh.getFontMetrics().getAscent() - 1);
                }
            } else {
                // *** paint the accText normally
                if (model.isArmed()
                    || (lh.getMenuItem() instanceof JMenu
                        && model.isSelected())) {
                    g.setColor(acceleratorSelectionForeground);
                } else {
                    g.setColor(acceleratorForeground);
                }
                SwingUtilities2.drawString(lh.getMenuItem(), g, lh.getAccText(),
                                           lr.getAccRect().x, lr.getAccRect().y +
                                                              lh.getAccFontMetrics().getAscent());
            }
        }
        config.restore();
    }

    protected void paintIcon(final Graphics g, final MenuItemLayoutHelper lh,
                             final MenuItemLayoutHelper.LayoutResult lr, final Color holdc) {
        if (lh.getIcon() != null) {
            Icon icon;
            ButtonModel model = lh.getMenuItem().getModel();
            if (!model.isEnabled()) {
                icon = lh.getMenuItem().getDisabledIcon();
            } else if (model.isPressed() && model.isArmed()) {
                icon = lh.getMenuItem().getPressedIcon();
                if (icon == null) {
                    // Use default icon
                    icon = lh.getMenuItem().getIcon();
                }
            } else {
                icon = lh.getMenuItem().getIcon();
            }

            if (icon != null) {
                icon.paintIcon(lh.getMenuItem(), g, lr.getIconRect().x, lr.getIconRect().y);
                g.setColor(holdc);
            }
        }
    }

    protected void paintText(final Graphics g, final MenuItemLayoutHelper lh,
                             final MenuItemLayoutHelper.LayoutResult lr) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        if (!StringUtil.isBlank(lh.getText())) {
            if (lh.getHtmlView() != null) {
                // Text is HTML
                lh.getHtmlView().paint(g, lr.getTextRect());
            } else {
                // Text isn't HTML
                paintText(g, lh.getMenuItem(), lr.getTextRect(), lh.getText());
            }
        }
        config.restore();
    }

    protected void paintArrowIcon(final Graphics g, final MenuItemLayoutHelper lh,
                                  final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color foreground) {
        if (lh.getArrowIcon() != null) {
            ButtonModel model = lh.getMenuItem().getModel();
            if (model.isArmed() || (lh.getMenuItem() instanceof JMenu
                                    && model.isSelected())) {
                g.setColor(foreground);
            }
            if (lh.useCheckAndArrow()) {
                lh.getArrowIcon().paintIcon(lh.getMenuItem(), g,
                                            lr.getArrowRect().x, lr.getArrowRect().y);
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
                g.fillRect(0, 0, menuWidth, menuHeight);
            } else {
                g.setColor(menuItem.getBackground());
                g.fillRect(0, 0, menuWidth, menuHeight);
            }
            g.setColor(oldColor);
        } else if (model.isArmed() || (menuItem instanceof JMenu &&
                                       model.isSelected())) {
            g.setColor(bgColor);
            g.fillRect(0, 0, menuWidth, menuHeight);
            g.setColor(oldColor);
        }
    }

    protected static class Actions extends UIAction {
        protected static final String CLICK = "doClick";

        Actions(final String key) {
            super(key);
        }

        public void actionPerformed(final ActionEvent e) {
            JMenuItem mi = (JMenuItem) e.getSource();
            MenuSelectionManager.defaultManager().clearSelectedPath();
            mi.doClick();
        }
    }
}
