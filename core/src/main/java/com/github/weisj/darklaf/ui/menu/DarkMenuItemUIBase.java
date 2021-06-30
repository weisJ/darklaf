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

import com.github.weisj.darklaf.ui.UIAction;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.ui.util.LazyActionMap;
import com.intellij.util.ui.MenuItemLayoutHelper;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkMenuItemUIBase extends BasicMenuItemUI implements MenuItemUI {

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
        disabledForeground = UIManager.getColor("MenuItem.disabledForeground");
    }

    @Override
    public void uninstallUI(JComponent c) {
        MenuItemLayoutHelper.clearUsedParentClientProperties(menuItem);
        super.uninstallUI(c);
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

    @Override
    protected void paintMenuItem(final Graphics g, final JComponent c, final Icon checkIcon,
            final Icon arrowIcon, final Color background,
            final Color foreground, final int defaultTextIconGap) {
        paintMenuItemImpl(g, c, checkIcon, arrowIcon, background, foreground, defaultTextIconGap);
    }

    public MenuItemLayoutHelper getMenuItemLayoutHelper(final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap, final JMenuItem mi, final Rectangle viewRect) {
        return getMenuItemLayoutHelperImpl(acceleratorDelimiter, acceleratorFont, getPropertyPrefix(), checkIcon,
                arrowIcon, defaultTextIconGap, mi, viewRect);
    }

    @Override
    public String getPropertyPrefix() {
        return super.getPropertyPrefix();
    }

    @Override
    public Color getDisabledForeground() {
        return disabledForeground;
    }

    @Override
    public Color getSelectionForeground() {
        return selectionForeground;
    }

    @Override
    public Color getAcceleratorSelectionForeground() {
        return acceleratorSelectionForeground;
    }

    @Override
    public Color getAcceleratorForeground() {
        return acceleratorForeground;
    }

    @Override
    public int getAcceleratorTextOffset() {
        return acceleratorTextOffset;
    }

    @Override
    public boolean isUseEvenHeight() {
        return useEvenHeight;
    }

    protected static MenuItemLayoutHelper getMenuItemLayoutHelperImpl(
            final String acceleratorDelimiter, final Font acceleratorFont, final String propertyPrefix,
            final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap, final JMenuItem mi, final Rectangle viewRect) {
        return new MenuItemLayoutHelper(mi, checkIcon, arrowIcon, viewRect, defaultTextIconGap, acceleratorDelimiter,
                mi.getComponentOrientation().isLeftToRight(), mi.getFont(), acceleratorFont,
                MenuItemLayoutHelper.useCheckAndArrow(mi), propertyPrefix);
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
        return getPreferredMenuItemSizeImpl(c, checkIcon, arrowIcon, defaultTextIconGap);
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
