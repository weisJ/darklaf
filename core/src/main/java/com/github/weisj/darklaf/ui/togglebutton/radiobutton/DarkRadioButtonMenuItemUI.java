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
package com.github.weisj.darklaf.ui.togglebutton.radiobutton;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.compatibility.MenuItemLayoutHelper;
import com.github.weisj.darklaf.listener.MouseClickListener;
import com.github.weisj.darklaf.ui.menu.DarkMenuItemUIBase;
import com.github.weisj.darklaf.ui.menu.MenuItemLayoutDelegate;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonMenuItemConstants;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

/** @author Jannis Weis */
public class DarkRadioButtonMenuItemUI extends DarkMenuItemUIBase implements ToggleButtonMenuItemConstants {

    private final MouseClickListener clickListener = e -> SwingUtilities.invokeLater(() -> {
        if (menuItem != null) menuItem.setArmed(true);
    });

    protected MenuItemLayoutDelegate layoutDelegate = new ToggleButtonMenuItemLayoutDelegate(null);
    private Icon stateIcon;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkRadioButtonMenuItemUI();
    }

    @Override
    public String getPropertyPrefix() {
        return "RadioButtonMenuItem";
    }

    @Override
    public void installDefaults() {
        super.installDefaults();
        acceleratorFont = UIManager.getFont("MenuItem.font");
        acceleratorForeground = UIManager.getColor("MenuItem.foreground");
        acceleratorSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
        menuItem.putClientProperty(KEY_NO_NOT_CLOSE_ON_CLICK, UIManager.getBoolean(KEY_NO_NOT_CLOSE_ON_CLICK));
        installIcons();
        checkIcon = getStateIcon(menuItem);
    }

    protected void installIcons() {
        stateIcon = UIManager.getIcon("RadioButton.icon");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        menuItem.addMouseListener(clickListener);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        menuItem.removeMouseListener(clickListener);
    }

    @Override
    public MenuItemLayoutHelper getMenuItemLayoutHelper(final Icon checkIcon, final Icon arrowIcon,
            final int defaultTextIconGap, final JMenuItem mi, final Rectangle viewRect) {
        if (mi.getIcon() == null) {
            layoutDelegate.setDelegate(mi);
            return super.getMenuItemLayoutHelper(null, arrowIcon, defaultTextIconGap, layoutDelegate, viewRect);
        } else {
            return super.getMenuItemLayoutHelper(checkIcon, arrowIcon, defaultTextIconGap, mi, viewRect);
        }
    }

    @Override
    public void paintCheckIcon(final Graphics g2, final JMenuItem mi, final MenuItemLayoutHelper lh,
            final MenuItemLayoutHelper.MILayoutResult lr, final Color foreground) {
        Rectangle rect = lr.getCheckRect();
        if (mi.getIcon() == null) {
            rect.y = lr.getIconRect().y;
            rect.height = lr.getIconRect().height;
        }
        getStateIcon(mi).paintIcon(mi, g2, rect.x, rect.y);
    }

    @Override
    public Insets getMargin(JMenuItem item) {
        Insets margin = super.getMargin(item);
        Icon icon = getStateIcon(item);
        if (icon instanceof VisualPaddingProvider) {
            int padding = ((VisualPaddingProvider) icon).getVisualPaddings(item).left;
            margin.left -= padding;
        }
        return margin;
    }

    protected Icon getStateIcon(final AbstractButton b) {
        return stateIcon;
    }

    protected class ToggleButtonMenuItemLayoutDelegate extends MenuItemLayoutDelegate {

        public ToggleButtonMenuItemLayoutDelegate(final JMenuItem delegate) {
            super(delegate);
        }

        @Override
        public Icon getIcon() {
            return checkIcon;
        }
    }
}
