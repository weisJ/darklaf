/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.internalframe;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicInternalFrameUI;

import com.github.weisj.darklaf.ui.util.DarkUIUtil;

/** @author Jannis Weis */
public class DarkInternalFrameUI extends BasicInternalFrameUI implements PropertyChangeListener {

    protected JMenuBar currentMenuBar;

    public DarkInternalFrameUI(final JInternalFrame b) {
        super(b);
    }

    public static ComponentUI createUI(final JComponent b) {
        return new DarkInternalFrameUI((JInternalFrame) b);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        frame.setOpaque(false);
    }

    @Override
    protected JComponent createNorthPane(final JInternalFrame w) {
        this.titlePane = new DarkInternalFrameTitlePane(w);
        this.titlePane.setBorder(BorderFactory.createEmptyBorder(1, 1, 1, 0));
        return this.titlePane;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        if (UIManager.getBoolean("InternalFrame.useExternalMenuBar")) {
            frame.addPropertyChangeListener(this);
        }
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        frame.removePropertyChangeListener(this);
    }

    protected void updateActiveJMenuBar(final JDesktopPane desktopPane, final JMenuBar menuBar) {
        DarkDesktopPaneUI ui = DarkUIUtil.getUIOfType(desktopPane.getUI(), DarkDesktopPaneUI.class);
        if (ui != null) {
            ui.setActiveJMenuBar(menuBar);
        }
    }

    protected Rectangle getContentRegion(final JDesktopPane desktopPane) {
        DarkDesktopPaneUI ui = DarkUIUtil.getUIOfType(desktopPane.getUI(), DarkDesktopPaneUI.class);
        if (ui != null) {
            return ui.getContentRegion();
        }
        return null;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        boolean isMenuBarProp = JInternalFrame.MENU_BAR_PROPERTY.equals(key);
        boolean isSelectedProp = JInternalFrame.IS_SELECTED_PROPERTY.equals(key);
        boolean isIconProperty = JInternalFrame.IS_ICON_PROPERTY.equals(key);
        if (isMenuBarProp || isSelectedProp || isIconProperty) {
            if (isMenuBarProp) {
                currentMenuBar = frame.getJMenuBar();
                frame.getRootPane().remove(currentMenuBar);
                currentMenuBar.setPreferredSize(new Dimension(0, 0));
                frame.doLayout();
            }
            JDesktopPane desktopPane = frame.getDesktopPane();
            if (desktopPane != null) {
                if (shouldShowMenuBar(frame)) {
                    updateActiveJMenuBar(desktopPane, currentMenuBar);
                } else if (!shouldShowMenuBar(desktopPane.getSelectedFrame())) {
                    updateActiveJMenuBar(desktopPane, null);
                }
            }
        } else if (JInternalFrame.IS_MAXIMUM_PROPERTY.equals(key) && frame.isMaximum()) {
            Rectangle bounds = getContentRegion(frame.getDesktopPane());
            if (bounds != null) {
                frame.setBounds(bounds);
            }
        }
    }

    protected boolean shouldShowMenuBar(final JInternalFrame internalFrame) {
        if (internalFrame == null) return false;
        return internalFrame.isSelected() && !internalFrame.isClosed() && !internalFrame.isIcon();
    }
}
