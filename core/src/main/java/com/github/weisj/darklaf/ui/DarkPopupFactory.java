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
package com.github.weisj.darklaf.ui;

import com.github.weisj.darklaf.platform.Decorations;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.ui.tooltip.DarkTooltipBorder;

import javax.swing.*;
import java.awt.*;

public class DarkPopupFactory extends PopupFactory {

    @Override
    public Popup getPopup(final Component owner, final Component contents,
                          final int x, final int y) throws IllegalArgumentException {
        Popup popup = super.getPopup(owner, contents, x, y);
        if (popup.getClass().getSimpleName().endsWith("MediumWeightPopup")) {
            if (contents instanceof JToolTip
                && ((JToolTip) contents).getBorder() instanceof DarkTooltipBorder) {
                // null owner forces a heavyweight popup.
                popup = super.getPopup(null, contents, x, y);
            } else {
                JRootPane rootPane = SwingUtilities.getRootPane(contents);
                // Prevents decorations from being reinstalled.
                if (rootPane != null) rootPane.putClientProperty(DarkRootPaneUI.KEY_IS_MEDIUM_WEIGHT_POPUP_ROOT, true);
            }
        }
        // Sometimes the background is java.awt.SystemColor[i=7]
        // It results in a flash of white background, that is repainted with
        // the proper popup background later.
        // That is why we set window background explicitly.
        Window window = SwingUtilities.getWindowAncestor(contents);
        if (window != null) {
            window.setBackground(UIManager.getColor("PopupMenu.translucentBackground"));
            boolean install = true;
            if (window instanceof RootPaneContainer) {
                JRootPane rootPane = ((RootPaneContainer) window).getRootPane();
                rootPane.putClientProperty(DarkRootPaneUI.KEY_IS_POPUP, true);
                install = !Boolean.TRUE.equals(rootPane.getClientProperty(DarkRootPaneUI.KEY_IS_TOOLTIP));
            }
            if (install) {
                Decorations.installPopupWindow(window);
            } else {
                Decorations.uninstallPopupWindow(window);
            }
        }
        return popup;
    }
}
