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

import com.github.weisj.darklaf.platform.DecorationsHandler;
import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;

import javax.swing.*;
import java.awt.*;

public class DarkPopupFactory extends PopupFactory {

    public static final String KEY_NO_DECORATION = "JPopupFactory.noDecorations";
    public static final String KEY_FOCUSABLE_POPUP = "JPopupFactory.focusablePopup";
    public static final String KEY_FORCE_HEAVYWEIGHT = "JPopupFactory.forceHeavyweight";
    public static final String KEY_START_HIDDEN = "JPopupFactory.startHidden";

    @Override
    public Popup getPopup(final Component owner, final Component contents,
                          final int x, final int y) throws IllegalArgumentException {
        Popup popup = super.getPopup(owner, contents, x, y);
        boolean isMediumWeight = popup.getClass().getSimpleName().endsWith("MediumWeightPopup");
        boolean isLightWeight = popup.getClass().getSimpleName().endsWith("LightWeightPopup");
        boolean forceHeavy = contents instanceof JComponent
                             && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_FORCE_HEAVYWEIGHT));
        boolean isFocusable = contents instanceof JComponent
                              && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_FOCUSABLE_POPUP));
        boolean startHidden = contents instanceof JComponent
                              && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_START_HIDDEN));
        if (forceHeavy && (isMediumWeight || isLightWeight)) {
            // null owner forces a heavyweight popup.
            popup = super.getPopup(null, contents, x, y);
        }
        if (isMediumWeight) {
            JRootPane rootPane = SwingUtilities.getRootPane(contents);
            // Prevents decorations from being reinstalled.
            if (rootPane != null) rootPane.putClientProperty(DarkRootPaneUI.KEY_NO_DECORATIONS_UPDATE, true);
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
                rootPane.putClientProperty(DarkRootPaneUI.KEY_NO_DECORATIONS, true);
                install = !Boolean.TRUE.equals(rootPane.getClientProperty(KEY_NO_DECORATION));
            }
            if (install) {
                DecorationsHandler.getSharedInstance().installPopupWindow(window);
            } else {
                DecorationsHandler.getSharedInstance().uninstallPopupWindow(window);
            }
            if (startHidden) {
                ((JComponent) contents).putClientProperty(DarkPopupMenuUI.KEY_MAKE_VISIBLE, true);
                window.setOpacity(0.0f);
            }
            if (isFocusable && !window.getFocusableWindowState()) {
                window.dispose();
                window.setFocusableWindowState(true);
            }
        }
        return popup;
    }
}
