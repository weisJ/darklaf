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
 *
 */
package com.github.weisj.darklaf.ui;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.platform.DecorationsHandler;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class DarkPopupFactory extends PopupFactory {

    public static final String KEY_NO_DECORATION = "JPopupFactory.noDecorations";
    public static final String KEY_FOCUSABLE_POPUP = "JPopupFactory.focusablePopup";
    public static final String KEY_FORCE_HEAVYWEIGHT = "JPopupFactory.forceHeavyweight";
    public static final String KEY_START_HIDDEN = "JPopupFactory.startHidden";
    public static final String KEY_MAKE_VISIBLE = "JPopupFactory.makeVisible";
    public static final String KEY_OPAQUE = "JPopupFactory.opaque";

    private HeavyWeightParent heavyWeightParent;

    @Override
    public Popup getPopup(final Component owner, final Component contents,
                          final int x, final int y) throws IllegalArgumentException {
        if (heavyWeightParent == null) {
            JComponent box = Box.createHorizontalBox();
            super.getPopup(null, box, 0, 0);
            heavyWeightParent = new HeavyWeightParent(DarkUIUtil.getWindow(box));
        }
        boolean isJComponent = contents instanceof JComponent;
        Popup popup = super.getPopup(owner, contents, x, y);
        boolean isMediumWeight = popup.getClass().getSimpleName().endsWith("MediumWeightPopup");
        boolean isLightWeight = popup.getClass().getSimpleName().endsWith("LightWeightPopup");
        boolean forceHeavy = isJComponent
                             && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_FORCE_HEAVYWEIGHT));
        boolean isFocusable = isJComponent
                              && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_FOCUSABLE_POPUP));
        boolean startHidden = isJComponent
                              && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_START_HIDDEN));
        if (forceHeavy && (isMediumWeight || isLightWeight)) {
            // null owner forces a heavyweight popup.
            popup = super.getPopup(heavyWeightParent, contents, x, y);
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
            boolean noDecorations = (isJComponent
                                     && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_NO_DECORATION)));
            boolean opaque = isJComponent
                             && Boolean.TRUE.equals(((JComponent) contents).getClientProperty(KEY_OPAQUE));
            window.setBackground(PaintUtil.TRANSPARENT_COLOR);
            if (window instanceof RootPaneContainer) {
                JRootPane rootPane = ((RootPaneContainer) window).getRootPane();
                if (opaque) {
                    window.setBackground(rootPane.getBackground());
                } else {
                    rootPane.setBackground(PaintUtil.TRANSPARENT_COLOR);
                    rootPane.setOpaque(false);
                }
            }

            if (isFocusable && !window.getFocusableWindowState()) {
                window.dispose();
                window.setFocusableWindowState(true);
            }

            if (DecorationsHandler.getSharedInstance().isCustomDecorationSupported()) {
                if (noDecorations) {
                    DecorationsHandler.getSharedInstance().uninstallPopupWindow(window);
                } else {
                    DecorationsHandler.getSharedInstance().installPopupWindow(window);
                }
            }
            if (startHidden) {
                try {
                    ((JComponent) contents).putClientProperty(KEY_MAKE_VISIBLE, true);
                    window.setOpacity(0);
                } catch (Exception ignored) {}
            }
        }
        return popup;
    }

    private static class HeavyWeightParent extends JComponent {

        private final Window window;

        private HeavyWeightParent(final Window window) {
            this.window = window;
        }

        @Override
        public Container getParent() {
            return window;
        }

        public Window getWindow() {
            return window;
        }
    }
}
