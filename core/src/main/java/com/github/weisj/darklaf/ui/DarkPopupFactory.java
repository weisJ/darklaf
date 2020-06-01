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

import com.github.weisj.darklaf.platform.DecorationsHandler;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.Pair;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkPopupFactory extends PopupFactory {

    public static final String KEY_NO_DECORATION = "JPopupFactory.noDecorations";
    public static final String KEY_FOCUSABLE_POPUP = "JPopupFactory.focusablePopup";
    public static final String KEY_FORCE_HEAVYWEIGHT = "JPopupFactory.forceHeavyweight";
    public static final String KEY_START_HIDDEN = "JPopupFactory.startHidden";
    public static final String KEY_OPAQUE = "JPopupFactory.opaque";

    private HeavyWeightParent heavyWeightParent;

    @Override
    public Popup getPopup(final Component owner, final Component contents,
                          final int x, final int y) throws IllegalArgumentException {
        Pair<Popup, PopupType> result = getEffectivePopup(owner, contents, x, y);
        Popup popup = result.getFirst();
        PopupType type = result.getSecond();
        setupPopup(popup, type, contents);
        return popup;
    }

    protected Pair<Popup, PopupType> getEffectivePopup(final Component owner, final Component contents,
                                                       final int x, final int y) {
        Popup popup = super.getPopup(owner, contents, x, y);
        PopupType type = getPopupType(popup);
        boolean forceHeavy = type != PopupType.HEAVY_WEIGHT
                             && PropertyUtil.getBooleanProperty(contents, KEY_FORCE_HEAVYWEIGHT);
        if (forceHeavy) {
            // null owner forces a heavyweight popup.
            Popup p = super.getPopup(getHeavyWeightParent(), contents, x, y);
            return new Pair<>(p, PopupType.HEAVY_WEIGHT);
        }
        return new Pair<>(popup, type);
    }

    protected PopupType getPopupType(final Popup popup) {
        String popupClassName = popup.getClass().getSimpleName();
        if (popupClassName.endsWith("LightWeightPopup")) return PopupType.LIGHT_WEIGHT;
        if (popupClassName.endsWith("MediumWeightPopup")) return PopupType.MEDIUM_WEIGHT;
        return PopupType.HEAVY_WEIGHT;
    }

    protected void setupPopup(final Popup popup, final PopupType type, final Component contents) {
        if (type == PopupType.MEDIUM_WEIGHT) {
            JRootPane rootPane = SwingUtilities.getRootPane(contents);
            // Prevents decorations from being reinstalled.
            if (rootPane != null) rootPane.putClientProperty(DarkRootPaneUI.KEY_NO_DECORATIONS_UPDATE, true);
        } else if (type == PopupType.HEAVY_WEIGHT) {
            Window window = SwingUtilities.getWindowAncestor(contents);
            if (window != null) {
                boolean isFocusable = PropertyUtil.getBooleanProperty(contents, KEY_FOCUSABLE_POPUP);
                boolean startHidden = PropertyUtil.getBooleanProperty(contents, KEY_START_HIDDEN);
                setupWindow(window, contents, isFocusable, startHidden);
            }
        }
    }

    protected void setupWindow(final Window window, final Component contents,
                               final boolean isFocusable, final boolean startHidden) {
        boolean noDecorations = PropertyUtil.getBooleanProperty(contents, KEY_NO_DECORATION);
        boolean opaque = PropertyUtil.getBooleanProperty(contents, KEY_OPAQUE);
        setupWindowBackground(window, opaque, !noDecorations);
        setupWindowFocusableState(isFocusable, window);
        setupWindowDecorations(window, noDecorations);
        setupWindowOpacity(startHidden, window);
    }

    protected void setupWindowBackground(final Window window, final boolean opaque, final boolean decorations) {
        // Sometimes the background is java.awt.SystemColor[i=7]
        // It results in a flash of white background, that is repainted with
        // the proper popup background later.
        // That is why we set window background explicitly.
        if (window instanceof RootPaneContainer) {
            JRootPane rootPane = ((RootPaneContainer) window).getRootPane();
            rootPane.setOpaque(opaque);
            if (opaque) {
                window.setBackground(rootPane.getBackground());
            } else {
                Color bg = getTranslucentPopupBackground(decorations);
                window.setBackground(bg);
            }
        }
    }

    protected void setupWindowFocusableState(final boolean isFocusable, final Window window) {
        if (isFocusable && !window.getFocusableWindowState()) {
            window.dispose();
            window.setFocusableWindowState(true);
        }
    }

    protected void setupWindowDecorations(final Window window, final boolean noDecorations) {
        if (DecorationsHandler.getSharedInstance().isCustomDecorationSupported()) {
            if (noDecorations) {
                DecorationsHandler.getSharedInstance().uninstallPopupWindow(window);
            } else {
                DecorationsHandler.getSharedInstance().installPopupWindow(window);
            }
        }
    }

    protected void setupWindowOpacity(final boolean startHidden, final Window window) {
        boolean translucencySupported = DarkUIUtil.supportsTransparency(window);
        if (startHidden && translucencySupported) {
            try {
                window.setOpacity(0);
            } catch (Exception ignored) {}
        }
    }

    protected HeavyWeightParent getHeavyWeightParent() {
        if (heavyWeightParent == null) {
            JComponent box = Box.createHorizontalBox();
            super.getPopup(null, box, 0, 0);
            heavyWeightParent = new HeavyWeightParent(DarkUIUtil.getWindow(box));
        }
        return heavyWeightParent;
    }

    protected Color getTranslucentPopupBackground(final boolean decorated) {
        Color c = UIManager.getColor("PopupMenu.translucentBackground");
        if (!decorated) c = new DarkColorUIResource(ColorUtil.toAlpha(c, 0));
        return c;
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

    protected enum PopupType {
        LIGHT_WEIGHT,
        MEDIUM_WEIGHT,
        HEAVY_WEIGHT
    }
}
