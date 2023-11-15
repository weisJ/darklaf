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
package com.github.weisj.darklaf.ui;

import java.awt.*;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.nativelaf.DecorationsHandler;
import com.github.weisj.darklaf.properties.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.*;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public class DarkPopupFactory extends PopupFactory {

    private static final Logger LOGGER = LogUtil.getLogger(DarkPopupFactory.class);

    public static final String KEY_NO_DECORATION = "JPopupFactory.noDecorations";
    public static final String KEY_FOCUSABLE_POPUP = "JPopupFactory.focusablePopup";
    public static final String KEY_FORCE_HEAVYWEIGHT = "JPopupFactory.forceHeavyweight";
    public static final String KEY_OPAQUE = "JPopupFactory.opaque";
    public static final String KEY_START_HIDDEN = "JPopupFactory.startHidden";
    public static final String KEY_DOUBLE_BUFFERED = "JPopupFactory.doubleBuffered";

    private HeavyWeightParent heavyWeightParent;

    public static Popup createNoOpPopup() {
        return new NoOpPopup();
    }

    @Override
    public Popup getPopup(final Component owner, final Component contents, final int x, final int y)
            throws IllegalArgumentException {
        Pair<Popup, PopupType> result = getEffectivePopup(owner, contents, x, y);
        Popup popup = result.getFirst();
        PopupType type = result.getSecond();
        setupPopup(type, contents, x, y);
        return popup;
    }

    protected Pair<Popup, PopupType> getEffectivePopup(final Component owner, final Component contents,
            final int x, final int y) {
        Popup popup = super.getPopup(owner, contents, x, y);
        PopupType type = getPopupType(popup);
        boolean forceHeavy = type != PopupType.HEAVY_WEIGHT
                && PropertyUtil.getBooleanProperty(contents, KEY_FORCE_HEAVYWEIGHT);
        LOGGER.fine("Received popup of type " + type + "  but requested a heavy weight popup.");
        if (forceHeavy) {
            LOGGER.fine("Forcing heavy weight popup");
            // Heavy weight owner forces a heavyweight popup.
            Window targetWindow = DarkUIUtil.getWindow(owner);
            popup = super.getPopup(getHeavyWeightParent(targetWindow), contents, x, y);
            type = PopupType.HEAVY_WEIGHT;
        }
        if (type == PopupType.HEAVY_WEIGHT) {
            Window window = DarkUIUtil.getWindow(contents);
            if (owner != null && window != null
                    && !Objects.equals(window.getGraphicsConfiguration(), owner.getGraphicsConfiguration())) {
                /*
                 * Window uses incorrect graphics configuration. Setting the focusable window state will force the
                 * PopupFactory to dispose it and create a new window.
                 */
                window.setFocusableWindowState(true);
                popup = super.getPopup(getHeavyWeightParent(DarkUIUtil.getWindow(owner)), contents, x, y);
                window = DarkUIUtil.getWindow(contents);
            }
            if (window != null) {
                window.setLocation(x, y);
            }
        }
        return new Pair<>(popup, type);
    }

    public static PopupType getPopupType(final Popup popup) {
        String popupClassName = popup.getClass().getSimpleName();
        if (popupClassName.endsWith("LightWeightPopup")) {
            return PopupType.LIGHT_WEIGHT;
        }
        if (popupClassName.endsWith("MediumWeightPopup")) {
            return PopupType.MEDIUM_WEIGHT;
        }
        return PopupType.HEAVY_WEIGHT;
    }

    protected void setupPopup(final PopupType type, final Component contents, final int x, final int y) {
        if (type == PopupType.MEDIUM_WEIGHT) {
            JRootPane rootPane = SwingUtilities.getRootPane(contents);
            // Prevents decorations from being reinstalled.
            if (rootPane != null) {
                rootPane.putClientProperty(DarkRootPaneUI.KEY_NO_DECORATIONS_UPDATE, true);
            }
        } else if (type == PopupType.HEAVY_WEIGHT) {
            Window window = SwingUtilities.getWindowAncestor(contents);
            if (window != null) {
                LOGGER.fine(() -> "Configuring popup for " + contents);
                boolean isFocusable = PropertyUtil.getBooleanProperty(contents, KEY_FOCUSABLE_POPUP);
                boolean startHidden = PropertyUtil.getBooleanProperty(contents, KEY_START_HIDDEN);
                setupHeavyWeightWindow(window, contents, isFocusable, startHidden);
                window.setLocation(x, y);
            }
        }
    }

    protected void setupHeavyWeightWindow(final Window window, final Component contents,
            final boolean isFocusable, final boolean startHidden) {
        boolean noDecorations = PropertyUtil.getBooleanProperty(contents, KEY_NO_DECORATION);
        boolean opaque = PropertyUtil.getBooleanProperty(contents, KEY_OPAQUE, true);
        LOGGER.fine("Popup config [noDecorations: " + noDecorations + ", opaque: " + opaque + "]");
        JRootPane rootPane = window instanceof RootPaneContainer ? ((RootPaneContainer) window).getRootPane() : null;
        if (rootPane != null && contents instanceof Container) {
            JPanel contentPane = new JPanel(new BorderLayout());
            contentPane.add(contents);
            rootPane.setContentPane(contentPane);
        }
        setupFocusableWindowState(isFocusable, window);
        setupWindowBackground(window, rootPane, contents, opaque, !noDecorations);
        setupWindowDecorations(window, rootPane, noDecorations);
        setupWindowOpacity(startHidden, window);
    }

    protected void setupWindowBackground(final Window window, final JRootPane rootPane,
            final Component content, final boolean opaque, final boolean decorations) {
        if (rootPane == null) {
            return;
        }

        Color bg = opaque
                ? ColorUtil.toAlpha(rootPane.getBackground(), 255)
                : getTranslucentPopupBackground(decorations);
        Component p = content.getParent();

        /*
         * Sometimes the background is java.awt.SystemColor[i=7] It results in a flash of white background,
         * that is repainted with the proper popup background later. That is why we set window background
         * explicitly.
         */
        if (!opaque) {
            boolean doubleBufferHint = PropertyUtil.getBooleanProperty(content, KEY_DOUBLE_BUFFERED);
            LOGGER.fine("Content wants to be double buffered = " + doubleBufferHint);
            setupDoubleBuffering(rootPane);
        }

        while (p != null && p != window) {
            p.setBackground(bg);
            if (p instanceof JComponent) {
                ((JComponent) p).setOpaque(opaque);
            }
            p = p.getParent();
        }
        window.setBackground(bg);
    }

    private void setupDoubleBuffering(final JRootPane rootPane) {
        // On Linux (X11) Java 8 the regular fix doesn't work and the hacky workaround is needed.
        boolean useDoubleBuffering = false;
        LOGGER.fine("Content can use double buffering = " + useDoubleBuffering);

        // See: https://bugs.java.com/bugdatabase/view_bug.do?bug_id=6848852
        rootPane.setDoubleBuffered(useDoubleBuffering);
        setUseDoubleBuffer(rootPane, useDoubleBuffering);
        setUseDoubleBuffer(rootPane.getGlassPane(), useDoubleBuffering);
        setUseDoubleBuffer(rootPane.getLayeredPane(), useDoubleBuffering);
        setUseDoubleBuffer(rootPane.getContentPane(), useDoubleBuffering);
    }

    private void setUseDoubleBuffer(final Component c, final boolean enabled) {
        if (c instanceof JComponent) {
            setUseDoubleBuffer((JComponent) c, enabled);
        }
    }

    private void setUseDoubleBuffer(final JComponent c, final boolean enabled) {
        if (c != null) {
            c.setDoubleBuffered(enabled);
        }
    }

    protected void setupFocusableWindowState(final boolean isFocusable, final Window window) {
        if (isFocusable && !window.getFocusableWindowState()) {
            LOGGER.fine("Window isn't focusable but needs to be. Disposing the window");
            window.dispose();
            window.setFocusableWindowState(true);
        }
    }

    protected void setupWindowDecorations(final Window window, final JRootPane rootPane, final boolean noDecorations) {
        if (rootPane != null) {
            /*
             * To ensure truly undecorated windows we need to specify the Window.shadow property to remove
             * shadows on macOS.
             */
            rootPane.putClientProperty("Window.shadow", !noDecorations);
        }
        if (noDecorations) {
            // We also uninstall if decorations aren't enabled/supported as a reused window could be
            // decorated previously.
            DecorationsHandler.getSharedInstance().uninstallPopupWindow(window);
        } else if (DecorationsHandler.getSharedInstance().isCustomDecorationSupported()) {
            DecorationsHandler.getSharedInstance().installPopupWindow(window);
        }
    }

    protected void setupWindowOpacity(final boolean startHidden, final Window window) {
        boolean translucencySupported = GraphicsUtil.supportsTransparency(window);
        LOGGER.fine("Window supports opacity (translucency + per pixel translucency) = " + translucencySupported);
        if (startHidden && translucencySupported) {
            try {
                window.setOpacity(0);
            } catch (final Exception e) {
                // We shouldn't throw here as a popup failure shouldn't break the GUI.
                LOGGER.log(Level.SEVERE, "We shouldn't be here: ", e);
            }
        }
    }

    private HeavyWeightParent getHeavyWeightParent(final Container owner) {
        if (heavyWeightParent == null) {
            JComponent box = Box.createHorizontalBox();
            super.getPopup(null, box, 0, 0);
            heavyWeightParent = new HeavyWeightParent(DarkUIUtil.getWindow(box));
        }
        heavyWeightParent.setOwner(owner);
        return heavyWeightParent;
    }

    protected Color getTranslucentPopupBackground(final boolean decorated) {
        Color c = UIManager.getColor("PopupMenu.translucentBackground");
        if (!decorated) {
            c = new DarkColorUIResource(ColorUtil.toAlpha(c, 0));
        }
        return c;
    }


    private static class HeavyWeightParent extends JComponent {

        private int parentRequestCount = 0;
        private final Window heavyWeightParent;
        private Container owner;

        private HeavyWeightParent(final Window heavyWeightParent) {
            this.heavyWeightParent = heavyWeightParent;
        }

        public void setOwner(final Container owner) {
            parentRequestCount = 0;
            this.owner = owner;
        }

        @Override
        public GraphicsConfiguration getGraphicsConfiguration() {
            return super.getGraphicsConfiguration();
        }

        @Override
        public Container getParent() {
            if (parentRequestCount == 0) {
                parentRequestCount++;
                return heavyWeightParent;
            }
            return owner;
        }
    }

    public enum PopupType {
        LIGHT_WEIGHT,
        MEDIUM_WEIGHT,
        HEAVY_WEIGHT
    }

    private static class NoOpPopup extends Popup {
        @Override
        public void show() {}

        @Override
        public void hide() {}
    }
}
