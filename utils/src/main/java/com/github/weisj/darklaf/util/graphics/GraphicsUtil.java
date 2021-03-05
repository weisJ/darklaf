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
package com.github.weisj.darklaf.util.graphics;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.PropertyValue;
import com.github.weisj.darklaf.util.SystemInfo;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class GraphicsUtil {

    public static final String DESKTOP_HINTS_KEY = "awt.font.desktophints";
    public static final String KEY_OPAQUE_BUFFERED = "JComponent.opaqueBuffered";

    private GraphicsUtil() {}

    public static void setOpaqueBuffered(final JComponent c, final boolean opaqueBuffered) {
        if (c != null) c.putClientProperty(KEY_OPAQUE_BUFFERED, opaqueBuffered);
    }

    public static boolean isOpaqueBuffered(final JComponent c) {
        if (!SystemInfo.isWindows) return false;
        return PropertyUtil.getBooleanProperty(c, KEY_OPAQUE_BUFFERED);
    }

    public static GraphicsContext setupAntialiasing(final Graphics g2) {
        return setupAntialiasing(g2, true, false);
    }

    public static GraphicsContext setupAntialiasing(final Graphics g2, final boolean enableAA,
            final boolean ignoreSystemSettings) {
        GraphicsContext config = new GraphicsContext(g2);
        config.setupAntialiasing(enableAA, ignoreSystemSettings);
        return config;
    }

    public static GraphicsContext paintWithAlpha(final Graphics g, final float alpha) {
        assert 0.0F <= alpha && alpha <= 1.0F : "alpha should be in range 0.0f .. 1.0f";

        GraphicsContext config = new GraphicsContext(g);
        config.setAlpha(alpha);
        return config;
    }

    public static GraphicsContext setupStrokePainting(final Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        GraphicsContext context = new GraphicsContext(g2);
        context.setupStrokePainting();
        return context;
    }

    public static boolean isHighDpiEnabled() {
        return PropertyValue.TRUE.equalsIgnoreCase(System.getProperty("hidpi"));
    }

    public static boolean useQuartz() {
        return SystemInfo.isMac && PropertyValue.TRUE.equals(System.getProperty("apple.awt.graphics.UseQuartz"));
    }

    public static boolean supportsTransparency(final Window window) {
        if (window == null || GraphicsEnvironment.isHeadless()) return false;
        GraphicsConfiguration gc = window.getGraphicsConfiguration();
        if (gc == null) return false;
        return supportsTransparency(gc.getDevice());
    }

    public static boolean supportsTransparency() {
        if (GraphicsEnvironment.isHeadless()) return false;
        return supportsTransparency(GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice());
    }

    public static boolean supportsTransparency(final GraphicsDevice gd) {
        return gd != null
                && gd.isWindowTranslucencySupported(GraphicsDevice.WindowTranslucency.PERPIXEL_TRANSLUCENT)
                && gd.isWindowTranslucencySupported(GraphicsDevice.WindowTranslucency.TRANSLUCENT);
    }
}
