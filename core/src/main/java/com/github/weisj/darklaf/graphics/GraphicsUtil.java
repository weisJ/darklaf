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
package com.github.weisj.darklaf.graphics;

import java.awt.*;
import java.util.Map;

import com.github.weisj.darklaf.util.PropertyValue;
import com.github.weisj.darklaf.util.SystemInfo;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class GraphicsUtil {

    private GraphicsUtil() {}

    public static GraphicsContext setupAntialiasing(final Graphics g2) {
        return setupAntialiasing(g2, true, false);
    }

    public static GraphicsContext setupAntialiasing(final Graphics g2, final boolean enableAA,
                                                    final boolean ignoreSystemSettings) {
        GraphicsContext config = new GraphicsContext(g2);
        if (g2 instanceof Graphics2D) {
            Graphics2D g = (Graphics2D) g2;
            Toolkit tk = Toolkit.getDefaultToolkit();
            Map<?, ?> map = (Map<?, ?>) tk.getDesktopProperty("awt.font.desktophints");
            if (map != null && !ignoreSystemSettings) {
                g.addRenderingHints(map);
            } else {
                g.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING,
                                   enableAA
                                           ? RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HBGR
                                           : RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
            }
        }
        return config;
    }

    public static GraphicsContext setupAAPainting(final Graphics g) {
        GraphicsContext config = new GraphicsContext(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);
        return config;
    }

    public static GraphicsContext paintWithAlpha(final Graphics g, final float alpha) {
        assert 0.0F <= alpha && alpha <= 1.0F : "alpha should be in range 0.0f .. 1.0f";

        GraphicsContext config = new GraphicsContext(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha));
        return config;
    }

    public static GraphicsContext setupStrokePainting(final Graphics g) {
        Graphics2D g2 = (Graphics2D) g;
        GraphicsContext context = new GraphicsContext(g2);
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL,
                            useQuartz() ? RenderingHints.VALUE_STROKE_PURE
                                    : RenderingHints.VALUE_STROKE_NORMALIZE);
        return context;
    }

    public static boolean isHighDpiEnabled() {
        return PropertyValue.TRUE.equalsIgnoreCase(System.getProperty("hidpi"));
    }

    public static boolean useQuartz() {
        return SystemInfo.isMac && PropertyValue.TRUE.equals(System.getProperty("apple.awt.graphics.UseQuartz"));
    }
}
