/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.util;

import com.weis.darklaf.log.LogFormatter;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.Map;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class GraphicsUtil {

    /**
     * The scaling factor.
     */
    public static final double SCALE;
    public static final double SCALE_X;
    public static final double SCALE_Y;
    private static final Logger LOGGER = Logger.getLogger(ImageUtil.class.getName());

    static {
        LOGGER.setUseParentHandlers(false);
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new LogFormatter());
        LOGGER.addHandler(handler);

        var mode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        var screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        SCALE_X = mode.getWidth() / (double) screenSize.width;
        SCALE_Y = mode.getHeight() / (double) screenSize.height;
        SCALE = SCALE_X;
        LOGGER.info("Using screen scaling SCALE_X=" + SCALE_X + ", SCALE_Y=" + SCALE_Y);

    }

    @Contract(pure = true)
    private GraphicsUtil() {
    }

    @NotNull
    public static GraphicsContext setupAntialiasing(final Graphics g2) {
        return setupAntialiasing(g2, true, false);
    }

    @NotNull
    public static GraphicsContext setupAntialiasing(final Graphics g2, final boolean enableAA,
                                                    final boolean ignoreSystemSettings) {
        var config = new GraphicsContext(g2);
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

    @NotNull
    public static GraphicsContext setupAAPainting(final Graphics g) {
        GraphicsContext config = new GraphicsContext(g);
        Graphics2D g2 = (Graphics2D) g;
        g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);
        return config;
    }

    @NotNull
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
                            DarkUIUtil.USE_QUARTZ ? RenderingHints.VALUE_STROKE_PURE
                                                  : RenderingHints.VALUE_STROKE_NORMALIZE);
        return context;
    }

    public static boolean isHighDpiEnabled() {
        return "true".equalsIgnoreCase(System.getProperty("hidpi"));
    }
}
