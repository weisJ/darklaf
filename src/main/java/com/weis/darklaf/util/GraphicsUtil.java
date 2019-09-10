package com.weis.darklaf.util;

import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.Map;

public final class GraphicsUtil {

    private GraphicsUtil() {
    }

    public static void setupAntialiasing(final Graphics g2) {
        setupAntialiasing(g2, true, false);
    }

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
        g2.setComposite(AlphaComposite.getInstance(3, alpha));
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
}
