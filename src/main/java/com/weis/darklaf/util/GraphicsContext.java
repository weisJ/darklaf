package com.weis.darklaf.util;

import java.awt.*;
import java.util.Map;

public class GraphicsContext {
    private final Graphics2D graphics2D;
    private final Composite composite;
    private final Map<?, ?> hintsMap;
    private final Stroke stroke;
    private final Color color;
    private final Paint paint;
    private final Font font;
    private final Shape clip;

    public GraphicsContext(final Graphics g) {
        graphics2D = (Graphics2D) g;
        hintsMap = (Map<?, ?>) graphics2D.getRenderingHints().clone();
        composite = graphics2D.getComposite();
        stroke = graphics2D.getStroke();
        color = graphics2D.getColor();
        paint = graphics2D.getPaint();
        font = graphics2D.getFont();
        clip = graphics2D.getClip();
    }

    public GraphicsContext setAntialiasing(final boolean on) {
        graphics2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                                    on ? RenderingHints.VALUE_ANTIALIAS_ON
                                       : RenderingHints.VALUE_ANTIALIAS_OFF);
        return this;
    }

    public Graphics2D getGraphics() {
        return this.graphics2D;
    }

    public void restore() {
        graphics2D.setRenderingHints(this.hintsMap);
        graphics2D.setComposite(composite);
        graphics2D.setStroke(stroke);
        graphics2D.setColor(color);
        graphics2D.setPaint(paint);
        graphics2D.setFont(font);
        graphics2D.setClip(clip);
    }
}
