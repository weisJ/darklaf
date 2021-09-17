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
import java.awt.geom.AffineTransform;
import java.util.Map;

/**
 * Utility class for scoping changes to a {@link Graphics2D} object.
 *
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class GraphicsContext {
    private final Graphics2D graphics2D;
    private Composite composite;
    private Map<?, ?> hintsMap;
    private Stroke stroke;
    private Color color;
    private Paint paint;
    private Font font;
    private Shape clip;
    private AffineTransform transform;

    public GraphicsContext(final Graphics g) {
        graphics2D = (Graphics2D) g;
        save();
    }

    public Graphics2D getGraphics() {
        return this.graphics2D;
    }

    public void setupStrokePainting() {
        graphics2D.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        graphics2D.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, GraphicsUtil.useQuartz()
                ? RenderingHints.VALUE_STROKE_PURE
                : RenderingHints.VALUE_STROKE_NORMALIZE);
    }

    public void setAlpha(final float alpha) {
        graphics2D.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha));
    }

    public void setupAntialiasing(final boolean enableAA, final boolean ignoreSystemSettings) {
        if (ignoreSystemSettings) {
            graphics2D.setRenderingHint(RenderingHints.KEY_TEXT_ANTIALIASING, enableAA
                    ? RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HBGR
                    : RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
        }
    }

    public void restore() {
        restoreRenderingHints();
        restoreComposite();
        restoreStroke();
        restoreColor();
        restorePaint();
        restoreFont();
        restoreClip();
        restoreTransform();
    }

    public void save() {
        saveRenderingHints();
        saveComposite();
        saveStroke();
        saveColor();
        savePaint();
        saveFont();
        saveClip();
        saveTransform();
    }

    public void restoreTransform() {
        graphics2D.setTransform(transform);
    }

    public void restoreComposite() {
        graphics2D.setComposite(composite);
    }

    public void restoreFont() {
        graphics2D.setFont(font);
    }

    public void restoreRenderingHints() {
        graphics2D.setRenderingHints(this.hintsMap);
    }

    public void restoreStroke() {
        graphics2D.setStroke(stroke);
    }

    public void restoreColor() {
        graphics2D.setColor(color);
    }

    public void restorePaint() {
        graphics2D.setPaint(paint);
    }

    public void restoreClip() {
        graphics2D.setClip(clip);
    }

    public void saveTransform() {
        transform = graphics2D.getTransform();
    }

    public void saveComposite() {
        composite = graphics2D.getComposite();
    }

    public void saveFont() {
        font = graphics2D.getFont();
    }

    public void saveRenderingHints() {
        hintsMap = graphics2D.getRenderingHints();
    }

    public void saveStroke() {
        stroke = graphics2D.getStroke();
    }

    public void saveColor() {
        color = graphics2D.getColor();
    }

    public void savePaint() {
        paint = graphics2D.getPaint();
    }

    public void saveClip() {
        clip = graphics2D.getClip();
    }
}
