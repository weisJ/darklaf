/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.progressbar;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicProgressBarUI;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkProgressBarUI extends BasicProgressBarUI implements PropertyChangeListener {

    protected static final String KEY_PREFIX = "JProgressBar.";
    public static final String KEY_FAILED = KEY_PREFIX + "failed";
    public static final String KEY_PASSED = KEY_PREFIX + "passed";

    private static final int CYCLE_TIME_DEFAULT = 800;
    private static final int REPAINT_INTERVAL_DEFAULT = 50;
    private static final int CYCLE_TIME_SIMPLIFIED = 1000;
    private static final int REPAINT_INTERVAL_SIMPLIFIED = 500;
    private Color trackColor;
    private Color progressColor;
    private Color indeterminateStartColor;
    private Color indeterminateEndColor;
    private Color failedColor;
    private Color failedEndColor;
    private Color passedColor;
    private Color passedEndColor;
    private int stripeWidth;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkProgressBarUI();
    }

    protected static boolean hasFailed(final JComponent c) {
        return PropertyUtil.getBooleanProperty(c, KEY_FAILED);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        progressBar.addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        progressBar.removePropertyChangeListener(this);
    }

    @Override
    protected int getBoxLength(final int availableLength, final int otherDimension) {
        return availableLength;
    }

    @Override
    protected void paintIndeterminate(final Graphics g, final JComponent c) {

        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);

            Rectangle r = new Rectangle(progressBar.getSize());
            if (c.isOpaque()) {
                g2.setColor(c.getParent().getBackground());
                g2.fill(r);
            }

            Insets i = progressBar.getInsets();
            DarkUIUtil.applyInsets(r, i);
            int orientation = progressBar.getOrientation();

            Color startColor, endColor;
            if (hasFailed(progressBar)) {
                startColor = failedColor;
                endColor = failedEndColor;
            } else if (hasPassed(progressBar)) {
                startColor = passedColor;
                endColor = passedEndColor;
            } else {
                startColor = getStartColor();
                endColor = getEndColor();
            }

            int pHeight = progressBar.getPreferredSize().height;
            int pWidth = progressBar.getPreferredSize().width;

            int yOffset = r.y + (r.height - pHeight) / 2;
            int xOffset = r.x + (r.width - pWidth) / 2;

            if (isSimplified()) {
                Color[] ca = {startColor, endColor};
                int idx = 0;
                int delta = 10;
                if (orientation == SwingConstants.HORIZONTAL) {
                    for (float offset = r.x; offset - r.x < r.width; offset += delta) {
                        g2.setPaint(ca[(getAnimationIndex() + idx++) % 2]);
                        g2.fill(new Rectangle2D.Float(offset, yOffset, delta, pHeight));
                    }
                } else {
                    for (float offset = r.y; offset - r.y < r.height; offset += delta) {
                        g2.setPaint(ca[(getAnimationIndex() + idx++) % 2]);
                        g2.fill(new Rectangle2D.Float(xOffset, offset, delta, pWidth));
                    }
                }
            } else {
                Shape shape;
                int step = 6;
                if (orientation == SwingConstants.HORIZONTAL) {
                    shape = getShapedRect(r.x, yOffset, r.width, pHeight, pHeight);
                    yOffset = r.y + pHeight / 2;
                    g2.setPaint(new GradientPaint(r.x + getAnimationIndex() * step * 2, yOffset, startColor,
                            r.x + getFrameCount() * step + getAnimationIndex() * step * 2, yOffset, endColor, true));
                } else {
                    shape = getShapedRect(xOffset, r.y, pWidth, r.height, pWidth);
                    xOffset = r.x + pWidth / 2;
                    g2.setPaint(new GradientPaint(xOffset, r.y + getAnimationIndex() * step * 2, startColor, xOffset,
                            r.y + getFrameCount() * step + getAnimationIndex() * step * 2, endColor, true));
                }
                g2.fill(shape);
            }

            // Paint text
            if (progressBar.isStringPainted()) {
                if (progressBar.getOrientation() == SwingConstants.HORIZONTAL) {
                    paintString((Graphics2D) g, i.left, i.top, r.width, r.height, boxRect.x, boxRect.width);
                } else {
                    paintString((Graphics2D) g, i.left, i.top, r.width, r.height, boxRect.y, boxRect.height);
                }
            }
        } finally {
            g2.dispose();
        }
    }

    protected static boolean hasPassed(final JComponent c) {
        return PropertyUtil.getBooleanProperty(c, KEY_PASSED);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        UIManager.put("ProgressBar.repaintInterval",
                isSimplified() ? REPAINT_INTERVAL_SIMPLIFIED : REPAINT_INTERVAL_DEFAULT);
        UIManager.put("ProgressBar.cycleTime", isSimplified() ? CYCLE_TIME_SIMPLIFIED : CYCLE_TIME_DEFAULT);
        trackColor = UIManager.getColor("ProgressBar.trackColor");
        progressColor = UIManager.getColor("ProgressBar.progressColor");
        indeterminateStartColor = UIManager.getColor("ProgressBar.indeterminateStartColor");
        indeterminateEndColor = UIManager.getColor("ProgressBar.indeterminateEndColor");
        stripeWidth = UIManager.getInt("ProgressBar.stripeWidth");
        failedColor = UIManager.getColor("ProgressBar.failedColor");
        failedEndColor = UIManager.getColor("ProgressBar.failedEndColor");
        passedColor = UIManager.getColor("ProgressBar.passedColor");
        passedEndColor = UIManager.getColor("ProgressBar.passedEndColor");
    }

    protected Color getStartColor() {
        return indeterminateStartColor;
    }

    protected Color getEndColor() {
        return indeterminateEndColor;
    }

    private Shape getShapedRect(final float x, final float y, final float w, final float h, final float ar) {
        return new RoundRectangle2D.Float(x, y, w, h, ar, ar);
    }

    private void paintString(final Graphics2D g, final int x, final int y, final int w, final int h,
            final int fillStart, final int amountFull) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        String progressString = progressBar.getString();
        g.setFont(progressBar.getFont());
        Point renderLocation = getStringPlacement(g, progressString, x, y, w, h);

        g.setColor(getSelectionBackground());
        if (progressBar.getOrientation() == SwingConstants.HORIZONTAL) {
            g.drawString(progressString, renderLocation.x, renderLocation.y);
            g.clipRect(fillStart, y, amountFull, h);
        } else { // VERTICAL
            AffineTransform rotate = AffineTransform.getRotateInstance(Math.PI / 2);
            g.setFont(progressBar.getFont().deriveFont(rotate));
            renderLocation = getStringPlacement(g, progressString, x, y, w, h);

            g.drawString(progressString, renderLocation.x, renderLocation.y);

            g.clipRect(x, fillStart, w, amountFull);
        }
        g.setColor(getSelectionForeground());
        g.drawString(progressString, renderLocation.x, renderLocation.y);
        config.restore();
    }

    @Override
    protected void paintDeterminate(final Graphics g, final JComponent c) {
        Graphics2D g2 = (Graphics2D) g.create();
        try {
            g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_NORMALIZE);

            Rectangle r = new Rectangle(progressBar.getSize());
            if (c.isOpaque() && c.getParent() != null) {
                g2.setColor(c.getParent().getBackground());
                g2.fill(r);
            }

            Insets i = progressBar.getInsets();
            DarkUIUtil.applyInsets(r, i);
            int amountFull = getAmountFull(i, r.width, r.height);

            Shape fullShape, coloredShape;
            int orientation = progressBar.getOrientation();
            if (orientation == SwingConstants.HORIZONTAL) {
                int pHeight = progressBar.getPreferredSize().height;
                int yOffset = r.y + (r.height - pHeight) / 2;

                fullShape = getShapedRect(r.x, yOffset, r.width, pHeight, pHeight);
                if (progressBar.getComponentOrientation().isLeftToRight()) {
                    coloredShape = getShapedRect(r.x, yOffset, amountFull, pHeight, pHeight);
                } else {
                    coloredShape = getShapedRect(r.x + r.width - amountFull, yOffset, amountFull, pHeight, pHeight);
                }
            } else {
                int pWidth = progressBar.getPreferredSize().width;
                int xOffset = r.x + (r.width - pWidth) / 2;

                fullShape = getShapedRect(xOffset, r.y, pWidth, r.height, pWidth);
                if (progressBar.getComponentOrientation().isLeftToRight()) {
                    coloredShape = getShapedRect(xOffset, r.y, pWidth, amountFull, pWidth);
                } else {
                    coloredShape = getShapedRect(xOffset, r.y + r.height - amountFull, pWidth, amountFull, pWidth);
                }
            }
            g2.setColor(getRemainderColor());
            g2.fill(fullShape);

            if (hasFailed(progressBar)) {
                g2.setColor(failedColor);
            } else if (hasPassed(progressBar)) {
                g2.setColor(passedColor);
            } else {
                g2.setColor(getFinishedColor());
            }

            g2.fill(coloredShape);

            if (progressBar.isStringPainted()) {
                GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
                Rectangle progressRect = coloredShape.getBounds();
                if (progressBar.getOrientation() == JProgressBar.HORIZONTAL) {
                    paintString((Graphics2D) g, i.left, i.top, r.width, r.height, progressRect.x,
                            progressRect.x + progressRect.width);
                } else {
                    paintString((Graphics2D) g, i.left, i.top, r.width, r.height, progressRect.y,
                            progressRect.y + progressRect.height);
                }
                config.restore();
            }
        } finally {
            g2.dispose();
        }
    }

    protected Color getRemainderColor() {
        return trackColor;
    }

    protected Color getFinishedColor() {
        return progressColor;
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        Dimension size = super.getPreferredSize(c);
        if (!(c instanceof JProgressBar)) {
            return size;
        }
        if (!((JProgressBar) c).isStringPainted()) {
            if (((JProgressBar) c).getOrientation() == SwingConstants.HORIZONTAL) {
                size.height = getStripeWidth();
            } else {
                size.width = getStripeWidth();
            }
        }
        return size;
    }

    private int getStripeWidth() {
        return stripeWidth;
    }

    private static boolean isSimplified() {
        return UIManager.getBoolean("ProgressBar.isSimplified");
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (KEY_FAILED.equals(key) || KEY_PASSED.equals(key)) {
            progressBar.repaint();
        } else if (PropertyKey.COMPONENT_ORIENTATION.equals(key) || "stringPainted".equals(key)) {
            progressBar.revalidate();
        }
    }
}
