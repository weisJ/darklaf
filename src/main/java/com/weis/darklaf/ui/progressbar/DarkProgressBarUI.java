package com.weis.darklaf.ui.progressbar;

import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicProgressBarUI;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkProgressBarUI extends BasicProgressBarUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkProgressBarUI();
    }

    private Color trackColor;
    private Color progressColor;
    private Color indeterminateStartColor;
    private Color indeterminateEndColor;

    private Color failedColor;
    private Color failedEndColor;
    private Color passedColor;
    private Color passedEndColor;

    private static final int CYCLE_TIME_DEFAULT = 800;
    private static final int REPAINT_INTERVAL_DEFAULT = 50;

    private static final int CYCLE_TIME_SIMPLIFIED = 1000;
    private static final int REPAINT_INTERVAL_SIMPLIFIED = 500;

    private static final int DEFAULT_WIDTH = 4;

    @Override
    protected void installDefaults() {
        super.installDefaults();
        UIManager.put("ProgressBar.repaintInterval", isSimplified() ? REPAINT_INTERVAL_SIMPLIFIED : REPAINT_INTERVAL_DEFAULT);
        UIManager.put("ProgressBar.cycleTime", isSimplified() ? CYCLE_TIME_SIMPLIFIED : CYCLE_TIME_DEFAULT);
        trackColor = DarkColors.get().getProgressBarTrackBackground();
        progressColor = DarkColors.get().getProgressBarProgressColor();
        indeterminateStartColor = DarkColors.get().getProgressBarIndeterminateStartColor();
        indeterminateEndColor = DarkColors.get().getProgressBarIndeterminateEndColor();

        failedColor = DarkColors.get().getProgressBarFailedColor();
        failedEndColor = DarkColors.get().getProgressBarFailedEndColor();
        passedColor = DarkColors.get().getProgressBarPassedColor();
        passedEndColor = DarkColors.get().getProgressBarPassedEndColor();
    }

    @Override
    protected void paintIndeterminate(@NotNull final Graphics g, @NotNull final JComponent c) {

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
            DarkUIUtil.removeInsets(r, i);
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
                                                  r.x + getFrameCount() * step + getAnimationIndex() * step * 2, yOffset,
                                                  endColor, true));
                } else {
                    shape = getShapedRect(xOffset, r.y, pWidth, r.height, pWidth);
                    xOffset = r.x + pWidth / 2;
                    g2.setPaint(new GradientPaint(xOffset, r.y + getAnimationIndex() * step * 2, startColor,
                                                  xOffset, r.y + getFrameCount() * step + getAnimationIndex() * step * 2,
                                                  endColor, true));
                }
                g2.fill(shape);
            }

            // Paint text
            if (progressBar.isStringPainted()) {
                if (progressBar.getOrientation() == SwingConstants.HORIZONTAL) {
                    paintString((Graphics2D) g, i.left, i.top, r.width, r.height,
                                boxRect.x, boxRect.width);
                } else {
                    paintString((Graphics2D) g, i.left, i.top, r.width, r.height,
                                boxRect.y, boxRect.height);
                }
            }
        } finally {
            g2.dispose();
        }
    }

    protected Color getStartColor() {
        return indeterminateStartColor;
    }

    protected Color getEndColor() {
        return indeterminateEndColor;
    }

    private void paintString(@NotNull final Graphics2D g, final int x, final int y,
                             final int w, final int h, final int fillStart, final int amountFull) {
        String progressString = progressBar.getString();
        g.setFont(progressBar.getFont());
        Point renderLocation = getStringPlacement(g, progressString, x, y, w, h);

        var config = GraphicsUtil.setupAAPainting(g);
        if (progressBar.getOrientation() == SwingConstants.HORIZONTAL) {
            g.setColor(getSelectionBackground());
            g.drawString(progressString, renderLocation.x, renderLocation.y);

            g.setColor(getSelectionForeground());
            g.clipRect(fillStart, y, amountFull, h);
            g.drawString(progressString, renderLocation.x, renderLocation.y);

        } else { // VERTICAL
            g.setColor(getSelectionBackground());
            AffineTransform rotate = AffineTransform.getRotateInstance(Math.PI / 2);
            g.setFont(progressBar.getFont().deriveFont(rotate));
            renderLocation = getStringPlacement(g, progressString, x, y, w, h);
            g.drawString(progressString, renderLocation.x, renderLocation.y);

            g.setColor(getSelectionForeground());
            g.clipRect(x, fillStart, w, amountFull);

            g.drawString(progressString, renderLocation.x, renderLocation.y);
        }
        config.restore();
    }

    @Override
    protected void paintDeterminate(@NotNull final Graphics g, final JComponent c) {
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
            DarkUIUtil.removeInsets(r, i);
            int amountFull = getAmountFull(i, r.width, r.height);

            Shape fullShape, coloredShape;
            int orientation = progressBar.getOrientation();
            if (orientation == SwingConstants.HORIZONTAL) {
                int pHeight = progressBar.getPreferredSize().height;
                int yOffset = r.y + (r.height - pHeight) / 2;

                fullShape = getShapedRect(r.x, yOffset, r.width, pHeight, pHeight);
                coloredShape = getShapedRect(r.x, yOffset, amountFull, pHeight, pHeight);
            } else {
                int pWidth = progressBar.getPreferredSize().width;
                int xOffset = r.x + (r.width - pWidth) / 2;

                fullShape = getShapedRect(xOffset, r.y, pWidth, r.height, pWidth);
                coloredShape = getShapedRect(xOffset, r.y, pWidth, amountFull, pWidth);
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
                var config = GraphicsUtil.setupAAPainting(g);
                paintString(g, i.left, i.top, r.width, r.height, amountFull, i);
                config.restore();
            }
        } finally {
            g2.dispose();
        }
    }

    protected static boolean hasFailed(@NotNull final JComponent c) {
        return Boolean.TRUE.equals(c.getClientProperty("JProgressBar.failed"));
    }

    protected static boolean hasPassed(@NotNull final JComponent c) {
        return Boolean.TRUE.equals(c.getClientProperty("JProgressBar.passed"));
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
        Object ho = progressBar.getClientProperty("ProgressBar.stripeWidth");
        if (ho != null) {
            try {
                return Integer.parseInt(ho.toString());
            } catch (NumberFormatException nfe) {
                return DEFAULT_WIDTH;
            }
        } else {
            return DEFAULT_WIDTH;
        }
    }

    @Override
    protected int getBoxLength(final int availableLength, final int otherDimension) {
        return availableLength;
    }

    @NotNull
    @Contract("_, _, _, _, _ -> new")
    private Shape getShapedRect(final float x, final float y, final float w, final float h, final float ar) {
        return new RoundRectangle2D.Float(x, y, w, h, ar, ar);
    }

    @Contract(pure = true)
    private static boolean isSimplified() {
        return UIManager.getBoolean("ProgressBar.isSimplified");
    }
}