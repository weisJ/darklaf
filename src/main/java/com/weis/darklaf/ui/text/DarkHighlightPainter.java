package com.weis.darklaf.ui.text;

import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.JTextComponent;
import javax.swing.text.LayeredHighlighter;
import javax.swing.text.Position;
import javax.swing.text.View;
import java.awt.*;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

public class DarkHighlightPainter extends LayeredHighlighter.LayerPainter {

    private static final boolean DEBUG_COLOR = false;

    private Paint paint;
    private boolean roundedEdges;
    private AlphaComposite alphaComposite;
    private float alpha;

    private int selectionStart = -1;
    private int selectionEnd = -1;
    private int repaintCount = 0;

    private static final int ARC_WIDTH = 5;
    private static final int ARC_HEIGHT = 5;


    public DarkHighlightPainter() {
        this(null);
    }


    public DarkHighlightPainter(final Paint paint) {
        this(paint, false);
    }


    public DarkHighlightPainter(final Paint paint, final boolean rounded) {
        this(paint, rounded, 1.0f);
    }


    public DarkHighlightPainter(final Paint paint, final boolean rounded, final float alpha) {
        setPaint(paint);
        setRoundedEdges(rounded);
        setAlpha(alpha);
    }


    public float getAlpha() {
        return alpha;
    }


    private AlphaComposite getAlphaComposite() {
        if (alphaComposite == null) {
            alphaComposite = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, alpha);
        }
        return alphaComposite;
    }


    public Paint getPaint() {
        return paint;
    }

    public boolean getRoundedEdges() {
        return roundedEdges;
    }

    private boolean isRounded(final JTextComponent c) {
        return roundedEdges || Boolean.TRUE.equals(c.getClientProperty("JTextComponent.roundedSelection"));
    }


    /**
     * Paints a highlight.
     *
     * @param g      the graphics context
     * @param offs0  the starting model offset &gt;= 0
     * @param offs1  the ending model offset &gt;= offs1
     * @param bounds the bounding box for the highlight
     * @param c      the editor
     */
    @Override
    public void paint(final Graphics g, final int offs0, final int offs1, @NotNull final Shape bounds,
                      final JTextComponent c) {
        Rectangle alloc = bounds.getBounds();
        Graphics2D g2d = (Graphics2D) g;
        GraphicsContext context = new GraphicsContext(g2d);

        if (getAlpha() < 1.0f) {
            g2d.setComposite(getAlphaComposite());
        }

        try {
            TextUI mapper = c.getUI();
            Rectangle p0 = mapper.modelToView2D(c, offs0, Position.Bias.Forward).getBounds();
            Rectangle p1 = mapper.modelToView2D(c, offs1, Position.Bias.Forward).getBounds();
            Paint paint = getPaint();
            if (paint == null) {
                g2d.setColor(c.getSelectionColor());
            } else {
                g2d.setPaint(paint);
            }

            if (p0.y == p1.y) {
                // Entire highlight is on one line.
                p1.width = 0;
                Rectangle r = p0.union(p1);
                g2d.fillRect(r.x, r.y, r.width, r.height);
            } else {
                // Highlight spans lines.
                int p0ToMarginWidth = alloc.x + alloc.width - p0.x;
                g2d.fillRect(p0.x, p0.y, p0ToMarginWidth, p0.height);
                if ((p0.y + p0.height) != p1.y) {
                    g2d.fillRect(alloc.x, p0.y + p0.height, alloc.width,
                                 p1.y - (p0.y + p0.height));
                }
                g2d.fillRect(alloc.x, p1.y, (p1.x - alloc.x), p1.height);
            }

        } catch (BadLocationException ignored) {
        } finally {
            context.restore();
        }
    }


    @Override
    public Shape paintLayer(final Graphics g, final int offs0, final int offs1,
                            final Shape bounds, final JTextComponent c, final View view) {
        Shape dirtyShape = null;
        Graphics2D g2d = (Graphics2D) g;
        GraphicsContext context = GraphicsUtil.setupAAPainting(g2d);
        if (getAlpha() < 1.0f) {
            g2d.setComposite(getAlphaComposite());
        }
        try {
            Rectangle posStart = c.modelToView2D(c.getSelectionStart()).getBounds();
            Rectangle posEnd = c.modelToView2D(c.getSelectionEnd()).getBounds();

            Paint paint = getPaint();
            if (paint == null) {
                g2d.setColor(c.getSelectionColor());
            } else {
                g2d.setPaint(paint);
            }

            if (offs0 == offs1) {
                Shape s = view.modelToView(offs0, bounds, Position.Bias.Forward);
                Rectangle r = s.getBounds();
                g.drawLine(r.x, r.y, r.x, r.y + r.height);
                dirtyShape = r;
            } else if (offs0 == view.getStartOffset() && offs1 == view.getEndOffset() && posEnd.y != posStart.y) {
                // Contained in view, can just use bounds.
                //Full lines of selection
                dirtyShape = paintSelectionLine(g2d, bounds, c, posStart, posEnd);
            } else {
                // Should only render part of View.
                //Start/End parts of selection
                Shape shape = view.modelToView(offs0, Position.Bias.Forward, offs1, Position.Bias.Backward, bounds);
                Rectangle r = shape.getBounds();
                if ((offs0 != view.getStartOffset() && offs1 != view.getEndOffset()) || posEnd.y == posStart.y) {
                    if (isRounded(c)) {
                        g2d.fillRoundRect(r.x, r.y, r.width, r.height, ARC_WIDTH, ARC_HEIGHT);
                    } else {
                        g2d.fillRect(r.x, r.y, r.width, r.height);
                    }
                    dirtyShape = r;
                } else if (offs1 == view.getEndOffset()) {
                    dirtyShape = paintSelectionStart(g2d, r, c, posStart, posEnd);
                } else {
                    dirtyShape = paintSelectionEnd(g2d, r, c, posStart, posEnd);
                }
            }
        } catch (BadLocationException ignored) {
        } finally {
            context.restore();
        }
        /*
         * To make sure the the right part of the highlight is actually painted we manually repaint
         * after the selection has changed.
         */
        if (dirtyShape != null && (selectionEnd != c.getSelectionEnd()
                                   || selectionStart != c.getSelectionStart()
                                   || repaintCount < 2)) {
            selectionStart = c.getSelectionStart();
            selectionEnd = c.getSelectionEnd();
            c.repaint(dirtyShape.getBounds());
            /*
             * Sometimes one repaint process isn't enough to fully update the selection painting if in
             * Right to Left mode. This forces a second repaint.
             */
            if (!c.getComponentOrientation().isLeftToRight()) {
                repaintCount = repaintCount >= 1 ? 0 : 1;
            } else {
                repaintCount = 2;
            }
        }
        return dirtyShape;
    }

    @NotNull
    @Contract("_, _, _, _, _ -> param2")
    private Shape paintSelectionStart(@NotNull final Graphics2D g2d, @NotNull final Rectangle r,
                                      @NotNull final JTextComponent c,
                                      @NotNull final Rectangle posStart, final Rectangle posEnd) {
        if (DEBUG_COLOR) g2d.setColor(Color.RED);

        r.width = c.getWidth() - posStart.x - c.getMargin().right;
        if (isRounded(c)) {
            boolean roundLeftBottom = posEnd.x < (posStart.x + 2) && posEnd.y == posStart.y + posStart.height;
            boolean roundRightBottom = posEnd.y == posStart.y + posStart.height;
            if (!c.getComponentOrientation().isLeftToRight()) {
                roundLeftBottom = roundLeftBottom || c.getWidth() - c.getMargin().right - posEnd.x == 1;
            }

            g2d.fillRoundRect(r.x, r.y, r.width, r.height, ARC_WIDTH, ARC_HEIGHT);
            if (!roundLeftBottom && !roundRightBottom) {
                //Optimize draw calls
                g2d.fillRect(r.x, r.y + r.height - ARC_HEIGHT, r.width, ARC_HEIGHT);
            } else if (!roundLeftBottom) {
                g2d.fillRect(r.x, r.y + r.height - ARC_HEIGHT, ARC_WIDTH, ARC_HEIGHT);
            } else if (!roundRightBottom) {
                g2d.fillRect(r.x + r.width - ARC_WIDTH, r.y + r.height - ARC_HEIGHT, ARC_WIDTH, ARC_HEIGHT);
            }
            if (!roundLeftBottom) {
                paintStartArc(g2d, r);
                r.x -= ARC_WIDTH;
                r.width += ARC_WIDTH;
            }
        } else {
            g2d.fillRect(r.x, r.y, r.width, r.height);
        }
        return r;
    }

    @Contract("_, _, _, _, _ -> param2")
    @NotNull
    private Shape paintSelectionEnd(@NotNull final Graphics2D g2d, @NotNull final Rectangle r,
                                    @NotNull final JTextComponent c, final Rectangle posStart,
                                    final Rectangle posEnd) {
        if (DEBUG_COLOR) g2d.setColor(Color.GREEN);

        var margin = c.getMargin();
        r.width += r.x - margin.left;
        r.x = margin.left;
        if (!c.getComponentOrientation().isLeftToRight() && r.y == posStart.y) {
            r.width -= posStart.x - margin.left;
            r.x = posStart.x;
        }
        if (!c.getComponentOrientation().isLeftToRight()) {
            if (posEnd.x >= (c.getWidth() - margin.right - ARC_WIDTH / 2)) {
                //Fill to right edge even if corners are not rounded.
                r.width = c.getWidth() - margin.right - margin.left;
            }
        }
        if (isRounded(c)) {
            boolean roundRightTop = posEnd.x < (posStart.x + 2) && posEnd.y == posStart.y + posStart.height;
            boolean roundLeftTop = posEnd.y == posStart.y + posStart.height && posStart.x != margin.left;

            g2d.fillRoundRect(r.x, r.y, r.width, r.height, ARC_WIDTH, ARC_HEIGHT);
            if (!roundLeftTop && !roundRightTop) {
                //Optimize draw calls
                g2d.fillRect(r.x, r.y, r.width, ARC_HEIGHT);
            } else if (!roundLeftTop) {
                g2d.fillRect(r.x, r.y, ARC_WIDTH, ARC_HEIGHT);
            } else if (!roundRightTop) {
                g2d.fillRect(r.x + r.width - ARC_WIDTH, r.y, ARC_WIDTH, ARC_HEIGHT);
            }
            if (!roundRightTop) {
                paintEndArc(g2d, r);
                r.width += ARC_WIDTH;
            }
        } else {
            g2d.fillRect(r.x, r.y, r.width, r.height);
        }
        return r;
    }

    private void paintStartArc(@NotNull final Graphics2D g2d, @NotNull final Rectangle r) {
        if (DEBUG_COLOR) g2d.setColor(Color.PINK);

        Area arc = new Area(new Rectangle2D.Double(
                r.x - ARC_WIDTH + 0.25, r.y + r.height - ARC_HEIGHT + 0.25, ARC_WIDTH, ARC_HEIGHT));
        arc.subtract(new Area(new Arc2D.Double(
                r.x - 2 * ARC_WIDTH + 0.25,
                r.y + r.height - 2 * ARC_HEIGHT + 0.25, 2 * ARC_WIDTH, 2 * ARC_HEIGHT,
                0, -90, Arc2D.Double.PIE)));
        g2d.fill(arc);
        r.x -= ARC_WIDTH;
        r.width += ARC_WIDTH;
    }

    private void paintEndArc(@NotNull final Graphics2D g2d, @NotNull final Rectangle r) {
        if (DEBUG_COLOR) g2d.setColor(Color.PINK);

        Area arc = new Area(new Rectangle2D.Double(
                r.x + r.width - 0.25, r.y - 0.25, ARC_WIDTH, ARC_HEIGHT));
        arc.subtract(new Area(new Arc2D.Double(
                r.x + r.width - 0.25,
                r.y - 0.25, 2 * ARC_WIDTH, 2 * ARC_HEIGHT, 90, 90, Arc2D.Double.PIE)));
        g2d.fill(arc);
    }

    @NotNull
    private Shape paintSelectionLine(@NotNull final Graphics2D g2d,
                                     @NotNull final Shape bounds,
                                     @NotNull final JTextComponent c,
                                     @NotNull final Rectangle posStart,
                                     final Rectangle posEnd) throws BadLocationException {
        if (DEBUG_COLOR) g2d.setColor(Color.YELLOW);

        Rectangle alloc = bounds.getBounds();
        var margin = c.getMargin();
        boolean leftToRight = c.getComponentOrientation().isLeftToRight();
        boolean isFirstLine = alloc.y == posStart.y;
        boolean isSecondLine = posStart.y + posStart.height == alloc.y;
        boolean isSecondLastLine = alloc.y + alloc.height == posEnd.y;

        if (alloc.width == 0 && isFirstLine && !leftToRight) {
            return alloc;
        }
        if (alloc.y == posStart.y && ((leftToRight || posStart.x - c.getWidth() - margin.right - posStart.x == 1))) {
            alloc.width = c.getWidth() - margin.right - alloc.x;
        } else {
            alloc.width = c.getWidth() - margin.left - margin.right;
            alloc.x = margin.left;
        }
        if (!leftToRight && alloc.y == posStart.y) {
            alloc.width -= posStart.x - margin.left;
            alloc.x = posStart.x;
        }
        if (isRounded(c)) {
            boolean roundRightBottom;
            boolean roundLeftTop;
            boolean roundLeftBottom;
            boolean roundRightTop;
            if (leftToRight) {
                boolean isLastLine = isSecondLastLine && posEnd.x == margin.left;
                roundRightBottom = isSecondLastLine;
                roundLeftTop = isFirstLine || isSecondLine && posStart.x != margin.left;
                roundLeftBottom = isLastLine;
                roundRightTop = isFirstLine;
            } else {
                boolean isLastLine = isSecondLastLine && posEnd.x == c.getWidth() - margin.right - 1;
                if (c.modelToView2D(c.getSelectionEnd() - 1).getBounds().y == posEnd.y) {
                    isLastLine = false;
                }
                roundRightBottom = isLastLine || (isSecondLastLine
                                                  && posEnd.x < (c.getWidth() - margin.right - ARC_WIDTH / 2));
                roundLeftBottom = isLastLine;
                roundLeftTop = isFirstLine || (isSecondLine && posStart.x > margin.left + ARC_WIDTH / 2);
                roundRightTop = isFirstLine || isSecondLine && c.getWidth() - margin.right - posStart.x == 1;
            }

            if (!(roundLeftBottom || roundRightBottom || roundRightTop || roundLeftTop)) {
                g2d.fillRect(alloc.x, alloc.y, alloc.width, alloc.height);
                return alloc;
            }
            // At least one round corner now.
            g2d.fillRoundRect(alloc.x, alloc.y, alloc.width, alloc.height, ARC_WIDTH, ARC_HEIGHT);
            if (!roundRightBottom) {
                g2d.fillRect(alloc.x + alloc.width - ARC_WIDTH, alloc.y + alloc.height - ARC_HEIGHT,
                             ARC_WIDTH, ARC_HEIGHT);
            }
            if (!roundLeftBottom) {
                g2d.fillRect(alloc.x, alloc.y + alloc.height - ARC_HEIGHT, ARC_WIDTH, ARC_HEIGHT);
            }
            if (!roundRightTop) {
                g2d.fillRect(alloc.x + alloc.width - ARC_WIDTH, alloc.y, ARC_WIDTH, ARC_HEIGHT);
            }
            if (!roundLeftTop) {
                g2d.fillRect(alloc.x, alloc.y, ARC_WIDTH, ARC_HEIGHT);
            }

            boolean drawArc = isFirstLine && !roundLeftBottom;
            if (leftToRight) {
                drawArc = drawArc && alloc.x != margin.left;
            } else {
                drawArc = drawArc && c.getWidth() - margin.right - alloc.x != 1;
            }
            if (drawArc) {
                paintStartArc(g2d, alloc);
                alloc.x -= ARC_WIDTH;
                alloc.width += ARC_WIDTH;
            }
        } else {
            g2d.fillRect(alloc.x, alloc.y, alloc.width, alloc.height);
        }
        return alloc;
    }

    public void setAlpha(final float alpha) {
        this.alpha = alpha;
        this.alpha = Math.max(alpha, 0.0f);
        this.alpha = Math.min(1.0f, alpha);
        alphaComposite = null;
    }


    public void setPaint(final Paint paint) {
        this.paint = paint;
    }

    public void setRoundedEdges(final boolean rounded) {
        roundedEdges = rounded;
    }
}
