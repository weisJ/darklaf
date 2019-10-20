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
package javax.swing.text.DefaultHighlighterDark;

import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.TextUI;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultHighlighter;
import javax.swing.text.Highlighter;
import javax.swing.text.JTextComponent;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.html.InlineView;
import java.awt.*;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

/**
 * Note this class only sits inside this weird package because of a hack used in {@link
 * SwingUtilities2#useSelectedTextColor(Highlighter.Highlight, JTextComponent)} that makes it impossible for custom
 * highlighters to use the correct text foreground specified by {@link JTextComponent#getSelectedTextColor()}.
 *
 * @author Jannis Weis
 */
public class DarkHighlightPainter extends DefaultHighlighter.DefaultHighlightPainter {

    private static final boolean DEBUG_COLOR = false;
    private Paint paint;
    private boolean roundedEdges;
    private AlphaComposite alphaComposite;
    private float alpha;
    private int selectionStart = -1;
    private int selectionEnd = -1;
    private int repaintCount = 0;
    private int arcSize;


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
        super(null);
        setPaint(paint);
        setRoundedEdges(rounded);
        setAlpha(alpha);
        arcSize = UIManager.getInt("Highlight.arc");
    }

    public boolean getRoundedEdges() {
        return roundedEdges;
    }

    public void setRoundedEdges(final boolean rounded) {
        roundedEdges = rounded;
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

    public void setPaint(final Paint paint) {
        this.paint = paint;
    }

    public void setAlpha(final float alpha) {
        this.alpha = alpha;
        this.alpha = Math.max(alpha, 0.0f);
        this.alpha = Math.min(1.0f, alpha);
        alphaComposite = null;
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
            if (view instanceof InlineView) {
                dirtyShape = paintLayerImplInline(g2d, offs0, offs1, bounds, c, view);
            } else {
                dirtyShape = paintLayerImpl(g2d, offs0, offs1, bounds, c, view);
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

    protected Shape paintLayerImpl(final Graphics2D g2d, final int offs0, final int offs1,
                                   final Shape bounds, @NotNull final JTextComponent c,
                                   final View view) throws BadLocationException {
        Shape dirtyShape;
        Rectangle posStart = c.modelToView2D(c.getSelectionStart()).getBounds();
        Rectangle posEnd = c.modelToView2D(c.getSelectionEnd()).getBounds();
        Rectangle posOffs0 = c.modelToView2D(offs0).getBounds();
        Rectangle posOffs1 = c.getUI().modelToView2D(c, offs1, Position.Bias.Backward).getBounds();
        Paint paint = getPaint();
        if (paint == null) {
            g2d.setColor(c.getSelectionColor());
        } else {
            g2d.setPaint(paint);
        }

        if (offs0 == offs1) {
            /*
             * No selection. We should still paint something.
             */
            if (DEBUG_COLOR) g2d.setColor(Color.YELLOW);
            Shape s = view.modelToView(offs0, bounds, Position.Bias.Forward);
            Rectangle r = s.getBounds();
            dirtyShape = paintSelectionLine(g2d, r, c, posStart, posEnd);
        } else if (posOffs0.y != posStart.y && posOffs1.y != posEnd.y
                && posOffs1.y >= posOffs0.y && posOffs1.y <= posOffs0.y + posOffs0.height) {
            if (DEBUG_COLOR) g2d.setColor(Color.ORANGE);
            // Contained in view, can just use bounds.
            //Full lines of selection
            var alloc = new Rectangle(posOffs0.x, posOffs0.y, posOffs1.x + posOffs1.width - posOffs0.x,
                                      posOffs1.y + posOffs1.height - posOffs0.y);
            dirtyShape = paintSelectionLine(g2d, alloc, c, posStart, posEnd);
        } else {
            // Should only render part of View.
            //Start/End parts of selection
            Shape shape = view.modelToView(offs0, Position.Bias.Forward, offs1, Position.Bias.Backward, bounds);
            Rectangle r = shape.getBounds();
            if (posEnd.y == posStart.y) {
                dirtyShape = paintSelection(g2d, c, r);
            } else if (posOffs0.y == posStart.y) {
                dirtyShape = paintSelectionStart(g2d, r, c, posStart, posEnd);
            } else {
                dirtyShape = paintSelectionEnd(g2d, r, c, posStart, posEnd);
            }
        }
        return dirtyShape;
    }

    protected Shape paintLayerImplInline(final Graphics2D g2d, final int offs0, final int offs1,
                                         final Shape bounds, @NotNull final JTextComponent c,
                                         final View view) throws BadLocationException {
        Shape dirtyShape;
        Rectangle posStart = c.modelToView2D(c.getSelectionStart()).getBounds();
        Rectangle posEnd = c.modelToView2D(c.getSelectionEnd()).getBounds();

        Paint paint = getPaint();
        if (paint == null) {
            g2d.setColor(c.getSelectionColor());
        } else {
            g2d.setPaint(paint);
        }

        if (offs0 == offs1) {
            if (DEBUG_COLOR) g2d.setColor(Color.YELLOW);
            Shape s = view.modelToView(offs0, bounds, Position.Bias.Forward);
            Rectangle r = s.getBounds();
            g2d.drawLine(r.x, r.y, r.x, r.y + r.height);
            dirtyShape = r;
        } else if (offs0 == view.getStartOffset() && offs1 == view.getEndOffset() && posEnd.y != posStart.y) {
            // Contained in view, can just use bounds.
            //Full lines of selection
            if (DEBUG_COLOR) g2d.setColor(Color.ORANGE);
            dirtyShape = paintSelectionLine(g2d, bounds.getBounds(), c, posStart, posEnd);
        } else {
            // Should only render part of View.
            //Start/End parts of selection
            Shape shape = view.modelToView(offs0, Position.Bias.Forward, offs1, Position.Bias.Backward, bounds);
            Rectangle r = shape.getBounds();
            if ((offs0 != view.getStartOffset() && offs1 != view.getEndOffset()) || posEnd.y == posStart.y) {
                dirtyShape = paintSelection(g2d, c, r);
            } else if (offs1 == view.getEndOffset()) {
                dirtyShape = paintSelectionStart(g2d, r, c, posStart, posEnd);
            } else {
                dirtyShape = paintSelectionEnd(g2d, r, c, posStart, posEnd);
            }
        }
        return dirtyShape;
    }

    @NotNull
    @Contract("_, _, _ -> param3")
    private Shape paintSelection(final Graphics2D g2d, final JTextComponent c, final Rectangle r) {
        /*
         * Selection is contained to one line.
         */
        if (DEBUG_COLOR) g2d.setColor(Color.BLUE);
        if (isRounded(c)) {
            g2d.fillRoundRect(r.x, r.y, r.width, r.height, arcSize, arcSize);
        } else {
            g2d.fillRect(r.x, r.y, r.width, r.height);
        }
        return r;
    }

    @Contract("_, _, _, _, _ -> param2")
    @NotNull
    private Shape paintSelectionLine(@NotNull final Graphics2D g2d,
                                     @NotNull final Rectangle alloc,
                                     @NotNull final JTextComponent c,
                                     @NotNull final Rectangle posStart,
                                     @NotNull final Rectangle posEnd) throws BadLocationException {
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
                boolean isLastLine = isSecondLastLine
                        && (posEnd.x == c.getWidth() - margin.right - 1 || posEnd.x == margin.left);
                if (c.modelToView2D(c.getSelectionEnd() - 1).getBounds().y == posEnd.y) {
                    isLastLine = false;
                }
                roundRightBottom = isLastLine || (isSecondLastLine
                        && posEnd.x < (c.getWidth() - margin.right - arcSize / 2));
                roundLeftBottom = isLastLine;
                roundLeftTop = isFirstLine || (isSecondLine && posStart.x > margin.left + arcSize / 2);
                roundRightTop = isFirstLine || isSecondLine && c.getWidth() - margin.right - posStart.x == 1;
            }

            if (!(roundLeftBottom || roundRightBottom || roundRightTop || roundLeftTop)) {
                g2d.fillRect(alloc.x, alloc.y, alloc.width, alloc.height);
                return alloc;
            }
            // At least one round corner now.
            g2d.fillRoundRect(alloc.x, alloc.y, alloc.width, alloc.height, arcSize, arcSize);
            if (!roundRightBottom) {
                g2d.fillRect(alloc.x + alloc.width - arcSize, alloc.y + alloc.height - arcSize,
                             arcSize, arcSize);
            }
            if (!roundLeftBottom) {
                g2d.fillRect(alloc.x, alloc.y + alloc.height - arcSize, arcSize, arcSize);
            }
            if (!roundRightTop) {
                g2d.fillRect(alloc.x + alloc.width - arcSize, alloc.y, arcSize, arcSize);
            }
            if (!roundLeftTop) {
                g2d.fillRect(alloc.x, alloc.y, arcSize, arcSize);
            }

            boolean drawArc = isFirstLine && !roundLeftBottom;
            drawArc = drawArc && alloc.x >= margin.left + arcSize;
            if (drawArc) {
                paintStartArc(g2d, alloc);
                alloc.x -= arcSize;
                alloc.width += arcSize;
            }
        } else {
            g2d.fillRect(alloc.x, alloc.y, alloc.width, alloc.height);
        }
        return alloc;
    }

    private boolean isRounded(final JTextComponent c) {
        return roundedEdges || Boolean.TRUE.equals(c.getClientProperty("JTextComponent.roundedSelection"));
    }

    @NotNull
    @Contract("_, _, _, _, _ -> param2")
    private Shape paintSelectionStart(@NotNull final Graphics2D g2d, @NotNull final Rectangle r,
                                      @NotNull final JTextComponent c,
                                      @NotNull final Rectangle posStart, final Rectangle posEnd) {
        if (DEBUG_COLOR) g2d.setColor(Color.RED);

        r.width = c.getWidth() - posStart.x - c.getMargin().right;
        var margin = c.getMargin();
        if (isRounded(c)) {
            boolean roundLeftBottom = posEnd.x < (posStart.x + 2) && posEnd.y == posStart.y + posStart.height;
            boolean roundRightBottom = posEnd.y == posStart.y + posStart.height;
            if (!c.getComponentOrientation().isLeftToRight()) {
                roundLeftBottom = roundLeftBottom || c.getWidth() - c.getMargin().right - posEnd.x == 1;
            }

            g2d.fillRoundRect(r.x, r.y, r.width, r.height, arcSize, arcSize);
            if (!roundLeftBottom && !roundRightBottom) {
                //Optimize draw calls
                g2d.fillRect(r.x, r.y + r.height - arcSize, r.width, arcSize);
            } else if (!roundLeftBottom) {
                g2d.fillRect(r.x, r.y + r.height - arcSize, arcSize, arcSize);
            } else if (!roundRightBottom) {
                g2d.fillRect(r.x + r.width - arcSize, r.y + r.height - arcSize, arcSize, arcSize);
            }
            boolean drawCorner = !roundLeftBottom && r.x >= margin.left + arcSize;
            if (drawCorner) {
                paintStartArc(g2d, r);
                r.x -= arcSize;
                r.width += arcSize;
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
            if (posEnd.x >= (c.getWidth() - margin.right - arcSize / 2)) {
                //Fill to right edge even if corners are not rounded.
                r.width = c.getWidth() - margin.right - margin.left;
            }
        }
        if (isRounded(c)) {
            boolean roundRightTop = posEnd.x < (posStart.x + 2) && posEnd.y == posStart.y + posStart.height;
            boolean roundLeftTop = posEnd.y == posStart.y + posStart.height && posStart.x != margin.left;

            g2d.fillRoundRect(r.x, r.y, r.width, r.height, arcSize, arcSize);
            if (!roundLeftTop && !roundRightTop) {
                //Optimize draw calls
                g2d.fillRect(r.x, r.y, r.width, arcSize);
            } else if (!roundLeftTop) {
                g2d.fillRect(r.x, r.y, arcSize, arcSize);
            } else if (!roundRightTop) {
                g2d.fillRect(r.x + r.width - arcSize, r.y, arcSize, arcSize);
            }
            boolean drawCorner = !roundRightTop && r.x + r.width <= c.getWidth() - margin.right - arcSize;
            if (drawCorner) {
                paintEndArc(g2d, r);
                r.width += arcSize;
            }
        } else {
            g2d.fillRect(r.x, r.y, r.width, r.height);
        }
        return r;
    }

    private void paintStartArc(@NotNull final Graphics2D g2d, @NotNull final Rectangle r) {
        if (DEBUG_COLOR) g2d.setColor(Color.PINK);

        Area arc = new Area(new Rectangle2D.Double(
                r.x - arcSize + 0.25, r.y + r.height - arcSize + 0.25, arcSize, arcSize));
        arc.subtract(new Area(new Arc2D.Double(
                r.x - 2 * arcSize + 0.25,
                r.y + r.height - 2 * arcSize + 0.25, 2 * arcSize, 2 * arcSize,
                0, -90, Arc2D.Double.PIE)));
        g2d.fill(arc);
        r.x -= arcSize;
        r.width += arcSize;
    }

    private void paintEndArc(@NotNull final Graphics2D g2d, @NotNull final Rectangle r) {
        if (DEBUG_COLOR) g2d.setColor(Color.PINK);

        Area arc = new Area(new Rectangle2D.Double(
                r.x + r.width - 0.25, r.y - 0.25, arcSize, arcSize));
        arc.subtract(new Area(new Arc2D.Double(
                r.x + r.width - 0.25,
                r.y - 0.25, 2 * arcSize, 2 * arcSize, 90, 90, Arc2D.Double.PIE)));
        g2d.fill(arc);
    }
}
