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

import com.github.weisj.darklaf.color.ColorWrapper;
import com.github.weisj.darklaf.ui.text.StyleConstantsEx;
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

    private static final boolean DEBUG_COLOR = true;
    private Paint paint;
    private Color color;
    private ColorWrapper wrapper;
    private boolean roundedEdges;
    private AlphaComposite alphaComposite;
    private float alpha;
    private int selectionStart = -1;
    private int selectionEnd = -1;
    private int repaintCount = 0;
    private int arcSize;
    private boolean suppressRounded = false;


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
        wrapper = new ColorWrapper(color) {
            @Override
            public boolean equals(final Object obj) {
                return obj != null;
            }
        };
    }

    public boolean getRoundedEdges() {
        return roundedEdges;
    }

    public void setRoundedEdges(final boolean rounded) {
        roundedEdges = rounded;
    }


    @Override
    public Color getColor() {
        return wrapper;
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
        color = c.getSelectedTextColor();
        wrapper.setColor(color);

        if (getAlpha() < 1.0f) {
            g2d.setComposite(getAlphaComposite());
        }

        try {
            TextUI mapper = c.getUI();
            Rectangle p0 = mapper.modelToView(c, offs0, Position.Bias.Forward);
            Rectangle p1 = mapper.modelToView(c, offs1, Position.Bias.Forward);
            setupColor(g2d, c);

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
                            final Shape bounds, final JTextComponent c, @NotNull final View view) {
        color = (Color) view.getAttributes().getAttribute(StyleConstantsEx.SelectedForeground);
        if (color == null) {
            color = c.getSelectedTextColor();
        }
        wrapper.setColor(color);
        Shape dirtyShape = null;
        Graphics2D g2d = (Graphics2D) g;
        GraphicsContext context = GraphicsUtil.setupAAPainting(g2d);
        if (getAlpha() < 1.0f) {
            g2d.setComposite(getAlphaComposite());
        }
        try {
            dirtyShape = paintLayerImpl(g2d, offs0, offs1, c);
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

    private void setupColor(final Graphics2D g2d, @NotNull final JTextComponent c) {
        Paint paint = getPaint();
        if (paint == null) {
            g2d.setColor(c.getSelectionColor());
        } else {
            g2d.setPaint(paint);
        }
    }

    protected Shape paintLayerImpl(final Graphics2D g2d, final int offs0, final int offs1,
                                   @NotNull final JTextComponent c) throws BadLocationException {
        Shape dirtyShape;
        Rectangle posStart = c.modelToView(c.getSelectionStart());
        Rectangle posEnd = c.modelToView(c.getSelectionEnd());
        Rectangle posEndPrev = c.modelToView(Math.max(0, c.getSelectionEnd() - 1));
        Rectangle posOffs0 = c.modelToView(offs0);
        Rectangle posOffs0Prev = c.modelToView(Math.max(0, offs0 - 1)).getBounds();
        Rectangle posOffs1 = c.getUI().modelToView(c, offs1, Position.Bias.Backward);
        Rectangle posOffs1Forward = c.modelToView(offs1);
        Rectangle posOffs1Next = c.modelToView(Math.min(c.getDocument().getLength(), offs1 - 1));
        boolean selectionStart = c.getSelectionStart() >= offs0;
        boolean selectionEnd = c.getSelectionEnd() <= offs1;

        Insets margin = c.getMargin();
        boolean lastLineNotPainted = posOffs1Next.y == posEnd.y && posOffs1.y == posOffs1Forward.y;
        boolean isToEndOfLine = posOffs1.y < posEnd.y && !lastLineNotPainted;
        boolean isToStartOfLine = !selectionEnd && posOffs0.y > posStart.y && (posOffs0.y != posOffs0Prev.y);

        Rectangle alloc;
        if (offs0 == offs1 && posEnd.y != posStart.y) {
            alloc = new Rectangle(margin.left, posOffs0.y,
                                  c.getWidth() - margin.left - margin.right, posOffs0.height);
        } else {
            alloc = new Rectangle(posOffs0.x, posOffs0.y, posOffs1.x + posOffs1.width - posOffs0.x,
                                  posOffs1.y + posOffs1.height - posOffs0.y);
        }

        boolean isFirstLine = alloc.y == posStart.y;
        boolean isSecondLine = posStart.y + posStart.height == alloc.y;
        boolean isSecondLastLine = alloc.y + alloc.height == posEnd.y;
        boolean isLastLine = alloc.y == posEnd.y || isSecondLastLine && posEnd.y != posEndPrev.y;
        boolean endBeforeStart = posEnd.x < (posStart.x + arcSize / 2.0)
                && (posEnd.y == posStart.y + posStart.height
                || (posEnd.y <= posStart.y + 2 * posStart.height && posEnd.x == margin.left));

        int originalWidth = alloc.width;
        int originalX = alloc.x;
        alloc.width = Math.max(2 * arcSize, alloc.width);
        alloc.x = Math.max(margin.left, Math.min(c.getWidth() - margin.right - alloc.width, alloc.x));

        setupColor(g2d, c);

        if (offs0 == offs1 && posEnd.y != posStart.y) {
            if (DEBUG_COLOR) g2d.setColor(Color.YELLOW.darker());
            isToEndOfLine = false;
            isToStartOfLine = false;
            dirtyShape = paintMiddleSelection(g2d, alloc, c,
                                              false, false);
        } else if (!selectionStart && !selectionEnd) {
            if (DEBUG_COLOR) g2d.setColor(Color.ORANGE);
            dirtyShape = paintMiddleSelection(g2d, alloc, c,
                                              isToEndOfLine, isToStartOfLine);
        } else {
            // Should only render part of View.
            //Start/End parts of selection
            if (posEnd.y == posStart.y) {
                Rectangle rect = alloc;
                if (originalWidth < 2 * arcSize && isRounded(c)) {
                    suppressRounded = true;
                    rect = new Rectangle(originalX, alloc.y, originalWidth, alloc.height);
                }
                paintSelection(g2d, c, rect, selectionStart, selectionEnd);
                dirtyShape = alloc;
                suppressRounded = false;
            } else if (selectionStart) {
                dirtyShape = paintSelectionStart(g2d, alloc, c, posStart, posOffs0, endBeforeStart, isSecondLastLine,
                                                 isToEndOfLine);
            } else {
                dirtyShape = paintSelectionEnd(g2d, alloc, c, posStart,
                                               isFirstLine, isSecondLine, isToStartOfLine, isToEndOfLine,
                                               endBeforeStart);
            }
        }
        dirtyShape = paintExtension(g2d, c,
                                    isToEndOfLine, isToStartOfLine,
                                    isFirstLine, isLastLine,
                                    isSecondLine, isSecondLastLine,
                                    selectionStart, selectionEnd,
                                    posStart, posEnd, dirtyShape.getBounds());
        return dirtyShape;
    }

    @NotNull
    private Shape paintMiddleSelection(final Graphics2D g, final Rectangle r, final JTextComponent c,
                                       final boolean toEndOfLine, final boolean toStartOfLine) {
        if (toStartOfLine) {
            r.width -= arcSize;
            r.x += arcSize;
        }
        if (toEndOfLine) {
            r.width -= arcSize;
        }
        g.fillRect(r.x, r.y, r.width, r.height);
        return r;
    }

    /*
     * Selection is contained to one line.
     */
    @Contract("_, _, _, _, _ -> param3")
    @NotNull
    private Shape paintSelection(final Graphics2D g2d, final JTextComponent c, final Rectangle r,
                                 final boolean selectionStart, final boolean selectionEnd) {
        if (DEBUG_COLOR) g2d.setColor(Color.BLUE);
        if (isRounded(c)) {
            paintRoundedLeftRight(g2d, selectionStart, selectionEnd, r);
        } else {
            g2d.fillRect(r.x, r.y, r.width, r.height);
        }
        return r;
    }

    @Contract("_, _, _, _, _, _, _, _ -> param2")
    @NotNull
    private Shape paintSelectionStart(@NotNull final Graphics2D g2d, @NotNull final Rectangle r,
                                      @NotNull final JTextComponent c,
                                      @NotNull final Rectangle posStart,
                                      final Rectangle posOffs0,
                                      final boolean endBeforeStart, final boolean isSecondLastLine,
                                      final boolean extendToEnd) {
        if (DEBUG_COLOR) g2d.setColor(Color.RED);
        Insets margin = c.getMargin();
        boolean rounded = isRounded(c);
        if (rounded && extendToEnd) r.width -= arcSize;
        if (rounded) {
            boolean roundLeftBottom = endBeforeStart && isSecondLastLine;
            if (r.width < 2 * arcSize) r.width = 2 * arcSize;
            paintRoundRect(g2d, r, arcSize, true, false, roundLeftBottom, false);
            boolean drawCorner = posOffs0.equals(posStart) && !roundLeftBottom && r.x >= margin.left + arcSize;
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

    private boolean isRounded(final JTextComponent c) {
        return !suppressRounded
                && (roundedEdges || Boolean.TRUE.equals(c.getClientProperty("JTextComponent.roundedSelection")));
    }

    @Contract("_, _, _, _, _, _, _, _, _ -> param2")
    @NotNull
    private Shape paintSelectionEnd(@NotNull final Graphics2D g2d, @NotNull final Rectangle r,
                                    @NotNull final JTextComponent c, final Rectangle posStart,
                                    final boolean isFirstLine, final boolean isSecondLine,
                                    final boolean extendToStart, final boolean extendToEnd,
                                    final boolean endBeforeStart) {
        if (DEBUG_COLOR) g2d.setColor(Color.GREEN);
        boolean rounded = isRounded(c);
        Insets margin = c.getMargin();
        if (r.x + r.width >= c.getWidth() - margin.right - arcSize / 2.0) {
            int end = c.getWidth() - margin.right;
            r.width = end - r.x;
            if (rounded && extendToEnd) r.width -= arcSize;
        }
        if (rounded) {
            boolean roundRightTop = endBeforeStart && !extendToEnd;
            boolean roundLeftBottom = !isFirstLine && !extendToStart;
            boolean roundLeftTop = isSecondLine && !extendToStart && posStart.x >= r.x + arcSize;
            paintRoundRect(g2d, r, arcSize, roundLeftTop, roundRightTop, roundLeftBottom, !extendToEnd);
            boolean drawCorner = !extendToEnd && !roundRightTop && r.x + r.width <= c.getWidth() - margin.right - arcSize;
            if (drawCorner) {
                paintEndArc(g2d, r);
                r.width += arcSize;
            }
        } else {
            g2d.fillRect(r.x, r.y, r.width, r.height);
        }
        return r;
    }

    private Shape paintExtension(final Graphics2D g2d, @NotNull final JTextComponent c,
                                 final boolean isToEndOfLine, final boolean isToStartOfLine,
                                 final boolean isFirstLine, final boolean isLastLine,
                                 final boolean isSecondLine, final boolean isSecondLastLine,
                                 final boolean selectionStart, final boolean selectionEnd,
                                 final Rectangle posStart, final Rectangle posEnd,
                                 final Rectangle r) {
        Insets margin = c.getMargin();
        boolean rounded = isRounded(c);
        if (isToEndOfLine) {
            if (DEBUG_COLOR) g2d.setColor(Color.CYAN);
            int start = r.x + r.width;
            int w = c.getWidth() - start - margin.right;
            w = Math.max(2 * arcSize, w);
            start = Math.min(start, c.getWidth() - margin.right - w);
            if (rounded) {
                boolean roundTop = isFirstLine || selectionStart;
                boolean roundBottom = isLastLine || (isSecondLastLine && posEnd.x + posEnd.width <= start + w - arcSize);
                boolean roundLeftTop = isFirstLine && start == margin.left;
                paintRoundRect(g2d, new Rectangle(start, r.y, w, r.height), arcSize,
                               roundLeftTop, roundTop, false, roundBottom);
            } else {
                g2d.fillRect(start, r.y, w, r.height);
            }
            r.x = Math.min(r.x, start);
            r.width += w;
        }
        if (isToStartOfLine) {
            if (DEBUG_COLOR) g2d.setColor(Color.CYAN.darker());
            int start = margin.left;
            int end = r.x;
            int w = end - start;
            w = Math.max(2 * arcSize, w);
            end = Math.max(end, start + w);
            if (rounded) {
                boolean roundTop = isFirstLine || (isSecondLine && posStart.x >= start + arcSize);
                boolean roundBottom = isLastLine || selectionEnd;
                boolean roundRightBottom = isLastLine && end == c.getWidth() - margin.right;
                paintRoundRect(g2d, new Rectangle(start, r.y, w, r.height), arcSize,
                               roundTop, false, roundBottom, roundRightBottom);
            } else {
                g2d.fillRect(r.x, r.y, end - r.x, r.height);
            }
            r.width += w;
            r.x = start;
        }
        return r;
    }

    private void paintRoundRect(@NotNull final Graphics g, @NotNull final Rectangle r, final int arcSize,
                                final boolean leftTop, final boolean rightTop,
                                final boolean leftBottom, final boolean rightBottom) {
        int aw = Math.min(arcSize, r.width);
        int ah = Math.min(arcSize, r.height);
        g.fillRoundRect(r.x, r.y, r.width, r.height, aw, ah);
        if (!leftTop) g.fillRect(r.x, r.y, aw, ah);
        if (!leftBottom) g.fillRect(r.x, r.y + r.height - ah, aw, ah);
        if (!rightTop) g.fillRect(r.x + r.width - aw, r.y, aw, ah);
        if (!rightBottom) g.fillRect(r.x + r.width - aw, r.y + r.height - ah, aw, ah);
    }

    private void paintRoundedLeftRight(final Graphics g, final boolean left, final boolean right, final Rectangle r) {
        if (right || left) {
            g.fillRoundRect(r.x, r.y, r.width, r.height, arcSize, arcSize);
            if (DEBUG_COLOR) g.setColor(Color.PINK);
            if (!left) {
                g.fillRect(r.x, r.y, arcSize, r.height);
            }
            if (!right) {
                g.fillRect(r.x + r.width - arcSize, r.y, arcSize, r.height);
            }
        } else {
            g.fillRect(r.x, r.y, r.width, r.height);
        }
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
