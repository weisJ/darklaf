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
package javax.swing.text.DefaultHighlighterDark;

import java.awt.*;
import java.awt.font.TextAttribute;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;
import java.util.Objects;

import javax.swing.*;
import javax.swing.text.*;

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.graphics.ColorWrapper;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.text.StyleConstantsEx;
import com.github.weisj.darklaf.util.Alignment;

/**
 * Note this class only sits inside this weird package because of a hack used in {@link
 * SwingUtilities2#useSelectedTextColor(Highlighter.Highlight, JTextComponent)} that makes it impossible for custom
 * highlighters to use the correct text foreground specified by {@link JTextComponent#getSelectedTextColor()}.
 *
 * @author Jannis Weis
 */
public class DarkHighlightPainter extends DefaultHighlighter.DefaultHighlightPainter {

    private Paint paint;
    private Color color;
    private final HighlighterColor wrapper;
    private boolean roundedEdges;
    private boolean extendLines;

    private AlphaComposite alphaComposite;
    private float alpha;
    private final int arcSize;
    private boolean enabled;

    private int repaintCount;

    private int lastSelStart = -1;
    private int lastSelEnd = -1;

    private final Shape[] arcs = new Shape[4];

    private final Point tmpPoint = new Point();

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
        wrapper = new HighlighterColor(null);
        arcSize = UIManager.getInt("Highlight.arc");
        setPaint(paint);
        setRoundedEdges(rounded);
        setAlpha(alpha);
        setEnabled(true);
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
    public void paint(final Graphics g, final int offs0, final int offs1, final Shape bounds,
                      final JTextComponent c) {
        if (!enabled) return;
        Graphics2D g2d = (Graphics2D) g;
        GraphicsContext context = new GraphicsContext(g2d);
        color = c.getSelectedTextColor();
        wrapper.setColor(color);
        wrapper.setCustomForeground(!Objects.equals(color, c.getForeground()));

        if (getAlpha() < 1.0f) {
            g2d.setComposite(getAlphaComposite());
        }
        super.paint(g, offs0, offs1, bounds, c);
        context.restore();
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

    public Paint getPaint(final Component c, final Shape bounds) {
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
        if (!enabled) return bounds;
        color = (Color) view.getAttributes().getAttribute(StyleConstantsEx.SelectedForeground);
        if (color == null) {
            color = StyleConstants.getForeground(view.getAttributes());
        }
        boolean customColor = color != null;
        if (color == null) {
            color = c.getSelectedTextColor();
        }

        boolean isForeground = Objects.equals(color, c.getForeground());
        customColor = customColor || isForeground;
        wrapper.setColor(color);
        wrapper.setCustomForeground(!isForeground);

        Graphics2D g2d = (Graphics2D) g;
        GraphicsContext context = GraphicsUtil.setupAAPainting(g2d);

        if (getAlpha() < 1.0f) {
            g2d.setComposite(getAlphaComposite());
        }

        setupColor((Graphics2D) g, c, bounds);
        Shape dirtyShape;

        boolean ltr = isLeftTorRight(c);

        if (ltr && (isLineExtendingEnabled() || isRounded())) {
            dirtyShape = paintRoundedLayer(g2d, c, offs0, offs1, context, false);

            // Swing may not recognise the painted extension. Just repaint if the selection changed to guarantee
            // everything is visible.
            if (isLineExtendingEnabled()) {
                if ((lastSelEnd != c.getSelectionEnd() || lastSelStart != c.getSelectionStart())) {
                    lastSelEnd = c.getSelectionEnd();
                    lastSelStart = c.getSelectionStart();
                    repaintCount = 1;
                }
                if (repaintCount > 0) {
                    if (customColor) {
                        c.repaint();
                    } else {
                        Rectangle repaintBounds = dirtyShape.getBounds();
                        repaintBounds.grow(10, 10);
                        c.repaint(repaintBounds);
                    }
                    repaintCount--;
                }
            }
        } else {
            Color oldColor = wrapper.getColor();
            wrapper.setColor(g.getColor());
            dirtyShape = super.paintLayer(g, offs0, offs1, bounds, c, view);
            wrapper.setColor(oldColor);
        }

        context.restore();
        return dirtyShape;
    }

    private boolean isLeftTorRight(final JTextComponent c) {
        return !TextAttribute.RUN_DIRECTION_RTL.equals(c.getDocument().getProperty(TextAttribute.RUN_DIRECTION))
               && !Boolean.TRUE.equals(c.getDocument().getProperty("i18n"));
    }

    protected Rectangle paintRoundedLayer(final Graphics2D g, final JTextComponent c, final int offs0, final int offs1,
                                          final GraphicsContext context, final boolean isPaintingPreceding) {
        Insets ins = c.getInsets();

        Rectangle posOffs0 = getPosRect(c, offs0);
        Rectangle posOffs1 = getPosRect(c, offs1, Position.Bias.Backward);

        Rectangle posStart = getPosRect(c, c.getSelectionStart());
        Rectangle posEnd = getPosRect(c, c.getSelectionEnd());

        int currentLineStart = isPaintingPreceding ? offs0 : getOffset(c, 0, posOffs0.y);
        int currentLineEnd = isPaintingPreceding ? offs1 : getOffset(c, c.getWidth(), posOffs1.y);

        int previousLineEnd = currentLineStart - 1;
        Rectangle prevEnd = getPosRect(c, previousLineEnd);
        int previousLineStart = getOffset(c, 0, prevEnd.y);
        Rectangle prevStart = getPosRect(c, previousLineStart);

        int nextLineStart = currentLineEnd + 1;
        int nextLineEnd = getOffset(c, c.getWidth(), getPosRect(c, nextLineStart).y);
        if (nextLineEnd < nextLineStart) nextLineEnd = nextLineStart;

        Rectangle layerRect = getRect(posOffs0, posOffs1);
        Rectangle previousLineRect = getRect(prevStart, prevEnd);
        Rectangle currentLineRect = getLineRect(c, currentLineStart, currentLineEnd);
        Rectangle nextLineRect = getLineRect(c, nextLineStart, nextLineEnd);

        /*
         * Adjust adjacent line rects if they contain the start/end of the selection.
         */
        if (posStart.y == previousLineRect.y) {
            previousLineRect = getRect(posStart, getPosRect(c, previousLineEnd, Position.Bias.Backward));
        }

        if (posEnd.y == nextLineRect.y) {
            nextLineRect = getRect(getPosRect(c, nextLineStart), posEnd);
        }

        /*
         * Adjust the current line rect if it contains the start/end of the selection.
         */
        if (posStart.y == currentLineRect.y) {
            currentLineRect.width = currentLineRect.x + currentLineRect.width - posStart.x;
            currentLineRect.x = posStart.x;
        }

        if (posEnd.y == currentLineRect.y) {
            currentLineRect.width = posEnd.x + posEnd.width - currentLineRect.x;
        }

        boolean hasLineAbove = previousLineEnd >= c.getSelectionStart();
        boolean hasLineBelow = nextLineStart <= c.getSelectionEnd();

        boolean previousLineVisible = hasLineAbove
                                      && previousLineRect.width > 0
                                      && previousLineStart < previousLineEnd;
        boolean nextLineVisible = hasLineBelow
                                  && nextLineRect.width > 0
                                  && nextLineStart < nextLineEnd;

        boolean paintPreviousLine = false;

        if (isLineExtendingEnabled()) {
            // Adjust the line rects for the adjacent lines for correct rounded corner placement.
            extendLine(c, ins, previousLineRect,
                       hasLineAbove && previousLineStart >= c.getSelectionStart(),
                       hasLineAbove && previousLineEnd <= c.getSelectionEnd());

            boolean extendNextRight = hasLineBelow
                                      && nextLineEnd < c.getDocument().getLength()
                                      && getOffset(c, getPosRect(c, nextLineEnd + 1)) <= c.getSelectionEnd();

            extendLine(c, ins, nextLineRect,
                       hasLineBelow && nextLineStart >= c.getSelectionStart(),
                       extendNextRight);

            paintPreviousLine = hasLineAbove && previousLineStart == previousLineEnd;
            previousLineVisible = hasLineAbove;
            if (nextLineRect.y != posEnd.y) nextLineVisible = hasLineBelow;

            boolean extendRight = isPaintingPreceding || (hasLineBelow && (endX(layerRect) == endX(currentLineRect)));
            boolean extendLeft = isPaintingPreceding || (hasLineAbove && startX(layerRect) == startX(currentLineRect));

            extendLine(c, ins, layerRect, extendLeft, extendRight);
            extendLine(c, ins, currentLineRect, extendLeft, extendRight);

            if (isPaintingPreceding) layerRect.height = currentLineRect.height;
        }

        boolean rounded = isRounded();
        boolean canRoundLeft = rounded && startX(currentLineRect) == startX(layerRect);
        boolean canRoundRight = rounded && endX(currentLineRect) == endX(layerRect);

        boolean roundedTopLeft = !previousLineVisible || leftRoundedVisible(currentLineRect, previousLineRect);
        boolean roundedTopRight = !previousLineVisible || rightRoundedVisible(currentLineRect, previousLineRect);
        boolean roundedBottomLeft = !nextLineVisible || leftRoundedVisible(currentLineRect, nextLineRect);
        boolean roundedBottomRight = !nextLineVisible || rightRoundedVisible(currentLineRect, nextLineRect);

        boolean arcTopLeft = previousLineVisible && !roundedTopLeft
                             && leftArcVisible(currentLineRect, previousLineRect);
        boolean arcTopRight = previousLineVisible && !roundedTopRight
                              && rightArcVisible(currentLineRect, previousLineRect);
        boolean arcBottomLeft = nextLineVisible && !roundedBottomLeft && leftArcVisible(currentLineRect, nextLineRect);
        boolean arcBottomRight = nextLineVisible && !roundedBottomRight
                                 && rightArcVisible(currentLineRect, nextLineRect);

        layerRect = paintRoundRect(g, context, layerRect,
                                   canRoundLeft && roundedTopLeft,
                                   canRoundRight && roundedTopRight,
                                   canRoundLeft && roundedBottomLeft,
                                   canRoundRight && roundedBottomRight);
        Rectangle r = paintArcs(g, context, layerRect,
                                canRoundLeft && arcTopLeft,
                                canRoundRight && arcTopRight,
                                canRoundLeft && arcBottomLeft,
                                canRoundRight && arcBottomRight);

        if (paintPreviousLine && !isPaintingPreceding) {
            Rectangle prev = paintRoundedLayer(g, c, previousLineStart, previousLineEnd, context, true);
            convexHull(r, prev);
        }
        return r;
    }

    private void extendLine(final JTextComponent c, final Insets ins, final Rectangle previousLineRect,
                            final boolean extendLeft, final boolean extendRight) {
        if (extendRight || extendLeft) {
            int x = startX(previousLineRect);
            if (extendRight) {
                previousLineRect.width = c.getWidth() - ins.left - x;
            }
            if (extendLeft) {
                previousLineRect.width = previousLineRect.x + previousLineRect.width - ins.left;
                previousLineRect.x = ins.left;
            }
        }
    }

    public boolean isLineExtendingEnabled() {
        return extendLines;
    }

    public void setLineExtendingEnabled(final boolean enabled) {
        extendLines = enabled;
    }

    private boolean leftRoundedVisible(final Rectangle current, final Rectangle other) {
        return startX(other) > startX(current) + arcSize
               || endX(other) < startX(current);
    }

    private boolean rightRoundedVisible(final Rectangle current, final Rectangle other) {
        return endX(other) < endX(current) - arcSize
               || startX(other) > endX(current);
    }

    private boolean leftArcVisible(final Rectangle current, final Rectangle other) {
        return startX(other) <= startX(current) - arcSize
               && endX(other) >= startX(current);
    }

    private boolean rightArcVisible(final Rectangle current, final Rectangle other) {
        return endX(other) >= endX(current) + arcSize
               && startX(other) <= endX(current);
    }

    private Rectangle getPosRect(final JTextComponent c, final int offset) {
        return getPosRect(c, offset, Position.Bias.Forward);
    }

    private Rectangle getPosRect(final JTextComponent c, final int offset, final Position.Bias bias) {
        try {
            return c.getUI().modelToView(c, Math.max(0, Math.min(offset, c.getDocument().getLength())), bias);
        } catch (BadLocationException ignored) {} catch (IllegalArgumentException e) {
            new RuntimeException("" + offset).printStackTrace();
        }
        return new Rectangle(Integer.MIN_VALUE + 100, Integer.MIN_VALUE + 100, 0, 0);
    }

    private int getOffset(final JTextComponent c, final int x, final int y) {
        tmpPoint.setLocation(x, y);
        return getOffset(c, tmpPoint);
    }

    private int getOffset(final JTextComponent c, final Rectangle r) {
        return getOffset(c, r.getLocation());
    }

    private int getOffset(final JTextComponent c, final Point p) {
        return c.viewToModel(p);
    }

    private int startX(final Rectangle r) {
        return r.x;
    }

    private int endX(final Rectangle r) {
        return r.x + r.width;
    }

    private int startY(final Rectangle r) {
        return r.y;
    }

    private int endY(final Rectangle r) {
        return r.y + r.height;
    }

    private Rectangle getLineRect(final JTextComponent c, final int startPos, final int endPos) {
        Rectangle rectStart = getPosRect(c, startPos);
        Rectangle rectEnd = endPos <= startPos ? rectStart : getPosRect(c, endPos, Position.Bias.Backward);
        return getRect(rectStart, rectEnd);
    }

    private Rectangle getRect(final Rectangle start, final Rectangle end) {
        return new Rectangle(start.x, start.y, end.x + end.width - start.x, end.y + end.height - start.y);
    }

    private void convexHull(final Rectangle start, final Rectangle end) {
        int startX = Math.min(startX(start), startX(end));
        int startY = Math.min(startY(start), startY(end));

        int endX = Math.max(endX(start), endX(end));
        int endY = Math.max(endY(start), endY(end));

        start.setRect(startX, startY, endX - startX, endY - startY);
    }

    private boolean isRounded() {
        return roundedEdges;
    }

    private Rectangle paintRoundRect(final Graphics g, final GraphicsContext context, final Rectangle shape,
                                     final boolean topLeft, final boolean topRight,
                                     final boolean bottomLeft, final boolean bottomRight) {
        Rectangle r = new Rectangle(shape);
        int aw = Math.min(arcSize, r.width);
        int ah = Math.min(arcSize, r.height);

        /*
         * If there is a rounded arc on one side A and none on the other (B) removing the arc B could overlap
         * arc A if the width of the allocation is too small. This happens e.g. when single characters have different
         * attribute sets.
         */
        g.clipRect(r.x, r.y, r.width, r.height);
        boolean showLeft = topLeft || bottomLeft;
        boolean showRight = topRight || bottomRight;
        boolean showTop = topRight || topLeft;
        boolean showBottom = bottomLeft || bottomRight;

        if (!showLeft) {
            r.x -= aw;
            r.width += aw;
        }
        if (!showRight) {
            r.width += aw;
        }
        if (!showBottom) {
            r.height += ah;
        }
        if (!showTop) {
            r.y -= ah;
            r.height += ah;
        }

        if (!topLeft && !topRight && !bottomRight && !bottomLeft) {
            PaintUtil.fillRect(g, r);
        } else {
            /*
             * First paint the whole rounded rectangle and remove any non visible arcs.
             */
            g.fillRoundRect(r.x, r.y, r.width, r.height, aw, ah);
            if (!topLeft && !topRight) {
                g.fillRect(r.x, r.y, r.width, ah);
            } else {
                if (!topLeft) g.fillRect(r.x, r.y, aw, ah);
                if (!topRight) g.fillRect(r.x + r.width - aw, r.y, aw, ah);
            }
            if (!bottomLeft && !bottomRight) {
                g.fillRect(r.x, r.y + r.height - ah, r.width, ah);
            } else {
                if (!bottomLeft) g.fillRect(r.x, r.y + r.height - ah, aw, ah);
                if (!bottomRight) g.fillRect(r.x + r.width - aw, r.y + r.height - ah, aw, ah);
            }
        }
        context.restoreClip();
        return shape;
    }

    private Rectangle paintArcs(final Graphics2D g, final GraphicsContext context, final Rectangle r,
                                final boolean topLeft, final boolean topRight,
                                final boolean bottomLeft, final boolean bottomRight) {
        Rectangle alloc = new Rectangle(r);
        if (topLeft || bottomLeft) {
            alloc.x -= arcSize;
            alloc.width += arcSize;
        }
        if (topRight || bottomRight) {
            alloc.width += arcSize;
        }

        if (topLeft) {
            g.translate(r.x - arcSize, r.y);
            g.fill(getArc(Alignment.NORTH_WEST));
            context.restoreTransform();
        }
        if (topRight) {
            g.translate(r.x + r.width, r.y);
            g.fill(getArc(Alignment.NORTH_EAST));
            context.restoreTransform();
        }
        if (bottomLeft) {
            g.translate(r.x - arcSize, r.y + r.height - arcSize);
            g.fill(getArc(Alignment.SOUTH_WEST));
            context.restoreTransform();
        }
        if (bottomRight) {
            g.translate(r.x + r.width, r.y + r.height - arcSize);
            g.fill(getArc(Alignment.SOUTH_EAST));
            context.restoreTransform();
        }
        return alloc;
    }

    private void setupColor(final Graphics2D g2d, final JTextComponent c, final Shape bounds) {
        Paint paint = getPaint(c, bounds);
        if (paint == null) {
            g2d.setColor(c.getSelectionColor());
        } else {
            g2d.setPaint(paint);
        }
    }

    private Shape getArc(final Alignment a) {
        Shape arc = arcs[getIndex(a)];
        if (arc == null) {
            Area arcArea = new Area(new Rectangle2D.Double(0, 0, arcSize, arcSize));
            arcArea.subtract(new Area(getSubtractShape(a)));
            arc = arcArea;
            arcs[getIndex(a)] = arc;
        }
        return arc;
    }

    private Shape getSubtractShape(final Alignment a) {
        switch (a) {
            case NORTH_EAST :
                return new Arc2D.Double(0, 0, 2 * arcSize, 2 * arcSize, 90, 90, Arc2D.PIE);
            case NORTH_WEST :
                return new Arc2D.Double(-arcSize, 0, 2 * arcSize, 2 * arcSize, 0, 90, Arc2D.PIE);
            case SOUTH_EAST :
                return new Arc2D.Double(0, -arcSize, 2 * arcSize, 2 * arcSize, 180, 90, Arc2D.PIE);
            case SOUTH_WEST :
                return new Arc2D.Double(-arcSize, -arcSize, 2 * arcSize, 2 * arcSize, 270, 90, Arc2D.PIE);
        }
        return new Rectangle();
    }

    private int getIndex(final Alignment a) {
        switch (a) {
            case NORTH_EAST :
                return 0;
            case NORTH_WEST :
                return 1;
            case SOUTH_EAST :
                return 2;
            case SOUTH_WEST :
                return 3;
        }
        return 0;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        wrapper.setEnabled(enabled);
        this.enabled = enabled;
    }

    private static class HighlighterColor extends ColorWrapper {

        private boolean enabled;
        private boolean customForeground;

        public void setEnabled(final boolean enabled) {
            this.enabled = enabled;
        }

        public void setCustomForeground(final boolean customForeground) {
            this.customForeground = customForeground;
        }

        public HighlighterColor(final Color color) {
            super(color);
        }

        @Override
        @SuppressWarnings("EqualsWhichDoesntCheckParameterClass")
        public boolean equals(final Object obj) {
            return obj != null && enabled && !customForeground;
        }
    }
}
