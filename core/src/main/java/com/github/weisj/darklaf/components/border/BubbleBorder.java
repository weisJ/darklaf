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
 */
package com.github.weisj.darklaf.components.border;

import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;

import javax.swing.border.AbstractBorder;
import javax.swing.plaf.InsetsUIResource;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Jannis Weis
 */
public class BubbleBorder extends AbstractBorder {

    private final Insets insets;
    private Alignment pointerSide = Alignment.NORTH;
    private Color color;
    private int thickness;
    private int radius;
    private int pointerSize;
    private int pointerWidth;

    /**
     * Create new TextBubbleBorder with given colour.
     *
     * @param color color of border.
     */
    public BubbleBorder(final Color color) {
        this(color, 1, 5, 5);
    }

    /**
     * Create new TextBubbleBorder.
     *
     * @param color       Colour of bubble.
     * @param thickness   Line thickness of border.
     * @param radius      corner radius of border.
     * @param pointerSize size of pointer. You can set this size to 0 to achieve no pointer, but it is not desirable.
     *                    The appropriate method for this is to set using {@link BubbleBorder#setPointerSide(Alignment)}
     *                    to. {@link Alignment#CENTER}
     */
    public BubbleBorder(final Color color, final int thickness,
                        final int radius, final int pointerSize) {
        this.color = color;
        this.thickness = thickness;
        this.radius = radius;
        this.pointerSize = pointerSize;
        this.pointerWidth = pointerSize;
        insets = new InsetsUIResource(0, 0, 0, 0);
        setThickness(thickness);
    }

    /**
     * Get border Colour.
     *
     * @return border colour
     */
    public Color getColor() {
        return color;
    }

    /**
     * Set the border colour.
     *
     * @param color border colour
     * @return this
     */

    public BubbleBorder setColor(final Color color) {
        this.color = color;
        return this;
    }

    /**
     * Get the width of the pointer base.
     *
     * @return the width of the base.
     */
    public int getPointerWidth() {
        return pointerWidth;
    }

    /**
     * Set the with of the pointer base.
     *
     * @param pointerWidth the width of the pointer base.
     * @return this
     */
    public BubbleBorder setPointerWidth(final int pointerWidth) {
        this.pointerWidth = pointerWidth;
        return this;
    }


    /**
     * Get the border thickness.
     *
     * @return thickness of border.
     */
    public int getThickness() {
        return thickness;
    }

    /**
     * Set the border thickness.
     *
     * @param n new thickness
     * @return this
     */

    public BubbleBorder setThickness(final int n) {
        thickness = Math.max(n, 0);
        return setPointerSize(pointerSize);
    }

    /**
     * Get the corner radius.
     *
     * @return radius of corners
     */
    public float getRadius() {
        return radius;
    }

    /**
     * Set the pointer size Clips at 0.
     *
     * @param size size of pointer.
     * @return this
     */

    public BubbleBorder setPointerSize(final int size) {
        pointerSize = Math.max(size, 0);
        float left = thickness;
        float right = thickness;
        float bottom = thickness;
        float top = thickness;

        switch (pointerSide) {
            case NORTH:
            case NORTH_WEST:
            case NORTH_EAST:
                top += pointerSize;
                break;
            case SOUTH:
            case SOUTH_WEST:
            case SOUTH_EAST:
                bottom += pointerSize;
                break;
            case WEST:
                left += pointerSize;
                break;
            case EAST:
                right += pointerSize;
                break;
            default:
                break;
        }
        insets.set((int) top, (int) left, (int) bottom, (int) right);
        return this;
    }

    /**
     * Get the pointer size.
     *
     * @return size of pointer.
     */
    public int getPointerSize() {
        return pointerSize;
    }

    /**
     * Set the corner radius.
     *
     * @param radius radius of corner.
     * @return this
     */

    public BubbleBorder setRadius(final int radius) {
        this.radius = radius;
        return setPointerSize(pointerSize);
    }

    /**
     * Get the Alignment the pointer follows. Default is {@link Alignment#NORTH}
     *
     * @return alignment
     */
    public Alignment getPointerSide() {
        return pointerSide;
    }

    /**
     * Set the alignment for the pointer. Not there is no difference between {@link Alignment#NORTH}, {@link
     * Alignment#NORTH_EAST} and {@link Alignment#NORTH_WEST} as well as {@link Alignment#SOUTH}, {@link
     * Alignment#SOUTH_EAST} and {@link Alignment#SOUTH_WEST} {@link Alignment#CENTER} results in no pointer.
     *
     * @param side direction in which the pointer should point.
     * @return this.
     */

    public BubbleBorder setPointerSide(final Alignment side) {
        this.pointerSide = side;
        setPointerSize(pointerSize);
        return this;
    }

    public float getOffset(final float w, final float h) {
        return (float) calculatePointerPad(w, h, Alignment.NORTH_WEST);
    }


    private double calculatePointerPad(final float width, final float height, final Alignment side) {
        double pointerPad;
        switch (side) {
            case WEST:
            case EAST:
                pointerPad = radius + (height - insets.top - insets.bottom - 2 * radius) / 2.0;
                break;
            case NORTH_WEST:
            case SOUTH_WEST:
                pointerPad = radius + insets.left + pointerWidth;
                break;
            case NORTH_EAST:
            case SOUTH_EAST:
                pointerPad = width - radius - insets.right - pointerWidth;
                break;
            case SOUTH:
            case NORTH:
                pointerPad = radius + (0.5 * (width - insets.left - insets.right - 2 * radius));
                break;
            default:
                pointerPad = 0;
                break;
        }
        return pointerPad;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        Area area = getBorderArea(x, y, width, height);
        paintBorder(g, area);
    }


    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(insets.top, insets.left, insets.bottom, insets.right);
    }


    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        return getBorderInsets(c);
    }

    public Area getBubbleArea(final float x, final float y, final float width, final float height,
                              final boolean inner) {
        int adj = inner ? getThickness() : 0;
        float w = width - 2 * adj;
        float h = height - 2 * adj;
        double pSize = inner ? getPointerSize() - getThickness() : getPointerSize();
        double pWidth = inner ? getPointerWidth() - 2 * getThickness() : getPointerWidth();
        RoundRectangle2D.Float bubble = calculateBubbleRect(x + adj, y + adj, w, h);
        final Area area = new Area(bubble);
        if (pointerSide != Alignment.CENTER) {
            double pointerPad = calculatePointerPad(w, h, pointerSide);
            Path2D pointer = creatPointerShape(pointerPad, pSize, pWidth, bubble);
            area.add(new Area(pointer));
        }
        return area;
    }

    public Area getBorderArea(final int x, final int y, final int width, final int height) {
        Area outer = getBubbleArea(x, y, width, height, false);
        Area inner = getBubbleArea(x, y, width, height, true);
        outer.subtract(inner);
        return outer;
    }

    public void paintBorder(final Graphics g, final Area strokeArea) {
        final Graphics2D g2 = (Graphics2D) g;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        g2.setColor(color);
        g2.fill(strokeArea);
        config.restore();
    }


    public RoundRectangle2D.Float calculateBubbleRect(final float x, final float y,
                                                      final float width, final float height) {
        return new RoundRectangle2D.Float(x + insets.left, y + insets.top, width - insets.left - insets.right,
                                          height - insets.top - insets.bottom, radius, radius);
    }


    private Path2D creatPointerShape(final double pointerPad, final double pSize, final double pWidth,
                                     final RoundRectangle2D.Float bubble) {
        final double w = pWidth / 2.0;
        final Path2D pointer = new Path2D.Double(Path2D.WIND_EVEN_ODD);
        double x = bubble.x;
        double y = bubble.y;
        switch (pointerSide) {
            case WEST:
                pointer.moveTo(x, y + pointerPad - w); //Top
                pointer.lineTo(x - pSize, y + pointerPad);
                pointer.lineTo(x, y + pointerPad + w);// bottom
                break;
            case EAST:
                pointer.moveTo(x + bubble.width, y + pointerPad - w);// top
                pointer.lineTo(x + bubble.width + pSize, y + pointerPad);
                pointer.lineTo(x + bubble.width, y + pointerPad + w);// bottom
                break;
            case NORTH:
            case NORTH_WEST:
            case NORTH_EAST:
                pointer.moveTo(x + pointerPad - w, y);// left
                pointer.lineTo(x + pointerPad, y - pSize);
                pointer.lineTo(x + pointerPad + w, y);// right
                break;
            case SOUTH:
            case SOUTH_WEST:
            case SOUTH_EAST:
                pointer.moveTo(x + pointerPad - w, y + bubble.height);// left
                pointer.lineTo(x + pointerPad, y + bubble.height + pSize);
                pointer.lineTo(x + pointerPad + w, y + bubble.height);// right
                break;
            default:
                break;
        }
        pointer.closePath();
        return pointer;
    }
}
