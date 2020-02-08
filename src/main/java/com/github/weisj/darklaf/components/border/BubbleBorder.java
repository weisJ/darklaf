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

import com.github.weisj.darklaf.components.alignment.Alignment;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.border.AbstractBorder;
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
    private BasicStroke stroke;


    /**
     * Create new TextBubbleBorder with given colour.
     *
     * @param color color of border
     */
    public BubbleBorder(final Color color) {
        this(color, 2, 4, 5);
    }

    /**
     * Create new TextBubbleBorder.
     *
     * @param color       Colour of bubble.
     * @param thickness   Line thickness of border.
     * @param radius      corner radius of border.
     * @param pointerSize size of pointer. You can set this size to 0 to achieve no pointer, but it is not desirable.
     *                    The appropriate method for this is to set using {@link BubbleBorder#setPointerSide(Alignment)}
     *                    to {@link Alignment#CENTER}
     */
    public BubbleBorder(final Color color, final int thickness,
                        final int radius, final int pointerSize) {
        this.color = color;
        this.thickness = thickness;
        this.radius = radius;
        this.pointerSize = pointerSize;
        this.pointerWidth = pointerSize;
        insets = new Insets(0, 0, 0, 0);
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
    @NotNull
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
    @NotNull
    public BubbleBorder setThickness(final int n) {
        thickness = Math.max(n, 0);
        stroke = new BasicStroke(thickness);
        return setPointerSize(pointerSize);
    }

    /**
     * Get the corner radius.
     *
     * @return radius of corners
     */
    public int getRadius() {
        return radius;
    }

    /**
     * Set the corner radius.
     *
     * @param radius radius of corner.
     * @return this
     */
    @NotNull
    public BubbleBorder setRadius(final int radius) {
        this.radius = radius;
        return setPointerSize(pointerSize);
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
     * Set the pointer size Clips at 0.
     *
     * @param size size of pointer.
     * @return this
     */
    @NotNull
    public BubbleBorder setPointerSize(final int size) {
        pointerSize = Math.max(size, 0);
        int left = thickness;
        int right = thickness;
        int bottom = thickness;
        int top = thickness;

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
        insets.set(top, left, bottom, right);
        return this;
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
    @NotNull
    public BubbleBorder setPointerSide(final Alignment side) {
        this.pointerSide = side;
        setPointerSize(pointerSize);
        return this;
    }

    public int getOffset(final int w, final int h) {
        return (int) calculatePointerPad(w, h, Alignment.NORTH_WEST);
    }

    @Contract(pure = true)
    private double calculatePointerPad(final int width, final int height, final Alignment side) {
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
    public void paintBorder(@NotNull final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        Area area = getInnerArea(x, y, width, height);
        paintBorder(g, area);
    }

    @NotNull
    @Override
    public Insets getBorderInsets(final Component c) {
        return new Insets(insets.top, insets.left, insets.bottom, insets.right);
    }

    @NotNull
    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        return getBorderInsets(c);
    }

    public Area getInnerArea(final int x, final int y, final int width, final int height) {
        RoundRectangle2D.Double bubble = calculateBubbleRect(x, y, width, height);
        final Area area = new Area(bubble);
        if (pointerSide != Alignment.CENTER) {
            double pointerPad = calculatePointerPad(width, height, pointerSide);
            Path2D pointer = creatPointerShape(pointerPad, bubble);
            area.add(new Area(pointer));
        }
        return area;
    }

    public void paintBorder(final Graphics g, final Area innerArea) {
        final Graphics2D g2 = (Graphics2D) g;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        g2.setColor(color);
        g2.setStroke(stroke);
        g2.draw(innerArea);
        config.restore();
    }

    @Contract("_, _, _, _ -> new")
    public RoundRectangle2D.Double calculateBubbleRect(final int x, final int y,
                                                       final int width, final int height) {
        return new RoundRectangle2D.Double(x + insets.left, y + insets.top, width - insets.left - insets.right,
                                           height - insets.top - insets.bottom, radius, radius);
    }

    @NotNull
    private Path2D creatPointerShape(final double pointerPad, @NotNull final RoundRectangle2D.Double bubble) {
        final double w = pointerWidth / 2.0;
        final Path2D pointer = new Path2D.Double(Path2D.WIND_EVEN_ODD);
        double x = bubble.x;
        double y = bubble.y;
        switch (pointerSide) {
            case WEST:
                pointer.moveTo(x, y + pointerPad - w); //Top
                pointer.lineTo(x - pointerSize, y + pointerPad);
                pointer.lineTo(x, y + pointerPad + w);// bottom
                break;
            case EAST:
                pointer.moveTo(x + bubble.width, y + pointerPad - w);// top
                pointer.lineTo(x + bubble.width + pointerSize, y + pointerPad);
                pointer.lineTo(x + bubble.width, y + pointerPad + w);// bottom
                break;
            case NORTH:
            case NORTH_WEST:
            case NORTH_EAST:
                pointer.moveTo(x + pointerPad - w, y);// left
                pointer.lineTo(x + pointerPad, y - pointerSize);
                pointer.lineTo(x + pointerPad + w, y);// right
                break;
            case SOUTH:
            case SOUTH_WEST:
            case SOUTH_EAST:
                pointer.moveTo(x + pointerPad - w, y + bubble.height);// left
                pointer.lineTo(x + pointerPad, y + bubble.height + pointerSize);
                pointer.lineTo(x + pointerPad + w, y + bubble.height);// right
                break;
            default:
                break;
        }
        pointer.closePath();
        return pointer;
    }
}