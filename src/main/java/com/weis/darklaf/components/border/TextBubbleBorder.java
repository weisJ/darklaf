package com.weis.darklaf.components.border;

import com.weis.darklaf.components.alignment.Alignment;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.border.AbstractBorder;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;

/**
 * Border that looks like a Text Bubble.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class TextBubbleBorder extends AbstractBorder {

    private static final long serialVersionUID = 1L;
    @NotNull
    private final RenderingHints hints;
    @NotNull
    private final Insets insets;
    private Alignment pointerSide = Alignment.NORTH;
    private Color color;
    private int thickness;
    private int radius;
    private int pointerSize;
    private int pointerWidth;
    private BasicStroke stroke;
    private double pointerPadPercent = 0.5;


    /**
     * Create new TextBubbleBorder with given colour.
     *
     * @param color color of border
     */
    public TextBubbleBorder(final Color color) {
        this(color, 2, 4, 5);
    }

    /**
     * Create new TextBubbleBorder.
     *
     * @param color       Colour of bubble.
     * @param thickness   Line thickness of border.
     * @param radius      corner radius of border.
     * @param pointerSize size of pointer. You can set this size to 0 to achieve no pointer, but it
     *                    is not desirable. The appropriate method for this is to set using {@link
     *                    TextBubbleBorder#setPointerSide(Alignment)} to {@link Alignment#CENTER}
     */
    public TextBubbleBorder(final Color color, final int thickness,
                            final int radius, final int pointerSize) {
        this.color = color;
        this.thickness = thickness;
        this.radius = radius;
        this.pointerSize = pointerSize;
        this.pointerWidth = pointerSize;

        hints = new RenderingHints(RenderingHints.KEY_ANTIALIASING,
                                   RenderingHints.VALUE_ANTIALIAS_ON);
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
    public TextBubbleBorder setColor(final Color color) {
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
    public TextBubbleBorder setPointerWidth(final int pointerWidth) {
        this.pointerWidth = pointerWidth;
        return this;
    }

    /**
     * Get the percentage for pointer padding where 0 - means left/top 1 - means right/bottom.
     *
     * @return percentage of padding
     */
    public double getPointerPadPercent() {
        return pointerPadPercent;
    }

    /**
     * Set the percentage for pointer padding where 0 - means left/top 1 - means right/bottom Clips
     * at 0 and 1.
     *
     * @param percent percentage between 0 and 1
     * @return this
     */
    @NotNull
    public TextBubbleBorder setPointerPadPercent(final double percent) {
        this.pointerPadPercent = percent > 1 ? 1 : percent;
        pointerPadPercent = pointerPadPercent < 0 ? 0 : pointerPadPercent;
        return this;
    }


    /**
     * Get the border thickness.
     *
     * @return thickness of border.
     */
    public int getThickness() {
        return pointerSide == Alignment.CENTER ? 0 : thickness;
    }

    /**
     * Set the border thickness.
     *
     * @param n new thickness
     * @return this
     */
    @NotNull
    public TextBubbleBorder setThickness(final int n) {
        thickness = n < 0 ? 0 : n;
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
    public TextBubbleBorder setRadius(final int radius) {
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
    public TextBubbleBorder setPointerSize(final int size) {
        pointerSize = size < 0 ? 0 : size;
        final int pad = radius / 2 + thickness;
        final int pointerSidePad = pad + pointerSize + thickness;
        int left = pad;
        int right = pad;
        int bottom = pad;
        int top = pad;

        switch (pointerSide) {
            case NORTH, NORTH_WEST, NORTH_EAST -> top = pointerSidePad;
            case SOUTH, SOUTH_WEST, SOUTH_EAST -> bottom = pointerSidePad;
            case WEST -> left = pointerSidePad;
            case EAST -> right = pointerSidePad;
            default -> {
            }
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
     * Set the alignment for the pointer. Not there is no difference between {@link
     * Alignment#NORTH}, {@link Alignment#NORTH_EAST} and {@link Alignment#NORTH_WEST} as well as
     * {@link Alignment#SOUTH}, {@link Alignment#SOUTH_EAST} and {@link Alignment#SOUTH_WEST} {@link
     * Alignment#CENTER} results in no pointer.
     *
     * @param side direction in which the pointer should point.
     * @return this.
     */
    @NotNull
    public TextBubbleBorder setPointerSide(final Alignment side) {
        this.pointerSide = side;
        return setPointerSize(pointerSize);
    }

    @NotNull
    @Override
    public Insets getBorderInsets(final Component c) {
        return insets;
    }


    @NotNull
    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        return getBorderInsets(c);
    }

    @Override
    public void paintBorder(@NotNull final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        final Graphics2D g2 = (Graphics2D) g;
        var bubble = calculateBubbleRect(width, height);
        final int pointerPad = switch (pointerSide) {
            case WEST, EAST -> (int) (pointerPadPercent * (height - 2 * radius - 5 * pointerSize));
            case CENTER -> 0;
            default -> (int) (pointerPadPercent * (width - 2 * radius - 5 * pointerSize));
        };
        final Polygon pointer = creatPointerShape(width, height, pointerPad, bubble);
        final Area area = new Area(bubble);
        area.add(new Area(pointer));
        g2.setRenderingHints(hints);
        g2.setColor(c.getBackground());
        g2.fill(area);
        g2.setColor(color);
        g2.setStroke(stroke);
        g2.draw(area);
    }

    @NotNull
    @Contract("_, _ -> new")
    private RoundRectangle2D.Double calculateBubbleRect(final int width, final int height) {
        int rx = thickness;
        int ry = thickness;
        int rw = width - thickness;
        int rh = height - thickness;
        switch (pointerSide) {
            case WEST -> {
                rx += pointerSize;
                rw -= pointerSize + thickness;
                rh -= thickness;
            }
            case EAST -> {
                rw -= pointerSize;
                rh -= thickness;
            }
            case NORTH, NORTH_WEST, NORTH_EAST -> {
                ry += pointerSize;
                rh -= pointerSize + thickness;
                rw -= thickness;
            }
            case SOUTH, SOUTH_WEST, SOUTH_EAST -> {
                rh -= pointerSize;
                rw -= thickness;
            }
            default -> {
            }
        }
        return new RoundRectangle2D.Double(rx, ry, rw, rh, radius, radius);
    }

    @NotNull
    private Polygon creatPointerShape(final int width, final int height, final int pointerPad,
                                      @NotNull final RoundRectangle2D.Double bubble) {
        final int basePad = 2 * pointerSize + thickness + radius + pointerPad;
        final int widthPad = pointerWidth / 2;
        final Polygon pointer = new Polygon();
        switch (pointerSide) {
            case WEST -> {
                pointer.addPoint((int) bubble.x, basePad - widthPad);// top
                pointer.addPoint((int) bubble.x, basePad + pointerSize + widthPad);// bottom
                pointer.addPoint(thickness, basePad + pointerSize / 2);
            }
            case EAST -> {
                int x = (int) (bubble.x + bubble.width);
                pointer.addPoint(x, basePad - widthPad);// top
                pointer.addPoint(x, basePad + pointerSize + widthPad);// bottom
                pointer.addPoint(width - thickness, basePad + pointerSize / 2);
            }
            case NORTH, NORTH_WEST, NORTH_EAST -> {
                pointer.addPoint(basePad - widthPad, (int) bubble.y);// left
                pointer.addPoint(basePad + pointerSize + widthPad, (int) bubble.y);// right
                pointer.addPoint(basePad + (pointerSize / 2), thickness);
            }
            case SOUTH, SOUTH_WEST, SOUTH_EAST -> {
                int y = (int) (bubble.y + bubble.height);
                pointer.addPoint(basePad - widthPad, y);// left
                pointer.addPoint(basePad + pointerSize + widthPad, y);// right
                pointer.addPoint(basePad + (pointerSize / 2), height - thickness);
            }
            default -> {
            }
        }
        return pointer;
    }
}