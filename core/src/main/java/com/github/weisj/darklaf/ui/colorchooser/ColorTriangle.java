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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.awt.image.ColorModel;
import java.awt.image.Raster;
import java.awt.image.WritableRaster;
import java.util.ArrayList;
import java.util.List;

import javax.swing.*;

import com.github.weisj.darklaf.color.ColorUtil;
import com.github.weisj.darklaf.color.DarkColorModel;
import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.color.DarkColorModelHSL;
import com.github.weisj.darklaf.color.DarkColorModelRGB;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;

public class ColorTriangle extends JComponent {

    protected static final double SQRT3 = Math.sqrt(3);
    private static final Point2D dummy = new Point2D.Double();
    protected static final AlphaComposite COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER);

    protected final List<ColorListener> myListeners = new ArrayList<>();
    protected Color dropFill;
    protected Color dropBorder;
    protected Color background;
    protected int outerIndicatorRadius;
    protected int innerIndicatorRadius;

    protected Color color;
    protected double hueHSB;
    protected double hueHSL;
    protected double valueHSB;
    protected double saturationHSB;
    protected double saturationHSL;
    protected double lightnessHSL;
    protected double opacity = 1.0;

    protected CircleInfo circleInfo = new CircleInfo();
    protected Shape circleShape;
    protected Shape triangleShape;
    protected AffineTransform triangleInverse;
    protected Shape outerIndicator;
    protected Shape innerIndicator;

    protected double centerX;
    protected double centerY;
    protected double outerRadius;
    protected double innerRadius;
    protected double rotation;

    protected PickResult lastPick;
    protected int minSize = 300;

    protected boolean isMessaging;
    protected boolean invalid;
    private boolean isHSB = true;

    public ColorTriangle() {
        setOpaque(true);
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(final ComponentEvent e) {
                invalid = true;
            }
        });
        addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(final MouseEvent e) {
                setFromPickResult(pick(e.getX(), e.getY()), true);
            }

            @Override
            public void mouseReleased(final MouseEvent e) {
                lastPick = null;
            }
        });
        addMouseMotionListener(new MouseMotionAdapter() {
            @Override
            public void mouseDragged(final MouseEvent e) {
                if (lastPick != null && lastPick.area != PickArea.OUTSIDE) {
                    setFromPickResult(pick(e.getX(), e.getY()), false);
                }
            }
        });
        updateDefaults();
    }

    protected void updateDefaults() {
        background = UIManager.getColor("ColorChooser.colorWheelBackground");
        dropFill = UIManager.getColor("ColorChooser.colorWheelDropBackgroundColor");
        if (dropFill == null) dropFill = UIManager.getColor("Label.background");
        dropBorder = UIManager.getColor("ColorChooser.colorWheelDropBorderColor");
        if (dropBorder == null) dropBorder = UIManager.getColor("Label.foreground");
        outerIndicatorRadius = UIManager.getInt("ColorChooser.outerIndicatorRadius");
        if (outerIndicatorRadius == 0) outerIndicatorRadius = 3;
        innerIndicatorRadius = UIManager.getInt("ColorChooser.innerIndicatorRadius");
        if (innerIndicatorRadius == 0) innerIndicatorRadius = 3;
    }

    protected double getHue() {
        return isHSB ? getHSBHue() : getHSLHue();
    }

    protected double getHSBHue() {
        return hueHSB;
    }

    protected double getHSLHue() {
        return hueHSL;
    }

    protected double getSaturation() {
        return isHSB ? getHSBSaturation() : getHSLSaturation();
    }

    protected double getHSBSaturation() {
        return saturationHSB;
    }

    protected double getHSLSaturation() {
        return saturationHSL;
    }

    protected double getValue() {
        return isHSB ? getHSBValue() : getHSLValue();
    }

    protected double getHSBValue() {
        return valueHSB;
    }

    protected double getHSLValue() {
        return lightnessHSL;
    }

    protected void setHue(final double hue) {
        if (isHSB) {
            setHSBHue(hue);
        } else {
            setHSLHue(hue);
        }
    }

    protected void setHSBHue(final double hue) {
        hueHSB = hue;
        if (hueHSB < 0) hueHSB += 1.0;
    }

    protected void setHSLHue(final double hue) {
        hueHSL = hue;
        if (hueHSL < 0) hueHSL += 1.0;
    }

    protected void setValue(final double value) {
        if (isHSB) {
            setHSBValue(value);
        } else {
            setHSLValue(value);
        }
    }

    protected void setHSBValue(final double value) {
        valueHSB = value;
    }

    protected void setHSLValue(final double value) {
        lightnessHSL = value;
    }

    protected void setSaturation(final double saturation) {
        if (isHSB) {
            setHSBSaturation(saturation);
        } else {
            setHSLSaturation(saturation);
        }
    }

    protected void setHSBSaturation(final double saturation) {
        saturationHSB = saturation;
    }

    protected void setHSLSaturation(final double saturation) {
        saturationHSL = saturation;
    }

    protected void setFromPickResult(final PickResult res, final boolean updateLastPick) {
        if (updateLastPick) {
            lastPick = res;
        }
        if (lastPick != null) {
            if (lastPick.area == PickArea.WHEEL) {
                setHue(res.hue);
                rotation = res.rotation;
                syncValues();
                invalidateWheel();
                fireColorChanged(this);
            } else if (lastPick.area == PickArea.TRIANGLE) {
                setValue(res.value);
                setSaturation(res.saturation);
                syncValues();
                invalidateWheel();
                fireColorChanged(this);
            }
        }
    }

    protected void setHSB(final double[] hsb) {
        setHSB(hsb[0], hsb[1], hsb[2]);
    }

    protected void setHSB(final double h, final double s, final double b) {
        setHSBHue(h);
        setHSBSaturation(s);
        setHSBValue(b);
    }

    protected void setHSL(final double[] hsl) {
        setHSL(hsl[0], hsl[1], hsl[2]);
    }

    protected void setHSL(final double h, final double s, final double l) {
        setHSLHue(h);
        setHSLSaturation(s);
        setHSLValue(l);
    }

    protected int getColorRGB(final double h, final double s, final double v) {
        if (isHSB) {
            return Color.HSBtoRGB((float) h, (float) s, (float) v);
        } else {
            return DarkColorModelHSL.getColorFromHSLValues(h, s, v).getRGB();
        }
    }

    public double getOpacity() {
        return opacity;
    }

    public void setOpacity(final double opacity) {
        this.opacity = opacity;
    }

    public int[] getValuesForModel(final DarkColorModel model) {
        if (model instanceof DarkColorModelHSB) {
            return new int[] {(int) Math.round(getHue() * model.getMaximum(0)),
                    (int) Math.round(getHSBSaturation() * model.getMaximum(1)),
                    (int) Math.round(getHSBValue() * model.getMaximum(2))};
        } else if (model instanceof DarkColorModelHSL) {
            return new int[] {(int) Math.round(getHue() * model.getMaximum(0)),
                    (int) Math.round(getHSLSaturation() * model.getMaximum(1)),
                    (int) Math.round(getHSLValue() * model.getMaximum(2))};
        } else {
            return model.getValuesFromColor(color);
        }
    }

    public void setColorFromModel(final Object source, final DarkColorModel model, final int[] values) {
        double x = values[0] / (double) model.getMaximum(0);
        double y = values[1] / (double) model.getMaximum(1);
        double z = values[2] / (double) model.getMaximum(2);
        if (model instanceof DarkColorModelHSB) {
            setColorFromHSB(source, x, y, z);
        } else if (model instanceof DarkColorModelHSL) {
            setColorFromHSL(source, x, y, z);
        } else if (model instanceof DarkColorModelRGB) {
            setColorFromRGB(source, values[0], values[1], values[2]);
        } else {
            setColor(source, model.getColorFromValues(values));
        }
    }

    public void setColor(final Object source, final Color color) {
        Color c = color != null ? color : Color.BLACK;
        setColorFromRGB(source, c.getRed(), c.getGreen(), c.getBlue());
    }

    protected void setColorFromRGB(final Object source, final int r, final int g, final int b) {
        if (isMessaging) return;
        this.color = new Color(r, g, b);
        double[] hsb = DarkColorModelHSB.RGBtoHSBValues(r, g, b);
        double[] hsl = DarkColorModelHSL.RGBtoHSLValues(r, g, b);
        if (Color.getHSBColor((float) getHSBHue(), (float) hsb[1], (float) hsb[2]).equals(color)) {
            hsb[0] = getHSBHue();
            hsl[0] = getHSLHue();
        }
        setHSL(hsl);
        setHSB(hsb);
        rotation = -(getHue() * 2 * Math.PI) + Math.PI / 2;
        invalidateWheel();
        fireColorChanged(source);
    }

    protected void setColorFromHSL(final Object source, final double h, final double s, final double l) {
        if (isMessaging) return;
        setHSL(h, s, l);
        color = DarkColorModelHSL.getColorFromHSLValues(h, s, l);
        setHSB(DarkColorModelHSB.RGBtoHSBValues(color.getRed(), color.getGreen(), color.getBlue()));
        setHSBHue(h);
        rotation = -(getHue() * 2 * Math.PI) + Math.PI / 2;
        invalidateWheel();
        fireColorChanged(source);
    }

    protected void setColorFromHSB(final Object source, final double h, final double s, final double b) {
        if (isMessaging) return;
        setHSB(h, s, b);
        color = DarkColorModelHSB.getColorFromHSBValues(h, s, b);
        setHSL(DarkColorModelHSL.RGBtoHSLValues(color.getRed(), color.getGreen(), color.getBlue()));
        setHSLHue(h);
        rotation = -(getHue() * 2 * Math.PI) + Math.PI / 2;
        invalidateWheel();
        fireColorChanged(source);
    }

    protected void invalidateWheel() {
        invalid = true;
        Component parent = getParent();
        if (parent != null) {
            parent.repaint();
        } else {
            repaint();
        }
    }

    protected void syncValues() {
        if (isHSB) {
            color = Color.getHSBColor((float) getHSBHue(), (float) getHSBSaturation(), (float) getHSBValue());
            setHSL(DarkColorModelHSL.RGBtoHSLValues(color.getRed(), color.getGreen(), color.getBlue()));
        } else {
            color = DarkColorModelHSL.getColorFromHSLValues(getHSLHue(), getHSLSaturation(), getHSLValue());
            setHSB(DarkColorModelHSB.RGBtoHSBValues(color.getRed(), color.getGreen(), color.getBlue()));
        }
    }

    protected void fireColorChanged(final Object source) {
        isMessaging = true;
        Color notifyColor = ColorUtil.toAlpha(color, opacity);
        for (ColorListener listener : myListeners) {
            listener.colorChanged(notifyColor, source);
        }
        isMessaging = false;
    }

    @Override
    public void updateUI() {
        super.updateUI();
        updateDefaults();
    }

    protected PickResult pick(final double x, final double y) {
        Point2D p = dummy;
        p.setLocation(x, y);
        double rotation = getRotation(x, y, centerX, centerY);
        double hue = 0.25 - rotation / (2.0 * Math.PI);
        triangleInverse.transform(p, p);
        Point2D sv = getSaturationAndValue(p.getX(), p.getY());
        PickArea area = PickArea.OUTSIDE;
        if (triangleShape.contains(x, y) || innerIndicator.contains(x, y)) {
            area = PickArea.TRIANGLE;
        } else if (circleShape.contains(x, y) || outerIndicator.contains(x, y)) {
            area = PickArea.WHEEL;
        }
        return new PickResult(area, rotation, hue, sv.getX(), sv.getY());
    }

    protected Point2D getSaturationAndValue(final double x, final double y) {
        double x1 = (x - centerX) / innerRadius;
        double y1 = (y - centerY) / innerRadius;
        double sat = (1.0 - 2.0 * y1) / (SQRT3 * x1 - y1 + 2.0);
        double val = (SQRT3 * x1 - y1 + 2.0) / 3.0;
        return new Point2D.Double(Math.max(Math.min(sat, 1), 0), Math.max(Math.min(val, 1), 0));
    }

    protected static double getRotation(final double x, final double y, final double cx, final double cy) {
        return Math.PI - Math.atan2(x - cx, y - cy);
    }

    protected static double getWheelHue(final double x, final double y, final double cx, final double cy) {
        return (Math.atan2(x - cx, y - cy) - Math.PI / 2.0) / (2 * Math.PI);
    }

    @Override
    public void paint(final Graphics g) {
        GraphicsContext context = GraphicsUtil.setupStrokePainting(g);
        Graphics2D g2d = (Graphics2D) g;

        final Dimension dim = getSize();
        int size = Math.min(dim.width, dim.height);
        size = Math.min(size, 600);
        float x = (float) ((dim.width - size) / 2.0);
        float y = (float) ((dim.height - size) / 2.0);
        centerX = x + size / 2.0;
        centerY = y + size / 2.0;

        if (invalid || circleShape == null || triangleShape == null || outerIndicator == null) {
            createShapes(x, y, size);
        }

        g2d.setComposite(COMPOSITE.derive((float) opacity));
        g2d.setPaint(new InnerPaint());
        g2d.fill(triangleShape);

        g2d.setPaint(new OuterPaint());
        g2d.fill(circleShape);
        context.restoreComposite();

        drawIndicator(g2d, outerIndicator);
        drawIndicator(g2d, innerIndicator);
    }

    public void createShapes(final float x, final float y, final int size) {
        int outerSize = 15;
        AffineTransform rotationTransform = AffineTransform.getRotateInstance(rotation, centerX, centerY);

        circleShape = calculateCircleShape(x, y, size, outerSize);
        triangleShape = calculateTriangleShape(x, y, size, outerSize, rotationTransform);
        outerIndicator = createOuterIndicator(centerX, centerY, (innerRadius + outerRadius) / 2.0, rotationTransform,
                outerIndicatorRadius);
        innerIndicator = createInnerIndicator(rotationTransform, innerIndicatorRadius);

        invalid = false;
        try {
            // For calculating pick location. MouseEvents already respect the ui scaling.
            triangleInverse = rotationTransform.createInverse();
        } catch (final NoninvertibleTransformException e) {
            e.printStackTrace();
        }
    }

    protected void drawIndicator(final Graphics2D g, final Shape indicator) {
        Stroke old = g.getStroke();

        g.setStroke(new BasicStroke(3));
        g.setColor(dropBorder);
        g.draw(indicator);

        g.setStroke(old);
        g.setColor(dropFill);
        g.draw(indicator);
    }

    protected Shape createInnerIndicator(final AffineTransform transform, final int dotRadius) {
        Point2D p = getTrianglePos(getSaturation(), getValue());
        transform.transform(p, p);
        return createIndicatorShape(p, dotRadius);
    }

    protected Shape createOuterIndicator(final double cx, final double cy, final double radius,
            final AffineTransform transform, final int dotRadius) {
        dummy.setLocation(cx, cy - radius);
        transform.transform(dummy, dummy);
        return createIndicatorShape(dummy, dotRadius);
    }

    protected Shape createIndicatorShape(final Point2D p, final int radius) {
        return new Ellipse2D.Double(p.getX() - radius, p.getY() - radius, 2 * radius, 2 * radius);
    }

    protected Shape calculateTriangleShape(final double x, final double y, final int size, final int outerSize,
            final AffineTransform transform) {
        double diameter = (size - 2 * outerSize);
        double radius = diameter / 2.0;
        double sideLength = Math.cos(Math.PI / 6.0) * diameter;
        double height = (SQRT3 / 2.0) * sideLength;

        double upperX = radius + x + outerSize;
        double upperY = y + outerSize;

        Path2D trianglePath = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        trianglePath.moveTo(upperX, upperY);
        trianglePath.lineTo(upperX - sideLength / 2.0, upperY + height);
        trianglePath.lineTo(upperX + sideLength / 2.0, upperY + height);
        trianglePath.closePath();
        trianglePath.transform(transform);
        return trianglePath;
    }

    protected Shape calculateCircleShape(final double x, final double y, final int size, final int outerSize) {
        outerRadius = size / 2.0;
        innerRadius = outerRadius - outerSize;
        if (!circleInfo.update(x, y, size, outerSize) && circleShape != null) return circleShape;

        Area outer = new Area(new Ellipse2D.Double(x, y, size, size));
        Area inner = new Area(
                new Ellipse2D.Double(x + outerSize, y + outerSize, size - 2 * outerSize, size - 2 * outerSize));
        outer.subtract(inner);
        return outer;
    }

    protected Point2D getTrianglePos(final double sat, final double val) {
        return new Point2D.Double(centerX + innerRadius * (2 * val - sat * val - 1) * SQRT3 / 2.0,
                centerY + innerRadius * (1 - 3 * sat * val) / 2.0);
    }

    @Override
    public Dimension getPreferredSize() {
        return getMinimumSize();
    }

    @Override
    public Dimension getMinimumSize() {
        return new Dimension(minSize, minSize);
    }

    public void setMinimumSize(final int size) {
        minSize = size;
    }

    public void addListener(final ColorListener listener) {
        myListeners.add(listener);
    }

    public Color getColor() {
        return ColorUtil.toAlpha(color, opacity);
    }

    public void setColorModel(final DarkColorModel model) {
        boolean hsb = !(model instanceof DarkColorModelHSL);
        if (isHSB != hsb) {
            isHSB = hsb;
            invalidateWheel();
        }
    }

    public abstract static class ColorWheelPaint implements Paint {

        protected final ColorTriangle.ColorWheelPaintContext context;

        protected ColorWheelPaint(final ColorTriangle.ColorWheelPaintContext context) {
            this.context = context;
        }

        @Override
        public PaintContext createContext(final ColorModel cm, final Rectangle deviceBounds,
                final Rectangle2D userBounds, final AffineTransform xform, final RenderingHints hints) {
            context.setHints(deviceBounds, xform);
            return context;
        }

        @Override
        public int getTransparency() {
            return Transparency.OPAQUE;
        }
    }

    protected class InnerPaint extends ColorWheelPaint {

        protected InnerPaint() {
            super(new InnerPaintContext());
        }
    }

    protected static class OuterPaint extends ColorWheelPaint {
        protected OuterPaint() {
            super(new ColorTriangle.OuterPaintContext());
        }
    }

    protected abstract static class ColorWheelPaintContext implements PaintContext {
        protected Rectangle deviceBounds;
        protected double cx;
        protected double cy;
        protected AffineTransform transform;

        public void setHints(final Rectangle deviceBounds, final AffineTransform transform) {
            this.deviceBounds = deviceBounds;
            cx = deviceBounds.x + deviceBounds.width / 2.0;
            cy = deviceBounds.y + deviceBounds.height / 2.0;
            this.transform = transform;
        }

        @Override
        public void dispose() {}

        @Override
        public ColorModel getColorModel() {
            return ColorModel.getRGBdefault();
        }

        protected void setPixel(final WritableRaster raster, final int i, final int j, final int rgb) {
            setPixel(raster, i, j, new int[] {(rgb >> 16) & 0xFF, (rgb >> 8) & 0xFF, (rgb) & 0xFF, 255});
        }

        protected void setPixel(final WritableRaster raster, final int i, final int j, final int[] vals) {
            raster.setPixel(i, j, vals);
        }
    }

    protected static class OuterPaintContext extends ColorWheelPaintContext {

        @Override
        public Raster getRaster(final int x, final int y, final int w, final int h) {
            WritableRaster raster = getColorModel().createCompatibleWritableRaster(w, h);
            for (int j = 0; j < h; j++) {
                for (int i = 0; i < w; i++) {
                    float hue = (float) ((getWheelHue(x + i, y + j, cx, cy)));
                    int rgb = Color.HSBtoRGB(hue, 1.0f, 1.0f);
                    setPixel(raster, i, j, rgb);
                }
            }
            return raster;
        }
    }

    protected class InnerPaintContext extends ColorWheelPaintContext {

        public InnerPaintContext() {}

        @Override
        public Raster getRaster(final int x, final int y, final int w, final int h) {
            WritableRaster raster = getColorModel().createCompatibleWritableRaster(w, h);
            for (int j = 0; j < h; j++) {
                for (int i = 0; i < w; i++) {
                    dummy.setLocation((x + i), (y + j));
                    try {
                        transform.inverseTransform(dummy, dummy);
                        triangleInverse.transform(dummy, dummy);
                        Point2D sv = getSaturationAndValue(dummy.getX(), dummy.getY());
                        setPixel(raster, i, j, getColorRGB(getHue(), sv.getX(), sv.getY()));
                    } catch (final NoninvertibleTransformException ignored) {
                    }
                }
            }
            return raster;
        }
    }

    protected static class PickResult {
        protected final PickArea area;
        protected final double rotation;
        protected final double saturation;
        protected final double value;
        protected final double hue;

        public PickResult(final PickArea area, final double rotation, final double hue, final double saturation,
                final double value) {
            this.area = area;
            this.hue = hue;
            this.rotation = rotation;
            this.saturation = saturation;
            this.value = value;
        }
    }

    protected enum PickArea {
        OUTSIDE,
        WHEEL,
        TRIANGLE
    }

    protected static class CircleInfo {
        public double x;
        public double y;
        public int size;
        public int outerSize;

        public boolean update(final double x, final double y, final int size, final int outerSize) {
            boolean updated = this.x != x || this.y != y || this.size != size || this.outerSize != outerSize;
            this.x = x;
            this.y = y;
            this.size = size;
            this.outerSize = outerSize;
            return updated;
        }
    }
}
