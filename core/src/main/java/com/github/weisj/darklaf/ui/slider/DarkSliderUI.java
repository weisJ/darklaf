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
package com.github.weisj.darklaf.ui.slider;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Path2D;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Dictionary;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSliderUI;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.listener.MouseClickListener;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public class DarkSliderUI extends BasicSliderUI implements PropertyChangeListener {

    protected static final String KEY_PREFIX = "JSlider.";
    public static final String KEY_THUMB_ARROW_SHAPE = KEY_PREFIX + "paintThumbArrowShape";
    public static final String KEY_SHOW_VOLUME_ICON = KEY_PREFIX + "volume.showIcon";
    public static final String KEY_VARIANT = KEY_PREFIX + "variant";
    public static final String KEY_INSTANT_SCROLL = KEY_PREFIX + "instantScrollEnabled";
    public static final String KEY_SHOW_FOCUS_GLOW = KEY_PREFIX + "paintFocusGlow";
    public static final String KEY_MANUAL_LABEL_ALIGN = KEY_PREFIX + "manualLabelAlign";
    public static final String VARIANT_VOLUME = "volume";

    private static final int ICON_BAR_EXT = 5;
    private static final int ICON_PAD = 10;
    private final Rectangle iconRect = new Rectangle(0, 0, 0, 0);
    private final MouseListener mouseListener = new MouseClickListener() {
        private boolean muted = false;
        private int oldValue;

        @Override
        public void mouseClicked(final MouseEvent e) {
            if (slider.isEnabled() && showVolumeIcon(slider) && iconRect.contains(e.getPoint())) {
                if (muted && slider.getValue() == slider.getMinimum()) {
                    setValue(oldValue);
                    muted = false;
                } else {
                    oldValue = slider.getValue();
                    setValue(slider.getMinimum());
                    muted = true;
                }
            }
        }
    };
    protected int plainThumbRadius;
    protected int arcSize;
    protected int trackSize;
    protected Dimension thumbSize;
    protected Color inactiveTickForeground;
    protected Color trackBackground;
    protected Color selectedTrackBackground;
    protected Color selectedTrackInactiveBackground;
    protected Color selectedVolumeTrackBackground;
    protected Color selectedVolumeTrackInactiveBackground;
    protected Color thumbBackground;
    protected Color thumbInactiveBackground;
    protected Color volumeThumbBackground;
    protected Color volumeThumbInactiveBackground;
    protected Color thumbBorderColor;
    protected Color thumbInactiveBorderColor;

    protected Icon volume0;
    protected Icon volume1;
    protected Icon volume2;
    protected Icon volume3;
    protected Icon volume4;
    protected Icon volume0Inactive;
    protected Icon volume1Inactive;
    protected Icon volume2Inactive;
    protected Icon volume3Inactive;
    protected Icon volume4Inactive;

    protected int focusBorderSize;
    protected RoundRectangle2D trackShape = new RoundRectangle2D.Double();

    public DarkSliderUI(final JSlider b) {
        super(b);
    }

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSliderUI((JSlider) c);
    }

    private static boolean showVolumeIcon(final JComponent c) {
        return isVolumeSlider(c) && PropertyUtil.getBooleanProperty(c, KEY_SHOW_VOLUME_ICON);
    }

    private static boolean isVolumeSlider(final JComponent c) {
        return PropertyUtil.isPropertyEqual(c, KEY_VARIANT, VARIANT_VOLUME);
    }

    @Override
    protected TrackListener createTrackListener(final JSlider slider) {
        return new SnapTrackListener();
    }

    @Override
    protected void installListeners(final JSlider slider) {
        super.installListeners(slider);
        slider.addMouseListener(mouseListener);
        slider.addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners(final JSlider slider) {
        super.uninstallListeners(slider);
        slider.removeMouseListener(mouseListener);
        slider.removePropertyChangeListener(this);
    }

    @Override
    protected void calculateGeometry() {
        super.calculateGeometry();
        if (showVolumeIcon(slider)) {
            calculateIconRect();
        } else {
            iconRect.setBounds(-1, -1, 0, 0);
        }
    }

    @Override
    protected void calculateContentRect() {
        super.calculateContentRect();
        if (showVolumeIcon(slider)) {
            if (isHorizontal()) {
                contentRect.width -= getVolumeIcon().getIconWidth() + ICON_PAD;
                if (!slider.getComponentOrientation().isLeftToRight()) {
                    contentRect.x += getVolumeIcon().getIconWidth() + ICON_PAD;
                }
            } else {
                contentRect.height -= getVolumeIcon().getIconHeight() + ICON_PAD;
                if (!slider.getComponentOrientation().isLeftToRight()) {
                    contentRect.y += getVolumeIcon().getIconHeight() + ICON_PAD;
                }
            }
        }
    }

    @Override
    protected void calculateTrackBuffer() {
        super.calculateTrackBuffer();
        if (slider.getOrientation() == JSlider.HORIZONTAL) {
            trackBuffer = 0;
        }
    }

    @Override
    protected void calculateTrackRect() {
        super.calculateTrackRect();
        adjustRect(trackRect);
    }

    @Override
    protected void calculateLabelRect() {
        super.calculateLabelRect();
    }

    protected void calculateThumbLocation() {
        if (slider.getOrientation() == JSlider.HORIZONTAL) {
            int valuePosition = xPositionForValue(slider.getValue());

            thumbRect.x = valuePosition - (thumbRect.width / 2);
            thumbRect.y = trackRect.y;
        } else {
            int valuePosition = yPositionForValue(slider.getValue());

            thumbRect.x = trackRect.x;
            thumbRect.y = valuePosition - (thumbRect.height / 2);
        }
    }

    @Override
    public int xPositionForValue(final int value) {
        return super.xPositionForValue(value);
    }

    @Override
    public int yPositionForValue(final int value) {
        return super.yPositionForValue(value);
    }

    /**
     * Scrolls by block.
     *
     * @param direction the direction
     */
    public void scrollByBlock(final int direction) {
        synchronized (slider) {
            int blockIncrement = (slider.getMaximum() - slider.getMinimum()) / 10;
            if (blockIncrement == 0) {
                blockIncrement = 1;
            }

            int tickSpacing = getTickSpacing();
            if (slider.getSnapToTicks()) {

                if (blockIncrement < tickSpacing) {
                    blockIncrement = tickSpacing;
                }
            } else {
                if (tickSpacing > 0) {
                    blockIncrement = tickSpacing;
                }
            }

            int delta = blockIncrement * ((direction > 0) ? POSITIVE_SCROLL : NEGATIVE_SCROLL);
            applyDelta(delta);
        }
    }

    /**
     * Scrolls by unit.
     *
     * @param direction the direction
     */
    public void scrollByUnit(final int direction) {
        synchronized (slider) {
            int delta = ((direction > 0) ? POSITIVE_SCROLL : NEGATIVE_SCROLL);

            if (slider.getSnapToTicks()) {
                delta *= getTickSpacing();
            }

            applyDelta(delta);
        }
    }

    protected void applyDelta(final int delta) {
        setValue(slider.getValue() + delta);
    }

    protected void setValue(final int value) {
        if (slider.getSnapToTicks()) {
            slider.setValue(getSnappedValue(value));
        } else {
            slider.setValue(value);
        }
    }

    protected int getTickSpacing() {
        int majorTickSpacing = slider.getMajorTickSpacing();
        int minorTickSpacing = slider.getMinorTickSpacing();

        int result;
        if (minorTickSpacing > 0) {
            result = minorTickSpacing;
        } else {
            result = Math.max(majorTickSpacing, 0);
        }
        return result;
    }

    protected void adjustRect(final Rectangle rectangle) {
        adjustRect(rectangle, false);
    }

    protected void adjustRect(final Rectangle rectangle, final boolean outwards) {
        boolean horizontal = slider.getOrientation() == JSlider.HORIZONTAL;

        if (horizontal) {
            boolean ltr = slider.getComponentOrientation().isLeftToRight();
            int left = ltr ? focusInsets.left : focusInsets.right;
            int right = ltr ? focusInsets.right : focusInsets.left;
            rectangle.x += left;
            rectangle.width -= left + right;
        } else {
            rectangle.y += focusInsets.top;
            rectangle.height -= focusInsets.top + focusInsets.bottom;
        }

        Dictionary<?, ?> dict = slider.getLabelTable();
        if (!slider.getPaintLabels() || dict == null || dict.isEmpty() || !horizontal) {
            return;
        }

        int extra = getLowerHorizontalExtend();
        int extend = getUpperHorizontalExtend();
        int thumbWidth = getThumbWidth() / 2;
        int factor = outwards ? 1 : -1;
        extra -= thumbWidth;
        extend -= thumbWidth;
        rectangle.x -= factor * extra;
        rectangle.width += factor * (extra + extend);
    }

    protected int getLowerHorizontalExtend() {
        boolean ltr = (slider.getComponentOrientation().isLeftToRight() && !slider.getInverted())
                      || (!slider.getComponentOrientation().isLeftToRight() && slider.getInverted());
        Component minLabel = ltr ? getLowestValueLabel() : getHighestValueLabel();
        boolean adjustMin = PropertyUtil.getBooleanProperty(minLabel, KEY_MANUAL_LABEL_ALIGN);
        int minPrefWidth = minLabel.getPreferredSize().width;
        float adj = (adjustMin ? minLabel.getAlignmentX() : Component.CENTER_ALIGNMENT);
        return (int) (minPrefWidth * adj) + (ltr ? focusInsets.left : focusInsets.right);
    }

    protected int getUpperHorizontalExtend() {
        boolean ltr = (slider.getComponentOrientation().isLeftToRight() && !slider.getInverted())
                      || (!slider.getComponentOrientation().isLeftToRight() && slider.getInverted());
        Component maxLabel = ltr ? getHighestValueLabel() : getLowestValueLabel();
        boolean adjustMax = PropertyUtil.getBooleanProperty(maxLabel, KEY_MANUAL_LABEL_ALIGN);
        int maxPrefWidth = maxLabel.getPreferredSize().width;
        float adj = (adjustMax ? maxLabel.getAlignmentX() : Component.CENTER_ALIGNMENT);
        return (int) (maxPrefWidth * (1f - adj)) + (ltr ? focusInsets.right : focusInsets.left);
    }

    @Override
    public Dimension getPreferredHorizontalSize() {
        Dimension dim = super.getPreferredHorizontalSize();
        Rectangle rect = new Rectangle(0, 0, 0, 0);
        rect.setSize(dim);
        adjustRect(rect, true);
        return rect.getSize();
    }

    @Override
    public Dimension getThumbSize() {
        if (isPlainThumb()) {
            return new Dimension(plainThumbRadius + 2 * focusBorderSize,
                                 plainThumbRadius + 2 * focusBorderSize);
        }
        return isHorizontal()
                ? new Dimension(thumbSize.width, thumbSize.height)
                : new Dimension(thumbSize.height, thumbSize.width);
    }

    @Override
    public void paint(final Graphics g2, final JComponent c) {
        super.paint(g2, c);
        if (showVolumeIcon(c)) {
            getVolumeIcon().paintIcon(c, g2, iconRect.x, iconRect.y);
        }
    }

    @Override
    public void paintFocus(final Graphics g2) {
        // Do nothing
    }

    @Override
    public void paintTrack(final Graphics g2d) {
        Graphics2D g = (Graphics2D) g2d;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);

        Color bgColor = getTrackBackground();
        Color selectionColor = getSelectedTrackColor();

        if (isHorizontal()) {
            Shape track = getHorizontalTrackShape(trackShape);
            g.setColor(bgColor);
            g.fill(track);
            setHorizontalTrackClip(g);
            g.setColor(selectionColor);
            g.fill(track);
        } else {
            Shape track = getVerticalTrackShape(trackShape);
            g.setColor(bgColor);
            g.fill(track);
            setVerticalTrackClip(g);
            g.setColor(selectionColor);
            g.fill(track);
        }
        config.restore();
    }

    @Override
    protected void paintMinorTickForHorizSlider(final Graphics g, final Rectangle tickBounds, final int x) {
        checkDisabled(g);
        super.paintMinorTickForHorizSlider(g, tickBounds, x);
    }

    @Override
    protected void paintMajorTickForHorizSlider(final Graphics g, final Rectangle tickBounds, final int x) {
        checkDisabled(g);
        super.paintMajorTickForHorizSlider(g, tickBounds, x);
    }

    @Override
    protected void paintMinorTickForVertSlider(final Graphics g, final Rectangle tickBounds, final int y) {
        checkDisabled(g);
        super.paintMinorTickForVertSlider(g, tickBounds, y);
    }

    @Override
    protected void paintMajorTickForVertSlider(final Graphics g, final Rectangle tickBounds, final int y) {
        checkDisabled(g);
        super.paintMajorTickForVertSlider(g, tickBounds, y);
    }

    @Override
    public void paintLabels(final Graphics g) {
        checkDisabled(g);
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        super.paintLabels(g);
        config.restore();
    }

    @Override
    protected void paintHorizontalLabel(final Graphics g, final int value, final Component label) {
        checkDisabled(g);
        int labelCenter = xPositionForValue(value);
        float align = Component.CENTER_ALIGNMENT;
        if (PropertyUtil.getBooleanProperty(label, KEY_MANUAL_LABEL_ALIGN)) {
            align = label.getAlignmentX();
        }
        int labelLeft = labelCenter - (int) (label.getPreferredSize().width * align);
        g.translate(labelLeft, 0);
        label.paint(g);
        g.translate(-labelLeft, 0);
    }

    @Override
    protected void paintVerticalLabel(final Graphics g, final int value, final Component label) {
        checkDisabled(g);
        super.paintVerticalLabel(g, value, label);
    }

    @Override
    public void paintThumb(final Graphics g2) {
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext context = GraphicsUtil.setupStrokePainting(g);
        g.translate(thumbRect.x, thumbRect.y);

        if (isPlainThumb()) {
            paintPlainSliderThumb(g);
        } else {
            paintSliderThumb(g);
        }

        g.translate(-thumbRect.x, -thumbRect.y);
        context.restore();
    }

    @Override
    protected void scrollDueToClickInTrack(final int dir) {
        Point p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, slider);
        Shape area = isHorizontal() ? getHorizontalTrackShape(trackShape) : getVerticalTrackShape(trackShape);
        if (!area.getBounds().contains(p)) {
            return;
        }
        if (instantScrollEnabled(slider)) {
            int value = isHorizontal() ? valueForXPosition(p.x) : valueForYPosition(p.y);
            setValue(value);
        } else {
            super.scrollDueToClickInTrack(dir);
        }
    }

    private void checkDisabled(final Graphics g) {
        if (!slider.isEnabled()) {
            g.setColor(getDisabledTickColor());
        }
    }

    protected Color getDisabledTickColor() {
        return inactiveTickForeground;
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        slider.putClientProperty(KEY_SHOW_FOCUS_GLOW, UIManager.getBoolean("Slider.paintFocusGlow"));
    }

    @Override
    protected void installDefaults(final JSlider slider) {
        super.installDefaults(slider);
        LookAndFeel.installProperty(slider, PropertyKey.OPAQUE, false);
        arcSize = UIManager.getInt("Slider.arc");
        trackSize = UIManager.getInt("Slider.trackThickness");
        plainThumbRadius = UIManager.getInt("Slider.plainThumbRadius");
        thumbSize = UIManager.getDimension("Slider.thumbSize");
        inactiveTickForeground = UIManager.getColor("Slider.disabledTickColor");
        trackBackground = UIManager.getColor("Slider.trackBackground");
        selectedTrackBackground = UIManager.getColor("Slider.selectedTrackColor");
        selectedTrackInactiveBackground = UIManager.getColor("Slider.disabledTrackColor");
        selectedVolumeTrackBackground = UIManager.getColor("Slider.volume.selectedTrackColor");
        selectedVolumeTrackInactiveBackground = UIManager.getColor("Slider.volume.disabledTrackColor");
        thumbBackground = UIManager.getColor("Slider.activeThumbFill");
        thumbInactiveBackground = UIManager.getColor("Slider.inactiveThumbFill");
        volumeThumbBackground = UIManager.getColor("Slider.volume.activeThumbFill");
        volumeThumbInactiveBackground = UIManager.getColor("Slider.volume.inactiveThumbFill");
        thumbBorderColor = UIManager.getColor("Slider.thumbBorderColor");
        thumbInactiveBorderColor = UIManager.getColor("Slider.thumbBorderColorDisabled");
        focusBorderSize = UIManager.getInt("Slider.focusBorderSize");

        volume0 = UIManager.getIcon("Slider.volume.enabled_level_0.icon");
        volume1 = UIManager.getIcon("Slider.volume.enabled_level_1.icon");
        volume2 = UIManager.getIcon("Slider.volume.enabled_level_2.icon");
        volume3 = UIManager.getIcon("Slider.volume.enabled_level_3.icon");
        volume4 = UIManager.getIcon("Slider.volume.enabled_level_4.icon");
        volume0Inactive = UIManager.getIcon("Slider.volume.disabled_level_0.icon");
        volume1Inactive = UIManager.getIcon("Slider.volume.disabled_level_1.icon");
        volume2Inactive = UIManager.getIcon("Slider.volume.disabled_level_2.icon");
        volume3Inactive = UIManager.getIcon("Slider.volume.disabled_level_3.icon");
        volume4Inactive = UIManager.getIcon("Slider.volume.disabled_level_4.icon");
    }

    protected void calculateIconRect() {
        iconRect.width = getVolumeIcon().getIconWidth();
        iconRect.height = getVolumeIcon().getIconHeight();
        if (isHorizontal()) {
            if (slider.getComponentOrientation().isLeftToRight()) {
                iconRect.x = trackRect.x + trackRect.width + ICON_PAD;
                iconRect.y = trackRect.y + (trackRect.height - iconRect.height) / 2;
            } else {
                iconRect.x = trackRect.x - iconRect.width - ICON_PAD;
                iconRect.y = trackRect.y + (trackRect.height - iconRect.height) / 2;
            }
        } else {
            if (slider.getComponentOrientation().isLeftToRight()) {
                iconRect.x = trackRect.x + (trackRect.width - iconRect.width) / 2;
                iconRect.y = trackRect.y + trackRect.height + ICON_PAD;
            } else {
                iconRect.x = trackRect.x + (trackRect.width - iconRect.width) / 2;
                iconRect.y = trackRect.y - iconRect.height - ICON_PAD;
            }
        }
    }

    private void setHorizontalTrackClip(final Graphics g) {
        int x = thumbRect.x + thumbRect.width / 2;
        boolean ltr = slider.getComponentOrientation().isLeftToRight();
        boolean inverted = slider.getInverted();
        if ((ltr && !inverted) || (!ltr && inverted)) {
            g.clipRect(0, 0, x, slider.getHeight());
        } else {
            g.clipRect(x, 0, slider.getWidth() - x, slider.getHeight());
        }
    }

    protected Icon getVolumeIcon() {
        int range = slider.getMaximum() - slider.getMinimum();
        int value = slider.getValue() - slider.getMinimum();
        double percentage = value / (double) range;
        boolean enabled = slider.isEnabled();
        if (Math.abs(percentage) < 1E-6) {
            return enabled ? volume0 : volume0Inactive;
        } else if (percentage < 0.25) {
            return enabled ? volume1 : volume1Inactive;
        } else if (percentage < 0.5) {
            return enabled ? volume2 : volume2Inactive;
        } else if (percentage < 0.75) {
            return enabled ? volume3 : volume3Inactive;
        } else {
            return enabled ? volume4 : volume4Inactive;
        }
    }

    private boolean isHorizontal() {
        return slider.getOrientation() == JSlider.HORIZONTAL;
    }

    private Shape getHorizontalTrackShape(final RoundRectangle2D trackShape) {
        int arc = arcSize;
        int yOff = (trackRect.height / 2) - trackSize / 2;
        int w = showVolumeIcon(slider) ? trackRect.width + getIconBarExt() : trackRect.width;
        if (slider.getComponentOrientation().isLeftToRight()) {
            trackShape.setRoundRect(trackRect.x, trackRect.y + yOff, w, trackSize, arc, arc);
        } else {
            trackShape.setRoundRect(trackRect.x - getIconBarExt(), trackRect.y + yOff, w, trackSize, arc, arc);
        }
        return trackShape;
    }

    private void setVerticalTrackClip(final Graphics g) {
        int y = thumbRect.y + thumbRect.height / 2;
        if (slider.getInverted()) {
            g.clipRect(0, 0, slider.getWidth(), y);
        } else {
            g.clipRect(0, y, slider.getWidth(), slider.getHeight() - y);
        }
    }

    private Shape getVerticalTrackShape(final RoundRectangle2D trackShape) {
        int arc = arcSize;
        int xOff = (trackRect.width / 2) - trackSize / 2;
        int h = showVolumeIcon(slider) ? trackRect.height + getIconBarExt() : trackRect.height;
        if (slider.getComponentOrientation().isLeftToRight()) {
            trackShape.setRoundRect(trackRect.x + xOff, trackRect.y, trackSize, h, arc, arc);
        } else {
            trackShape.setRoundRect(trackRect.x + xOff, trackRect.y - getIconBarExt(), trackSize, h, arc, arc);
        }
        return trackShape;
    }

    private int getIconBarExt() {
        return isPlainThumb() && showVolumeIcon(slider) ? ICON_BAR_EXT : 0;
    }

    private void paintPlainSliderThumb(final Graphics2D g) {
        int r = plainThumbRadius;
        int bw = focusBorderSize;
        g.setColor(getThumbColor());
        PaintUtil.fillRoundRect(g, bw, bw, r, r, r);
        if (!isVolumeSlider(slider)) {
            g.setColor(getThumbBorderColor());
            PaintUtil.paintLineBorder(g, bw, bw, r, r, r);
        }
        if (paintFocus()) {
            PaintUtil.paintFocusBorder(g, r + 2 * bw, r + 2 * bw, r + 2 * bw, bw);
        }
    }

    protected boolean isPlainThumb() {
        return !slider.getPaintTicks() || !PropertyUtil.getBooleanProperty(slider, KEY_THUMB_ARROW_SHAPE, true);
    }

    private void paintSliderThumb(final Graphics2D g) {
        Path2D thumb = getThumbShape();
        if (paintFocus()) {
            GraphicsContext config = new GraphicsContext(g);
            g.setComposite(PaintUtil.getGlowComposite());
            g.setStroke(new BasicStroke(4f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND, 8));
            PaintUtil.Outline.focus.setGraphicsColor(g, true);
            g.draw(thumb);
            config.restore();
        }
        g.setColor(getThumbColor());
        g.fill(thumb);
        g.setColor(getThumbBorderColor());
        g.draw(thumb);
    }

    private boolean paintFocus() {
        return slider.hasFocus() && PropertyUtil.getBooleanProperty(slider, KEY_SHOW_FOCUS_GLOW);
    }

    protected int getThumbWidth() {
        return thumbRect.width;
    }

    protected int getThumbHeight() {
        return thumbRect.height;
    }

    private Path2D getThumbShape() {
        if (isHorizontal()) {
            return getHorizontalThumbShape();
        } else if (slider.getComponentOrientation().isLeftToRight()) {
            return getVerticalThumbShapeLR();
        } else {
            return getVerticalThumbShapeRL();
        }
    }

    private Path2D getHorizontalThumbShape() {
        int w = thumbRect.width;
        int h = thumbRect.height;
        int cw = w / 2;
        Path2D shape = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        shape.moveTo(1, 1);
        shape.lineTo(w - 2, 1);
        shape.lineTo(w - 2, h - cw - 1);
        shape.lineTo(cw, h - 2);
        shape.lineTo(1, h - cw - 1);
        shape.closePath();
        return shape;
    }

    private Path2D getVerticalThumbShapeLR() {
        int w = thumbRect.width;
        int h = thumbRect.height;
        int cw = h / 2;
        Path2D shape = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        shape.moveTo(2, 1);
        shape.lineTo(w - cw - 1, 1);
        shape.lineTo(w - 1, h - cw);
        shape.lineTo(w - cw - 1, h - 2);
        shape.lineTo(2, h - 2);
        shape.closePath();
        return shape;
    }

    private Path2D getVerticalThumbShapeRL() {
        int w = thumbRect.width;
        int h = thumbRect.height;
        int cw = h / 2;
        Path2D shape = new Path2D.Float(Path2D.WIND_EVEN_ODD);
        shape.moveTo(w - 2, 1);
        shape.lineTo(cw + 1, 1);
        shape.lineTo(1, h - cw);
        shape.lineTo(cw + 1, h - 2);
        shape.lineTo(w - 2, h - 2);
        shape.closePath();
        return shape;
    }

    protected Color getThumbColor() {
        if (isVolumeSlider(slider)) {
            return slider.isEnabled() ? volumeThumbBackground : volumeThumbInactiveBackground;
        } else {
            return slider.isEnabled() ? thumbBackground : thumbInactiveBackground;
        }
    }

    protected Color getThumbBorderColor() {
        return slider.isEnabled() ? thumbBorderColor : thumbInactiveBorderColor;
    }

    protected Color getTrackBackground() {
        return trackBackground;
    }

    protected Color getSelectedTrackColor() {
        if (isVolumeSlider(slider)) {
            return slider.isEnabled() ? selectedVolumeTrackBackground : selectedVolumeTrackInactiveBackground;
        } else {
            return slider.isEnabled() ? selectedTrackBackground : selectedTrackInactiveBackground;
        }
    }

    private boolean instantScrollEnabled(final JComponent c) {
        return PropertyUtil.getBooleanProperty(c, KEY_INSTANT_SCROLL);
    }

    public Rectangle getThumbRect() {
        return new Rectangle(thumbRect);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (KEY_VARIANT.equals(key)) {
            slider.repaint();
        } else if (DarkSliderUI.KEY_SHOW_VOLUME_ICON.equals(key)) {
            calculateGeometry();
            slider.repaint();
        }
    }

    private int getSnappedValue(final int value) {
        // Now calculate if we should adjust the value
        int snappedValue = value;
        int tickSpacing = getTickSpacing();
        // If it's not on a tick, change the value
        if (tickSpacing != 0) {
            if ((value - slider.getMinimum()) % tickSpacing != 0) {
                float temp = (float) (value - slider.getMinimum())
                             / (float) tickSpacing;
                snappedValue = slider.getMinimum() +
                               (Math.round(temp) * tickSpacing);
            }
        }
        return snappedValue;
    }

    public class SnapTrackListener extends TrackListener {
        private int offset;

        public void mousePressed(final MouseEvent evt) {
            int pos = isHorizontal() ? evt.getX() : evt.getY();
            int loc = getLocationForValue(getSnappedValue(evt));
            offset = (loc < 0) ? 0 : pos - loc;
            if (iconRect.contains(evt.getPoint())) return;
            super.mousePressed(evt);
        }

        @Override
        public void mouseDragged(final MouseEvent e) {
            if (slider.getSnapToTicks()) {
                int pos = getLocationForValue(getSnappedValue(e));
                if (isHorizontal()) {
                    e.translatePoint(pos - e.getX() + offset, 0);
                } else {
                    e.translatePoint(0, pos - e.getY() + offset);
                }
            }
            super.mouseDragged(e);
            slider.repaint();
        }

        private int getLocationForValue(final int value) {
            return isHorizontal() ? xPositionForValue(value) : yPositionForValue(value);
        }

        private int getSnappedValue(final MouseEvent e) {
            int value = isHorizontal() ? valueForXPosition(e.getX())
                    : valueForYPosition(e.getY());
            return DarkSliderUI.this.getSnappedValue(value);
        }
    }
}
