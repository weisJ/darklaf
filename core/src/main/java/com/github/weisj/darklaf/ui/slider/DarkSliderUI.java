/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.slider;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.geom.RoundRectangle2D;
import java.util.Dictionary;
import java.util.Enumeration;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSliderUI;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.ui.VisualPaddingProvider;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/** @author Jannis Weis */
public class DarkSliderUI extends BasicSliderUI {

    protected static final String KEY_PREFIX = "JSlider.";
    public static final String KEY_THUMB_ARROW_SHAPE = KEY_PREFIX + "paintThumbArrowShape";
    public static final String KEY_SHOW_VOLUME_ICON = KEY_PREFIX + "volume.showIcon";
    public static final String KEY_VARIANT = KEY_PREFIX + "variant";
    public static final String KEY_INSTANT_SCROLL = KEY_PREFIX + "instantScrollEnabled";
    public static final String KEY_MANUAL_LABEL_ALIGN = KEY_PREFIX + "manualLabelAlign";
    public static final String VARIANT_VOLUME = "volume";

    protected final Rectangle iconRect = new Rectangle(0, 0, 0, 0);
    private DarkSliderListener sliderListener;

    protected int plainThumbRadius;
    protected int arcSize;
    protected int trackSize;
    protected int iconPad;
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
    protected Color thumbFocusBorderColor;
    protected Color thumbInactiveBorderColor;

    protected RotatableIcon rotatableIcon;

    protected Icon thumb;
    protected Icon thumbDisabled;
    protected Icon thumbFocused;

    protected Icon volumeThumb;
    protected Icon volumeThumbDisabled;
    protected Icon volumeThumbFocused;

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
    protected boolean paintFocus;
    protected RoundRectangle2D trackShape = new RoundRectangle2D.Double();

    public DarkSliderUI(final JSlider b) {
        super(b);
    }

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSliderUI((JSlider) c);
    }

    protected boolean showVolumeIcon(final JComponent c) {
        return PropertyUtil.getBooleanProperty(c, KEY_SHOW_VOLUME_ICON);
    }

    protected boolean isVolumeSlider(final JComponent c) {
        return PropertyUtil.isPropertyEqual(c, KEY_VARIANT, VARIANT_VOLUME);
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        updateVisualPaddings();
    }

    @Override
    protected TrackListener createTrackListener(final JSlider slider) {
        return new SnapTrackListener();
    }

    @Override
    protected void installListeners(final JSlider slider) {
        super.installListeners(slider);
        if (sliderListener == null) {
            sliderListener = createSliderListener();
        }
        slider.addMouseListener(sliderListener);
        slider.addMouseWheelListener(sliderListener);
        slider.addPropertyChangeListener(sliderListener);
    }

    protected DarkSliderListener createSliderListener() {
        return new DarkSliderListener(this, slider);
    }

    protected void updateVisualPaddings() {
        calculateGeometry();
        Rectangle r = new Rectangle(contentRect);
        Insets ins = new Insets(r.y, r.x, slider.getHeight() - (r.y + r.height), slider.getWidth() - (r.x + r.width));
        VisualPaddingProvider.updateProperty(slider, ins);
    }

    @Override
    protected void uninstallListeners(final JSlider slider) {
        super.uninstallListeners(slider);
        slider.removeMouseListener(sliderListener);
        slider.removeMouseWheelListener(sliderListener);
        slider.removePropertyChangeListener(sliderListener);
        sliderListener = null;
    }

    @Override
    protected void calculateGeometry() {
        // Thumb size should be calculated before content rect.
        calculateFocusRect();
        calculateThumbSize();
        calculateContentRect();
        calculateTrackBuffer();
        calculateTrackRect();
        calculateTickRect();
        calculateLabelRect();
        calculateThumbLocation();
        if (showVolumeIcon(slider)) {
            calculateIconRect();
        } else {
            iconRect.setBounds(-1, -1, 0, 0);
        }
    }

    @Override
    protected void calculateFocusRect() {
        focusRect.setRect(0, 0, slider.getWidth(), slider.getHeight());
        DarkUIUtil.applyInsets(focusRect, focusInsets);
    }

    @Override
    protected void calculateContentRect() {
        contentRect.setRect(focusRect);
        boolean horizontal = slider.getOrientation() == JSlider.HORIZONTAL;
        if (showVolumeIcon(slider)) {
            if (horizontal) {
                contentRect.width -= getVolumeIcon().getIconWidth() + iconPad;
                if (!slider.getComponentOrientation().isLeftToRight()) {
                    contentRect.x += getVolumeIcon().getIconWidth() + iconPad;
                }
            } else {
                contentRect.height -= getVolumeIcon().getIconHeight() + iconPad;
                if (!slider.getComponentOrientation().isLeftToRight()) {
                    contentRect.y += getVolumeIcon().getIconHeight() + iconPad;
                }
            }
        }
        if (horizontal) {
            int thumbWidth = getThumbWidth();
            contentRect.x += thumbWidth / 2;
            contentRect.width -= thumbWidth;
        } else {
            int thumbHeight = getThumbHeight();
            contentRect.y += thumbHeight / 2;
            contentRect.height -= thumbHeight;
        }
        adjustRect(contentRect);
    }

    @Override
    protected void calculateTrackBuffer() {
        trackBuffer = 0;
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
        setValue(slider.getValue() + delta, true);
    }

    protected void setValue(final int value, final boolean snapIfNeeded) {
        slider.setValue(snapIfNeeded ? maybeSnapValue(value) : value);
    }

    protected int maybeSnapValue(final int value) {
        if (slider.getSnapToTicks()) {
            return getSnappedValue(value);
        } else {
            return value;
        }
    }

    @Override
    public int valueForXPosition(final int xPos) {
        return maybeSnapValue(super.valueForXPosition(xPos));
    }

    @Override
    public int valueForYPosition(final int yPos) {
        return maybeSnapValue(super.valueForYPosition(yPos));
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
        Dictionary<?, ?> dict = slider.getLabelTable();
        if (!slider.getPaintLabels() || dict == null || dict.isEmpty() || !horizontal) {
            return;
        }
        int extra = getLowerHorizontalExtend();
        int extend = getUpperHorizontalExtend();
        int factor = outwards ? 1 : -1;
        rectangle.x -= factor * extra;
        rectangle.width += factor * (extra + extend);
    }

    protected int getLowerHorizontalExtend() {
        boolean ltr = !drawInverted();
        Component minLabel = ltr ? getLowestValueLabel() : getHighestValueLabel();
        boolean adjustMin = PropertyUtil.getBooleanProperty(minLabel, KEY_MANUAL_LABEL_ALIGN);
        int minPrefWidth = minLabel.getPreferredSize().width;
        float adj = (adjustMin ? minLabel.getAlignmentX() : Component.CENTER_ALIGNMENT);
        return (int) (minPrefWidth * adj);
    }

    protected int getUpperHorizontalExtend() {
        boolean ltr = !drawInverted();
        Component maxLabel = ltr ? getHighestValueLabel() : getLowestValueLabel();
        boolean adjustMax = PropertyUtil.getBooleanProperty(maxLabel, KEY_MANUAL_LABEL_ALIGN);
        int maxPrefWidth = maxLabel.getPreferredSize().width;
        float adj = (adjustMax ? maxLabel.getAlignmentX() : Component.CENTER_ALIGNMENT);
        return (int) (maxPrefWidth * (1f - adj));
    }

    public Dimension getPreferredHorizontalSize() {
        Dimension dim = super.getPreferredHorizontalSize();
        Rectangle rect = new Rectangle(0, 0, 0, 0);
        rect.setSize(dim);
        adjustRect(rect, true);
        Dictionary<?, ?> dict = slider.getLabelTable();
        if (dict != null && !dict.isEmpty()) {
            int totalLabelWidth = 0;
            Enumeration<?> labels = dict.elements();
            while (labels.hasMoreElements()) {
                Object obj = labels.nextElement();
                if (obj instanceof Component) {
                    totalLabelWidth += ((Component) obj).getPreferredSize().width;
                }
            }
            totalLabelWidth += getThumbWidth();
            rect.width = Math.max(rect.width, totalLabelWidth);
        }
        return rect.getSize();
    }

    @Override
    public Dimension getThumbSize() {
        if (isPlainThumb()) {
            int bw = 2 * getFocusBorderSize();
            return new Dimension(plainThumbRadius + bw, plainThumbRadius + bw);
        }
        return isHorizontal() ? new Dimension(thumbSize.width, thumbSize.height)
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

        if (isPlainThumb()) {
            paintPlainSliderThumb(g);
        } else {
            paintSliderThumb(g);
        }
        context.restore();
    }

    @Override
    protected void scrollDueToClickInTrack(final int dir) {
        Point p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, slider);
        // Extend the track up and down to be more lenient with hit testing.
        int size = 3 * trackSize;
        Shape area = isHorizontal()
                ? getHorizontalTrackShape(trackShape, size)
                : getVerticalTrackShape(trackShape, size);
        if (!area.getBounds().contains(p)) {
            return;
        }
        if (instantScrollEnabled(slider)) {
            int value = isHorizontal() ? valueForXPosition(p.x) : valueForYPosition(p.y);
            setValue(value, false);
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
    protected void installDefaults(final JSlider slider) {
        super.installDefaults(slider);
        LookAndFeel.installProperty(slider, PropertyKey.OPAQUE, false);
        arcSize = UIManager.getInt("Slider.arc");
        trackSize = UIManager.getInt("Slider.trackThickness");
        plainThumbRadius = UIManager.getInt("Slider.plainThumbRadius");
        thumbSize = UIManager.getDimension("Slider.thumbSize");
        iconPad = UIManager.getInt("Slider.iconPad");
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
        thumbFocusBorderColor = UIManager.getColor("Slider.thumbFocusBorderColor");
        thumbInactiveBorderColor = UIManager.getColor("Slider.thumbBorderColorDisabled");
        focusBorderSize = UIManager.getInt("Slider.focusBorderSize");
        paintFocus = UIManager.getBoolean("Slider.paintFocusGlow");

        thumb = UIManager.getIcon("Slider.enabledThumb.icon");
        thumbDisabled = UIManager.getIcon("Slider.disabledThumb.icon");
        thumbFocused = UIManager.getIcon("Slider.focusedThumb.icon");

        volumeThumb = UIManager.getIcon("Slider.volume.enabledThumb.icon");
        volumeThumbDisabled = UIManager.getIcon("Slider.volume.disabledThumb.icon");
        volumeThumbFocused = UIManager.getIcon("Slider.volume.focusedThumb.icon");

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

        rotatableIcon = new RotatableIcon();
    }

    public int getFocusBorderSize() {
        return paintFocus ? focusBorderSize : 0;
    }

    protected void calculateIconRect() {
        iconRect.width = getVolumeIcon().getIconWidth();
        iconRect.height = getVolumeIcon().getIconHeight();
        if (isHorizontal()) {
            int extraSpace = iconPad + getThumbWidth() / 2;
            if (slider.getComponentOrientation().isLeftToRight()) {
                iconRect.x = trackRect.x + trackRect.width + extraSpace;
            } else {
                iconRect.x = trackRect.x - iconRect.width - extraSpace;
            }
            iconRect.y = trackRect.y + (trackRect.height - iconRect.height) / 2;
        } else {
            int extraSpace = iconPad + getThumbHeight() / 2;
            if (slider.getComponentOrientation().isLeftToRight()) {
                iconRect.x = trackRect.x + (trackRect.width - iconRect.width) / 2;
                iconRect.y = trackRect.y + trackRect.height + extraSpace;
            } else {
                iconRect.x = trackRect.x + (trackRect.width - iconRect.width) / 2;
                iconRect.y = trackRect.y - iconRect.height - extraSpace;
            }
        }
    }

    private void setHorizontalTrackClip(final Graphics g) {
        int x = thumbRect.x + thumbRect.width / 2;
        if (!drawInverted()) {
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
        return getHorizontalTrackShape(trackShape, trackSize);
    }

    private Shape getHorizontalTrackShape(final RoundRectangle2D trackShape, final int size) {
        int arc = arcSize;
        int yOff = (trackRect.height - size) / 2;
        int w = trackRect.width;
        trackShape.setRoundRect(trackRect.x, trackRect.y + yOff, w, size, arc, arc);
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
        return getVerticalTrackShape(trackShape, trackSize);
    }

    private Shape getVerticalTrackShape(final RoundRectangle2D trackShape, final int size) {
        int arc = arcSize;
        int xOff = (trackRect.width - size) / 2;
        int h = trackRect.height;
        trackShape.setRoundRect(trackRect.x + xOff, trackRect.y, size, h, arc, arc);
        return trackShape;
    }

    private void paintPlainSliderThumb(final Graphics2D g) {
        g.translate(thumbRect.x, thumbRect.y);
        int r = plainThumbRadius;
        int bw = getFocusBorderSize();
        g.setColor(getThumbColor());
        PaintUtil.fillRoundRect(g, bw, bw, r, r, r);
        if (!isVolumeSlider(slider)) {
            g.setColor(getThumbBorderColor());
            PaintUtil.paintLineBorder(g, bw, bw, r, r, r);
        }
        if (paintFocus()) {
            PaintUtil.paintFocusBorder(g, r + 2 * bw, r + 2 * bw, r + 2 * bw, bw);
        }
        g.translate(-thumbRect.x, -thumbRect.y);
    }

    protected boolean isPlainThumb() {
        return !slider.getPaintTicks() || !PropertyUtil.getBooleanProperty(slider, KEY_THUMB_ARROW_SHAPE, true);
    }

    protected Icon getThumbIcon() {
        boolean enabled = slider.isEnabled();
        boolean focused = slider.hasFocus();
        boolean volume = isVolumeSlider(slider);
        Icon icon;
        if (volume) {
            icon = enabled ? focused ? volumeThumbFocused : volumeThumb : volumeThumbDisabled;
        } else {
            icon = enabled ? focused ? thumbFocused : thumb : thumbDisabled;
        }
        rotatableIcon.setIcon(icon);
        if (isHorizontal()) {
            rotatableIcon.setOrientation(Alignment.NORTH);
        } else {
            if (slider.getComponentOrientation().isLeftToRight()) {
                rotatableIcon.setOrientation(Alignment.WEST);
            } else {
                rotatableIcon.setOrientation(Alignment.EAST);
            }
        }
        return rotatableIcon;
    }

    private void paintSliderThumb(final Graphics2D g) {
        if (isHorizontal()) {
            getThumbIcon().paintIcon(slider, g, thumbRect.x, thumbRect.y);
        } else {
            int cx = thumbRect.x + thumbRect.width / 2;
            int cy = thumbRect.y + thumbRect.height / 2;
            int x = cx - thumbRect.height / 2;
            int y = cy - thumbRect.width / 2;
            getThumbIcon().paintIcon(slider, g, x, y);
        }
    }

    private boolean paintFocus() {
        return slider.hasFocus() && paintFocus;
    }

    protected int getThumbWidth() {
        return thumbRect.width;
    }

    protected int getThumbHeight() {
        return thumbRect.height;
    }

    protected Color getThumbColor() {
        if (isVolumeSlider(slider)) {
            return slider.isEnabled() ? volumeThumbBackground : volumeThumbInactiveBackground;
        } else {
            return slider.isEnabled() ? thumbBackground : thumbInactiveBackground;
        }
    }

    protected Color getThumbBorderColor() {
        return slider.isEnabled() ? paintFocus() ? thumbFocusBorderColor : thumbBorderColor : thumbInactiveBorderColor;
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

    private int getSnappedValue(final int value) {
        // Now calculate if we should adjust the value
        int snappedValue = value;
        int tickSpacing = getTickSpacing();
        // If it's not on a tick, change the value
        if (tickSpacing != 0) {
            if ((value - slider.getMinimum()) % tickSpacing != 0) {
                float temp = (float) (value - slider.getMinimum()) / (float) tickSpacing;
                snappedValue = slider.getMinimum() + (Math.round(temp) * tickSpacing);
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
            int value = isHorizontal() ? valueForXPosition(e.getX()) : valueForYPosition(e.getY());
            return DarkSliderUI.this.getSnappedValue(value);
        }
    }
}
