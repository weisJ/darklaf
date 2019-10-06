package com.weis.darklaf.ui.slider;

import com.weis.darklaf.decorators.MouseClickListener;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSliderUI;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Path2D;
import java.awt.geom.Rectangle2D;
import java.awt.geom.RoundRectangle2D;

public class DarkSliderUI extends BasicSliderUI {

    private static final int TRACK_SIZE = 4;
    private static final int ARC_SIZE = 4;
    private static final int PLAIN_THUMB_SIZE = 12;
    private static final int THUMB_SIZE_1 = 10;
    private static final int THUMB_SIZE_2 = 18;
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
                    slider.setValue(oldValue);
                    muted = false;
                } else {
                    oldValue = slider.getValue();
                    slider.setValue(slider.getMinimum());
                    muted = true;
                }
            }
        }
    };

    public DarkSliderUI(final JSlider b) {
        super(b);
    }

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkSliderUI((JSlider) c);
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        slider.putClientProperty("Slider.paintFocusGlow", UIManager.getBoolean("Slider.paintFocusGlow"));
    }

    @Override
    protected TrackListener createTrackListener(final JSlider slider) {
        return new SnapTrackListener();
    }

    @Override
    protected void installListeners(final JSlider slider) {
        super.installListeners(slider);
        slider.addMouseListener(mouseListener);
    }

    @Override
    protected void uninstallListeners(final JSlider slider) {
        super.uninstallListeners(slider);
        slider.removeMouseListener(mouseListener);
    }

    @Override
    protected void calculateGeometry() {
        super.calculateGeometry();
        calculateIconRect();
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
    protected Dimension getThumbSize() {
        if (isPlainThumb()) {
            return new Dimension(PLAIN_THUMB_SIZE + 6, PLAIN_THUMB_SIZE + 6);
        }
        return isHorizontal()
               ? new Dimension(THUMB_SIZE_1, THUMB_SIZE_2)
               : new Dimension(THUMB_SIZE_2, THUMB_SIZE_1);
    }

    @Override
    public void paint(@NotNull final Graphics g2, final JComponent c) {
        super.paint(g2, c);
        if (showVolumeIcon(c)) {
            getVolumeIcon().paintIcon(c, g2, iconRect.x, iconRect.y);
        }
    }

    @Override
    public void paintFocus(final Graphics g2) {
        //Do nothing
    }

    @Override
    public void paintTrack(final Graphics g2d) {
        Graphics2D g = (Graphics2D) g2d;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);

        Color bgColor = getTrackBackground();
        Color selectionColor = getSelectedTrackColor();

        if (isHorizontal()) {
            var track = getHorizontalTrackShape();
            g.setColor(bgColor);
            g.fill(track);
            var selection = getHorizontalSliderShape(track);
            g.setColor(selectionColor);
            g.fill(selection);
        } else {
            var track = getVerticalTrackShape();
            g.setColor(bgColor);
            g.fill(track);
            var selection = getVerticalSliderShape(track);
            g.setColor(selectionColor);
            g.fill(selection);
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
        var config = GraphicsUtil.setupAntialiasing(g);
        super.paintLabels(g);
        config.restore();
    }

    @Override
    protected void paintHorizontalLabel(final Graphics g, final int value, final Component label) {
        checkDisabled(g);
        super.paintHorizontalLabel(g, value, label);
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
        var area = isHorizontal() ? getHorizontalTrackShape() : getVerticalTrackShape();
        if (!area.getBounds().contains(p)) {
            return;
        }
        if (instantScrollEnabled(slider)) {
            int value = isHorizontal() ? valueForXPosition(p.x) : valueForYPosition(p.y);
            slider.setValue(value);
        } else {
            super.scrollDueToClickInTrack(dir);
        }
    }

    private void checkDisabled(final Graphics g) {
        if (!slider.isEnabled()) {
            g.setColor(getDisabledTickColor());
        }
    }

    @NotNull
    protected Color getDisabledTickColor() {
        return UIManager.getColor("Slider.disabledTickColor");
    }

    private static boolean showVolumeIcon(final JComponent c) {
        return isVolumeSlider(c)
                && Boolean.TRUE.equals(c.getClientProperty("Slider.volume.showIcon"));
    }

    protected Icon getVolumeIcon() {
        int range = slider.getMaximum() - slider.getMinimum();
        int value = slider.getValue() - slider.getMinimum();
        double percentage = value / (double) range;
        String prefix = slider.isEnabled() ? "enabled_" : "disabled_";
        if (Math.abs(percentage) < 1E-6) {
            return UIManager.getIcon("Slider.volume." + prefix + "level_0.icon");
        } else if (percentage < 0.25) {
            return UIManager.getIcon("Slider.volume." + prefix + "level_1.icon");
        } else if (percentage < 0.5) {
            return UIManager.getIcon("Slider.volume." + prefix + "level_2.icon");
        } else if (percentage < 0.75) {
            return UIManager.getIcon("Slider.volume." + prefix + "level_3.icon");
        } else {
            return UIManager.getIcon("Slider.volume." + prefix + "level_4.icon");
        }
    }

    private static boolean isVolumeSlider(@NotNull final JComponent c) {
        return "volume".equals(c.getClientProperty("Slider.variant"));
    }

    protected boolean isPlainThumb() {
        Boolean paintThumbArrowShape = (Boolean) slider.getClientProperty("Slider.paintThumbArrowShape");
        return (!slider.getPaintTicks() && paintThumbArrowShape == null) ||
                paintThumbArrowShape == Boolean.FALSE;
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

    private boolean isHorizontal() {
        return slider.getOrientation() == JSlider.HORIZONTAL;
    }

    @NotNull
    @Contract(" -> new")
    private Area getHorizontalTrackShape() {
        int trackSize = TRACK_SIZE;
        int arc = ARC_SIZE;
        int yOff = (trackRect.height / 2) - trackSize / 2;
        int w = showVolumeIcon(slider) ? trackRect.width + getIconBarExt() : trackRect.width;
        if (slider.getComponentOrientation().isLeftToRight()) {
            return new Area(new RoundRectangle2D.Double(
                    trackRect.x, trackRect.y + yOff, w, trackSize, arc, arc));
        } else {
            return new Area(new RoundRectangle2D.Double(
                    trackRect.x - getIconBarExt(), trackRect.y + yOff, w, trackSize, arc, arc));
        }
    }

    @Contract("_ -> param1")
    @NotNull
    private Area getHorizontalSliderShape(@NotNull final Area track) {
        double x = thumbRect.x + thumbRect.width / 2.0;
        var leftArea = new Area(new Rectangle2D.Double(0, 0, x, slider.getHeight()));
        var rightArea = new Area(new Rectangle2D.Double(x, 0, slider.getWidth() - x, slider.getHeight()));
        if (slider.getComponentOrientation().isLeftToRight()) {
            if (slider.getInverted()) {
                track.intersect(rightArea);
            } else {
                track.intersect(leftArea);
            }
        } else {
            if (!slider.getInverted()) {
                track.intersect(rightArea);
            } else {
                track.intersect(leftArea);
            }
        }
        return track;
    }

    @NotNull
    @Contract(" -> new")
    private Area getVerticalTrackShape() {
        int trackSize = TRACK_SIZE;
        int arc = ARC_SIZE;
        int xOff = (trackRect.width / 2) - trackSize / 2;
        int h = showVolumeIcon(slider) ? trackRect.height + getIconBarExt() : trackRect.height;
        if (slider.getComponentOrientation().isLeftToRight()) {
            return new Area(new RoundRectangle2D.Double(
                    trackRect.x + xOff, trackRect.y, trackSize, h, arc, arc));
        } else {
            return new Area(new RoundRectangle2D.Double(
                    trackRect.x + xOff, trackRect.y - getIconBarExt(), trackSize, h, arc, arc));

        }
    }

    private int getIconBarExt() {
        return isPlainThumb() && showVolumeIcon(slider) ? ICON_BAR_EXT : 0;
    }

    @Contract("_ -> param1")
    @NotNull
    private Area getVerticalSliderShape(@NotNull final Area track) {
        int y = thumbRect.y + thumbRect.height / 2;
        if (slider.getInverted()) {
            track.intersect(new Area(new Rectangle2D.Double(0, 0, slider.getWidth(), y)));
        } else {
            track.intersect(new Area(new Rectangle2D.Double(0, y, slider.getWidth(), slider.getHeight() - y)));
        }
        return track;
    }

    private void paintPlainSliderThumb(@NotNull final Graphics2D g) {
        int r = PLAIN_THUMB_SIZE;
        int x = isHorizontal() ? 4 : (thumbRect.width - r) / 2;
        int y = isHorizontal() ? (thumbRect.height - r) / 2 : 4;
        g.translate(x, y);
        Ellipse2D.Double thumb = new Ellipse2D.Double(0, 0, r, r);
        Ellipse2D.Double innerThumb = new Ellipse2D.Double(1, 1, r - 2, r - 2);
        if (paintFocus()) {
            GraphicsContext config = new GraphicsContext(g);
            g.setComposite(DarkUIUtil.ALPHA_COMPOSITE);
            DarkUIUtil.paintFocusOval(g, 1, 1, r - 2, r - 2);
            config.restore();
        }
        if (isVolumeSlider(slider)) {
            g.setColor(getThumbColor());
            g.fill(thumb);
        } else {
            g.setColor(getThumbBorderColor());
            g.fill(thumb);
            g.setColor(getThumbColor());
            g.fill(innerThumb);
        }
        g.translate(-x, -y);
    }

    private void paintSliderThumb(final Graphics2D g) {
        Path2D thumb = getThumbShape();
        if (paintFocus()) {
            GraphicsContext config = new GraphicsContext(g);
            g.setComposite(DarkUIUtil.ALPHA_COMPOSITE);
            g.setStroke(new BasicStroke(4.25f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
            DarkUIUtil.Outline.focus.setGraphicsColor(g, true);
            g.draw(thumb);
            config.restore();
        }
        g.setColor(getThumbColor());
        g.fill(thumb);
        g.setColor(getThumbBorderColor());
        g.draw(thumb);
    }

    private boolean paintFocus() {
        return slider.hasFocus() && Boolean.TRUE.equals(slider.getClientProperty("Slider.paintFocusGlow"));
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

    @NotNull
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

    @NotNull
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

    @NotNull
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

    @NotNull
    protected Color getTrackBackground() {
        return UIManager.getColor("Slider.trackBackground");
    }

    @NotNull
    protected Color getSelectedTrackColor() {
        if (isVolumeSlider(slider)) {
            return slider.isEnabled()
                   ? UIManager.getColor("Slider.volume.selectedTrackColor")
                   : UIManager.getColor("Slider.volume.disabledTrackColor");
        } else {
            return slider.isEnabled()
                   ? UIManager.getColor("Slider.selectedTrackColor")
                   : UIManager.getColor("Slider.disabledTrackColor");
        }
    }

    @NotNull
    protected Color getThumbColor() {
        if (isVolumeSlider(slider)) {
            return slider.isEnabled()
                   ? UIManager.getColor("Slider.volume.activeThumbFill")
                   : UIManager.getColor("Slider.volume.inactiveThumbFill");
        } else {
            return slider.isEnabled()
                   ? UIManager.getColor("Slider.activeThumbFill")
                   : UIManager.getColor("Slider.inactiveThumbFill");
        }
    }

    @NotNull
    protected Color getThumbBorderColor() {
        return slider.isEnabled()
               ? UIManager.getColor("Slider.thumbBorderColor")
               : UIManager.getColor("Slider.thumbBorderColorDisabled");
    }

    private boolean instantScrollEnabled(@NotNull final JComponent c) {
        return Boolean.TRUE.equals(c.getClientProperty("Slider.instantScrollEnabled"));
    }

    public class SnapTrackListener extends TrackListener {
        private int offset;

        public void mousePressed(final MouseEvent evt) {
            int pos = isHorizontal() ? evt.getX() : evt.getY();
            int loc = getLocationForValue(getSnappedValue(evt));
            offset = (loc < 0) ? 0 : pos - loc;
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
            // Now calculate if we should adjust the value
            int snappedValue = value;
            int tickSpacing = 0;
            int majorTickSpacing = slider.getMajorTickSpacing();
            int minorTickSpacing = slider.getMinorTickSpacing();
            if (minorTickSpacing > 0) {
                tickSpacing = minorTickSpacing;
            } else if (majorTickSpacing > 0) {
                tickSpacing = majorTickSpacing;
            }
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
    }
}
