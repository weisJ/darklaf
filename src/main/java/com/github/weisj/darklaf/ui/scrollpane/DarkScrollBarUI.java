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
package com.github.weisj.darklaf.ui.scrollpane;

import com.github.weisj.darklaf.decorators.MouseMovementListener;
import com.github.weisj.darklaf.util.Animator;
import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollBarUI;
import java.awt.*;
import java.awt.event.AdjustmentEvent;
import java.awt.event.AdjustmentListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkScrollBarUI extends BasicScrollBarUI {

    private static final float THUMB_ALPHA = 0.6f;
    private static final float MAX_TRACK_ALPHA = 0.3f;
    private static final float MAX_THUMB_ALPHA = 1;
    private static final int DELAY_FRAMES = 6;
    private static final int FADEOUT_FRAMES_COUNT = 10 + DELAY_FRAMES;
    private static final int FADEIN_FRAMES_COUNT = FADEOUT_FRAMES_COUNT / 2;
    private static final int FADEOUT_FRAME_COUNT_FACTOR = 50;
    private static final int FADEIN_FRAME_COUNT_FACTOR = 25;
    private static final int THICKNESS = 10;
    private static final int THICKNESS_THIN = 8;

    private static final AlphaComposite COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER);

    private final MouseWheelListener mouseWheelListener = e -> {
        if (scrollbar.getParent() instanceof JScrollPane
                && !((JScrollPane) scrollbar.getParent()).isWheelScrollingEnabled()) {
            return;
        }
        if (scrollbar.getOrientation() == VERTICAL && !e.isShiftDown()
                || scrollbar.getOrientation() == HORIZONTAL && e.isShiftDown()) {
            scrollbar.setValueIsAdjusting(true);
            var sp = scrollbar.getClientProperty("JScrollBar.scrollPaneParent");
            if (scrollbar.getParent() instanceof JScrollPane) {
                doScroll(scrollbar, ((JScrollPane) scrollbar.getParent()).getViewport(), e,
                         scrollbar.getParent().getComponentOrientation().isLeftToRight());
            } else if (sp instanceof JScrollPane) {
                doScroll(scrollbar, ((JScrollPane) sp).getViewport(), e,
                         scrollbar.getParent().getComponentOrientation().isLeftToRight());
            } else {
                doScroll(scrollbar, null, e, scrollbar.getComponentOrientation().isLeftToRight());
            }
            scrollbar.setValueIsAdjusting(false);
        }
    };
    protected Color thumbBorderColor;
    protected Color thumbFadeStartColor;
    protected Color thumbFadeEndColor;
    protected Color trackBackground;
    private float trackAlpha;
    private float thumbAlpha;
    private Animator trackFadeoutAnimator;
    private Animator trackFadeinAnimator;
    private Animator thumbFadeoutAnimator;
    private Animator thumbFadeinAnimator;
    private boolean mouseOverTrack = false;
    private boolean mouseOverThumb = false;
    private final MouseMotionListener mouseMotionListener = (MouseMovementListener) e -> {
        if (e == null) {
            return;
        }
        boolean overThumb = isOverThumb(e.getPoint());
        if (overThumb != mouseOverThumb) {
            mouseOverThumb = overThumb;
            if (!scrollbar.getValueIsAdjusting()) {
                resetThumbAnimator();
            }
        }
    };
    private final AdjustmentListener adjustmentListener = new AdjustmentListener() {
        @Override
        public void adjustmentValueChanged(@NotNull final AdjustmentEvent e) {
            if (!e.getValueIsAdjusting()) return;

            JScrollBar scrollBar = (JScrollBar) e.getAdjustable();
            int extent = scrollBar.getModel().getExtent();
            int value = scrollBar.getValue() + extent;
            if (value == extent || value == scrollBar.getMaximum()) return;

            var p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, scrollbar);
            if (!getThumbBounds().contains(p)) {
                if (!thumbFadeinAnimator.isRunning() && e.getValueIsAdjusting()) {
                    mouseOverThumb = true;
                    resetThumbAnimator();
                }
            }
        }
    };
    private final MouseListener mouseListener = new MouseAdapter() {

        @Override
        public void mouseReleased(@NotNull final MouseEvent e) {
            if (!getTrackBounds().contains(e.getPoint())) {
                mouseOverTrack = false;
                resetTrackAnimator();
            }
            if (!getThumbBounds().contains(e.getPoint())) {
                mouseOverThumb = false;
                resetThumbAnimator();
            }
        }

        @Override
        public void mouseEntered(final MouseEvent e) {
            if (getThumbBounds().isEmpty()) {
                return;
            }
            mouseOverTrack = true;
            if (!scrollbar.getValueIsAdjusting()) {
                resetTrackAnimator();
            }
            mouseMotionListener.mouseMoved(e);
        }

        @Override
        public void mouseExited(final MouseEvent e) {
            if (getThumbBounds().isEmpty()) {
                return;
            }
            mouseOverTrack = false;
            if (!scrollbar.getValueIsAdjusting()) {
                resetTrackAnimator();
            }
            mouseMotionListener.mouseMoved(e);
        }
    };

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkScrollBarUI();
    }

    @SuppressWarnings("MagicConstant")
    public static void doScroll(@NotNull final JScrollBar toScroll, final JViewport vp,
                                @NotNull final MouseWheelEvent e, final boolean leftToRight) {
        int direction = e.getWheelRotation() < 0 ? -1 : 1;
        int orientation = toScroll.getOrientation();
        if (!leftToRight && orientation == JScrollBar.HORIZONTAL) {
            direction *= -1;
        }

        if (e.getScrollType() == MouseWheelEvent.WHEEL_UNIT_SCROLL) {
            int units = Math.abs(e.getUnitsToScroll());

            boolean limitScroll = Math.abs(e.getWheelRotation()) == 1;

            Object fastWheelScroll = toScroll.getClientProperty("JScrollBar.fastWheelScrolling");
            var comp = vp == null ? null : vp.getView();
            if (Boolean.TRUE.equals(fastWheelScroll) && comp instanceof Scrollable) {
                Scrollable scrollComp = (Scrollable) comp;
                Rectangle viewRect = vp.getViewRect();
                int startingX = viewRect.x;
                int scrollMin = toScroll.getMinimum();
                int scrollMax = toScroll.getMaximum() - toScroll.getModel().getExtent();

                if (limitScroll) {
                    int blockIncr = scrollComp.getScrollableBlockIncrement(viewRect, orientation, direction);
                    if (direction < 0) {
                        scrollMin = Math.max(scrollMin,
                                             toScroll.getValue() - blockIncr);
                    } else {
                        scrollMax = Math.min(scrollMax,
                                             toScroll.getValue() + blockIncr);
                    }
                }

                for (int i = 0; i < units; i++) {
                    int unitIncr = scrollComp.getScrollableUnitIncrement(viewRect, orientation, direction);
                    // Modify the visible rect for the next unit, and
                    // check to see if we're at the end already.
                    if (orientation == SwingConstants.VERTICAL) {
                        if (direction < 0) {
                            viewRect.y -= unitIncr;
                            if (viewRect.y <= scrollMin) {
                                viewRect.y = scrollMin;
                                break;
                            }
                        } else { // (direction > 0
                            viewRect.y += unitIncr;
                            if (viewRect.y >= scrollMax) {
                                viewRect.y = scrollMax;
                                break;
                            }
                        }
                    } else {
                        // Scroll left
                        if ((leftToRight && direction < 0) ||
                                (!leftToRight && direction > 0)) {
                            viewRect.x -= unitIncr;
                            if (leftToRight) {
                                if (viewRect.x < scrollMin) {
                                    viewRect.x = scrollMin;
                                    break;
                                }
                            }
                        }
                        // Scroll right
                        else {
                            viewRect.x += unitIncr;
                            if (leftToRight) {
                                if (viewRect.x > scrollMax) {
                                    viewRect.x = scrollMax;
                                    break;
                                }
                            }
                        }
                    }
                }
                // Set the final view position on the ScrollBar
                if (orientation == SwingConstants.VERTICAL) {
                    toScroll.setValue(viewRect.y);
                } else {
                    if (leftToRight) {
                        toScroll.setValue(viewRect.x);
                    } else {
                        // rightToLeft scrollbars are oriented with
                        // minValue on the right and maxValue on the
                        // left.
                        int newPos = toScroll.getValue() - (viewRect.x - startingX);
                        if (newPos < scrollMin) {
                            newPos = scrollMin;
                        } else if (newPos > scrollMax) {
                            newPos = scrollMax;
                        }
                        toScroll.setValue(newPos);
                    }
                }
            } else {
                // Viewport's view is not a Scrollable, or fast wheel
                // scrolling is not enabled.
                scrollByUnits(toScroll, direction, units, limitScroll);
            }
        } else if (e.getScrollType() == MouseWheelEvent.WHEEL_BLOCK_SCROLL) {
            scrollByBlock(toScroll, direction);
        }
    }

    static void scrollByUnits(final JScrollBar scrollbar, final int direction,
                              final int units, final boolean limitToBlock) {
        // This method is called from BasicScrollPaneUI to implement wheel
        // scrolling, as well as from scrollByUnit().
        int delta;
        int limit = -1;

        if (limitToBlock) {
            if (direction < 0) {
                limit = scrollbar.getValue() -
                        scrollbar.getBlockIncrement(direction);
            } else {
                limit = scrollbar.getValue() +
                        scrollbar.getBlockIncrement(direction);
            }
        }

        for (int i = 0; i < units; i++) {
            if (direction > 0) {
                delta = scrollbar.getUnitIncrement(direction);
            } else {
                delta = -scrollbar.getUnitIncrement(direction);
            }

            int oldValue = scrollbar.getValue();
            int newValue = oldValue + delta;

            // Check for overflow.
            if (delta > 0 && newValue < oldValue) {
                newValue = scrollbar.getMaximum();
            } else if (delta < 0 && newValue > oldValue) {
                newValue = scrollbar.getMinimum();
            }
            if (oldValue == newValue) {
                break;
            }

            if (limitToBlock && i > 0) {
                assert limit != -1;
                if ((direction < 0 && newValue < limit) ||
                        (direction > 0 && newValue > limit)) {
                    break;
                }
            }
            scrollbar.setValue(newValue);
        }
    }

    static void scrollByBlock(@NotNull final JScrollBar scrollbar, final int direction) {
        // This method is called from BasicScrollPaneUI to implement wheel
        // scrolling, and also from scrollByBlock().
        int oldValue = scrollbar.getValue();
        int blockIncrement = scrollbar.getBlockIncrement(direction);
        int delta = blockIncrement * ((direction > 0) ? +1 : -1);
        int newValue = oldValue + delta;

        // Check for overflow.
        if (delta > 0 && newValue < oldValue) {
            newValue = scrollbar.getMaximum();
        } else if (delta < 0 && newValue > oldValue) {
            newValue = scrollbar.getMinimum();
        }

        scrollbar.setValue(newValue);
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        scrollbar.setFocusable(false);
    }

    @Override
    protected void installDefaults() {
        final int incGap = UIManager.getInt("ScrollBar.incrementButtonGap");
        final int decGap = UIManager.getInt("ScrollBar.decrementButtonGap");
        try {
            UIManager.put("ScrollBar.incrementButtonGap", 0);
            UIManager.put("ScrollBar.decrementButtonGap", 0);
            super.installDefaults();
        } finally {
            UIManager.put("ScrollBar.incrementButtonGap", incGap);
            UIManager.put("ScrollBar.decrementButtonGap", decGap);
        }
        thumbBorderColor = UIManager.getColor("ScrollBar.thumbBorderColor");
        thumbFadeStartColor = UIManager.getColor("ScrollBar.fadeStartColor");
        thumbFadeEndColor = UIManager.getColor("ScrollBar.fadeEndColor");
        trackBackground = UIManager.getColor("ScrollBar.trackColor");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        if (trackFadeoutAnimator == null || trackFadeoutAnimator.isDisposed()) {
            trackFadeoutAnimator = createTrackFadeoutAnimator();
        }
        if (thumbFadeoutAnimator == null || thumbFadeoutAnimator.isDisposed()) {
            thumbFadeoutAnimator = createThumbFadeoutAnimator();
        }
        if (trackFadeinAnimator == null || trackFadeinAnimator.isDisposed()) {
            trackFadeinAnimator = createTrackFadeinAnimator();
        }
        if (thumbFadeinAnimator == null || thumbFadeinAnimator.isDisposed()) {
            thumbFadeinAnimator = createThumbFadeinAnimator();
        }
        scrollbar.addAdjustmentListener(adjustmentListener);
        scrollbar.addMouseMotionListener(mouseMotionListener);
        scrollbar.addMouseListener(mouseListener);
        scrollbar.addMouseWheelListener(mouseWheelListener);
    }

    @Override
    protected void uninstallListeners() {
        if (scrollTimer != null) {
            // it is already called otherwise
            super.uninstallListeners();
        }
        scrollbar.removeMouseListener(mouseListener);
        scrollbar.removeMouseWheelListener(mouseWheelListener);
        scrollbar.removeMouseMotionListener(mouseMotionListener);
        scrollbar.removeAdjustmentListener(adjustmentListener);
        trackFadeoutAnimator.dispose();
        thumbFadeoutAnimator.dispose();
        trackFadeinAnimator.dispose();
        thumbFadeinAnimator.dispose();
    }

    @Override
    protected ModelListener createModelListener() {
        return new ModelListener() {
            @Override
            public void stateChanged(final ChangeEvent e) {
                if (scrollbar != null) {
                    super.stateChanged(e);
                }
            }
        };
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        return getMinimumSize(c);
    }

    @Override
    public Dimension getMaximumSize(final JComponent c) {
        return getMinimumSize(c);
    }

    @Override
    protected JButton createDecreaseButton(final int orientation) {
        return new EmptyButton();
    }

    @Override
    protected JButton createIncreaseButton(final int orientation) {
        return new EmptyButton();
    }

    protected void paintTrack(@NotNull final Graphics g, @NotNull final JComponent c, @NotNull final Rectangle bounds) {
        if (c.isOpaque()) {
            g.setColor(scrollbar.getBackground());
            g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
        }
        Graphics2D g2 = (Graphics2D) g.create();
        g2.setColor(getTrackColor());
        g2.setComposite(COMPOSITE.derive(trackAlpha));
        g2.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
        g2.dispose();
    }

    protected void paintThumb(@NotNull final Graphics g, final JComponent c, @NotNull final Rectangle thumbBounds) {
        if (!thumbBounds.isEmpty() && scrollbar.isEnabled()) {
            paintMaxiThumb((Graphics2D) g, thumbBounds);
        }
    }

    protected void paintMaxiThumb(@NotNull final Graphics2D g, @NotNull final Rectangle rect) {
        final var c = g.getComposite();
        g.setComposite(COMPOSITE.derive(THUMB_ALPHA));
        var thumbColor = getThumbColor();
        double percent = Math.min(1.0, Math.max(0.0, 1 - (thumbAlpha - THUMB_ALPHA)));
        g.setColor(DarkUIUtil.blendColors(thumbBorderColor, thumbColor, percent));
        DarkUIUtil.drawRect(g, rect.x, rect.y, rect.width, rect.height, 1);
        g.setColor(thumbColor);
        g.fillRect(rect.x + 1, rect.y + 1, rect.width - 2, rect.height - 2);
        g.setComposite(c);
    }

    @NotNull
    private Color getThumbColor() {
        return DarkUIUtil.blendColors(thumbFadeEndColor, thumbFadeStartColor, thumbAlpha);
    }

    @Override
    protected Dimension getMinimumThumbSize() {
        return isVertical()
               ? new Dimension(getThickness(), getThickness() * 2)
               : new Dimension(getThickness() * 2, getThickness());
    }

    @Override
    public void layoutContainer(final Container scrollbarContainer) {
        try {
            super.layoutContainer(scrollbarContainer);
        } catch (NullPointerException ignore) {
            //installUI is not performed yet or uninstallUI has set almost every field to null.
        }
    }

    @Override
    public boolean getSupportsAbsolutePositioning() {
        return true;
    }

    @NotNull
    @Contract(value = " -> new", pure = true)
    protected Color getTrackColor() {
        return trackBackground;
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        return getMinimumThumbSize();
    }

    protected boolean isVertical() {
        return this.scrollbar.getOrientation() == JScrollBar.VERTICAL;
    }

    private int getThickness() {
        return isThin() ? THICKNESS_THIN : THICKNESS;
    }

    protected boolean isThin() {
        return scrollbar.getClientProperty("ScrollBar.thin") == Boolean.TRUE;
    }

    @NotNull
    @Contract(" -> new")
    private Animator createTrackFadeoutAnimator() {
        return new Animator("Track fadeout", FADEOUT_FRAMES_COUNT,
                            FADEOUT_FRAMES_COUNT * FADEOUT_FRAME_COUNT_FACTOR, false) {
            public void paintNow(final int frame, final int totalFrames, final int cycle) {
                trackAlpha = MAX_TRACK_ALPHA;
                if (frame > DELAY_FRAMES) {
                    trackAlpha *= (float) (1 - (double) frame / totalFrames);
                }
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }

            @Override
            protected void paintCycleEnd() {
                trackAlpha = 0;
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }
        };
    }

    @NotNull
    @Contract(" -> new")
    private Animator createThumbFadeoutAnimator() {
        return new Animator("Adjustment fadeout", FADEOUT_FRAMES_COUNT,
                            FADEOUT_FRAMES_COUNT * FADEOUT_FRAME_COUNT_FACTOR, false) {
            @Override
            public void paintNow(final int frame, final int totalFrames, final int cycle) {
                thumbAlpha = MAX_THUMB_ALPHA;
                if (frame > DELAY_FRAMES) {
                    thumbAlpha *= (float) (1 - (double) frame / totalFrames);
                }
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }

            @Override
            protected void paintCycleEnd() {
                thumbAlpha = 0;
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }
        };
    }

    @NotNull
    @Contract(" -> new")
    private Animator createTrackFadeinAnimator() {
        return new Animator("Track fadein", FADEIN_FRAMES_COUNT,
                            FADEIN_FRAMES_COUNT * FADEIN_FRAME_COUNT_FACTOR, false) {
            public void paintNow(final int frame, final int totalFrames, final int cycle) {
                trackAlpha = ((float) frame * MAX_TRACK_ALPHA) / totalFrames;
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }

            @Override
            protected void paintCycleEnd() {
                trackAlpha = MAX_TRACK_ALPHA;
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }
        };
    }

    @NotNull
    @Contract(" -> new")
    private Animator createThumbFadeinAnimator() {
        return new Animator("Adjustment fadein", FADEIN_FRAMES_COUNT / 2,
                            FADEIN_FRAMES_COUNT * FADEIN_FRAME_COUNT_FACTOR, false) {
            @Override
            public void paintNow(final int frame, final int totalFrames, final int cycle) {
                thumbAlpha = ((float) frame * MAX_THUMB_ALPHA) / totalFrames;
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
            }

            @Override
            protected void paintCycleEnd() {
                thumbAlpha = MAX_THUMB_ALPHA;
                if (scrollbar != null) {
                    scrollbar.getParent().repaint();
                }
                var p = MouseInfo.getPointerInfo().getLocation();
                SwingUtilities.convertPointFromScreen(p, scrollbar);
                if (!getThumbBounds().contains(p)) {
                    mouseOverThumb = false;
                    resetThumbAnimator();
                }
            }
        };
    }

    private void resetThumbAnimator() {
        resetAnimators(thumbFadeinAnimator, thumbFadeoutAnimator, mouseOverThumb, thumbAlpha, MAX_THUMB_ALPHA);
    }

    private void resetAnimators(@NotNull final Animator fadeInAnimator, @NotNull final Animator fadeOutAnimator,
                                final boolean overAnimatedComponent, final float currentAlpha, final float maxAlpha) {
        fadeInAnimator.reset();
        fadeOutAnimator.reset();
        if (scrollbar != null && (scrollbar.getValueIsAdjusting() || overAnimatedComponent)) {
            fadeOutAnimator.suspend();
            int startFrame = (int) ((currentAlpha / maxAlpha) * fadeInAnimator.getTotalFrames());
            fadeInAnimator.resume(startFrame);
        } else {
            fadeInAnimator.suspend();
            int startFrame = 0;
            if (currentAlpha < maxAlpha) {
                startFrame = (int) ((1 - currentAlpha / maxAlpha) * fadeInAnimator.getTotalFrames());
            }
            fadeOutAnimator.resume(startFrame);
        }
    }

    private void resetTrackAnimator() {
        resetAnimators(trackFadeinAnimator, trackFadeoutAnimator, mouseOverTrack, trackAlpha, MAX_TRACK_ALPHA);
    }

    private boolean isOverThumb(final Point p) {
        final Rectangle bounds = getThumbBounds();
        return bounds != null && bounds.contains(p);
    }

    private static final class EmptyButton extends JButton {
        private EmptyButton() {
            setFocusable(false);
            setRequestFocusEnabled(false);
        }

        @NotNull
        @Contract(pure = true)
        @Override
        public Dimension getPreferredSize() {
            return getMaximumSize();
        }

        @NotNull
        @Contract(value = " -> new", pure = true)
        @Override
        public Dimension getMaximumSize() {
            return new Dimension(0, 0);
        }

        @NotNull
        @Contract(pure = true)
        @Override
        public Dimension getMinimumSize() {
            return getMaximumSize();
        }
    }
}
