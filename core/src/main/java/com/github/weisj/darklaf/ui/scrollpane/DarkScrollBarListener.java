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
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.Animator;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkScrollBarListener extends MouseAdapter implements AdjustmentListener, ScrollBarConstants {

    private static final float MAX_TRACK_ALPHA = 0.3f;
    private static final float MAX_THUMB_ALPHA = 1;
    private static final int DELAY_FRAMES = 6;
    private static final int FADEOUT_FRAMES_COUNT = 10 + DELAY_FRAMES;
    private static final int FADEIN_FRAMES_COUNT = FADEOUT_FRAMES_COUNT / 2;
    private static final int FADEOUT_FRAME_COUNT_FACTOR = 25;
    private static final int FADEIN_FRAME_COUNT_FACTOR = 25;

    protected final JScrollBar scrollbar;
    private final DarkScrollBarUI ui;

    private final Animator trackFadeoutAnimator;
    private final Animator trackFadeinAnimator;
    private final Animator thumbFadeoutAnimator;
    private final Animator thumbFadeinAnimator;

    protected boolean mouseOverThumb = false;
    protected boolean mouseOverTrack = false;
    protected float trackAlpha;
    protected float thumbAlpha;

    public DarkScrollBarListener(final JScrollBar scrollbar, final DarkScrollBarUI ui) {
        this.scrollbar = scrollbar;
        this.ui = ui;
        trackFadeoutAnimator = createTrackFadeoutAnimator();
        trackFadeinAnimator = createTrackFadeinAnimator();
        thumbFadeoutAnimator = createThumbFadeoutAnimator();
        thumbFadeinAnimator = createThumbFadeinAnimator();
    }

    public void uninstall() {
        trackFadeoutAnimator.dispose();
        thumbFadeoutAnimator.dispose();
        trackFadeinAnimator.dispose();
        thumbFadeinAnimator.dispose();
    }

    public float getTrackAlpha() {
        return trackAlpha;
    }

    public float getThumbAlpha() {
        return thumbAlpha;
    }

    @Override
    public void mouseWheelMoved(final MouseWheelEvent e) {
        if (
            scrollbar.getParent() instanceof JScrollPane
                && !((JScrollPane) scrollbar.getParent()).isWheelScrollingEnabled()
        ) {
            return;
        }
        if (
            scrollbar.getOrientation() == JScrollBar.VERTICAL && !e.isShiftDown()
                || scrollbar.getOrientation() == JScrollBar.HORIZONTAL && e.isShiftDown()
        ) {
            scrollbar.setValueIsAdjusting(true);
            JScrollPane sp = PropertyUtil.getObject(scrollbar, KEY_SCROLL_PANE_PARENT, JScrollPane.class);
            if (scrollbar.getParent() instanceof JScrollPane) {
                ScrollBarUtil.doScroll(
                    scrollbar, ((JScrollPane) scrollbar.getParent()).getViewport(), e,
                    scrollbar.getParent().getComponentOrientation().isLeftToRight()
                );
            } else if (sp != null) {
                ScrollBarUtil.doScroll(
                    scrollbar, sp.getViewport(), e, scrollbar.getParent().getComponentOrientation().isLeftToRight()
                );
            } else {
                ScrollBarUtil.doScroll(scrollbar, null, e, scrollbar.getComponentOrientation().isLeftToRight());
            }
            scrollbar.setValueIsAdjusting(false);
        }
    }

    @Override
    public void mouseMoved(final MouseEvent e) {
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
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        if (!ui.getTrackBounds().contains(e.getPoint())) {
            mouseOverTrack = false;
            resetTrackAnimator();
        }
        if (!ui.getThumbBounds().contains(e.getPoint())) {
            mouseOverThumb = false;
            resetThumbAnimator();
        }
    }

    @Override
    public void mouseEntered(final MouseEvent e) {
        if (ui.getThumbBounds().isEmpty()) {
            return;
        }
        mouseOverTrack = true;
        if (!scrollbar.getValueIsAdjusting()) {
            resetTrackAnimator();
        }
        mouseMoved(e);
    }

    @Override
    public void mouseExited(final MouseEvent e) {
        if (ui.getThumbBounds().isEmpty()) {
            return;
        }
        mouseOverTrack = false;
        if (!scrollbar.getValueIsAdjusting()) {
            resetTrackAnimator();
        }
        mouseMoved(e);
    }

    @Override
    public void adjustmentValueChanged(final AdjustmentEvent e) {
        if (!e.getValueIsAdjusting()) return;

        JScrollBar scrollBar = (JScrollBar) e.getAdjustable();
        int extent = scrollBar.getModel().getExtent();
        int value = scrollBar.getValue() + extent;
        if (value == extent || value == scrollBar.getMaximum()) return;

        Point p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, scrollbar);
        if (!ui.getThumbBounds().contains(p) && !e.getValueIsAdjusting()) {
            if (!thumbFadeinAnimator.isRunning()) {
                mouseOverThumb = true;
                resetThumbAnimator();
            }
        }
    }

    protected boolean isOverThumb(final Point p) {
        final Rectangle bounds = ui.getThumbBounds();
        return bounds != null && bounds.contains(p);
    }

    protected void resetThumbAnimator() {
        resetAnimators(thumbFadeinAnimator, thumbFadeoutAnimator, mouseOverThumb, thumbAlpha, MAX_THUMB_ALPHA);
    }

    protected void resetTrackAnimator() {
        resetAnimators(trackFadeinAnimator, trackFadeoutAnimator, mouseOverTrack, trackAlpha, MAX_TRACK_ALPHA);
    }

    protected void resetAnimators(
            final Animator fadeInAnimator, final Animator fadeOutAnimator, final boolean overAnimatedComponent,
            final float currentAlpha, final float maxAlpha
    ) {
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
                startFrame = (int) ((1.0 - currentAlpha / maxAlpha) * (fadeOutAnimator.getTotalFrames()));
            }
            fadeOutAnimator.resume(startFrame);
        }
    }

    protected Animator createTrackFadeoutAnimator() {
        return new TrackFadeOutAnimator();
    }

    protected Animator createThumbFadeoutAnimator() {
        return new ThumbFadeOutAnimator();
    }

    protected Animator createTrackFadeinAnimator() {
        return new TrackFadeInAnimator();
    }

    protected Animator createThumbFadeinAnimator() {
        return new ThumbFadeInAnimator();
    }

    protected class TrackFadeOutAnimator extends Animator {
        public TrackFadeOutAnimator() {
            super(
                "Track fadeout", DarkScrollBarListener.FADEOUT_FRAMES_COUNT,
                DarkScrollBarListener.FADEOUT_FRAMES_COUNT * DarkScrollBarListener.FADEOUT_FRAME_COUNT_FACTOR, false
            );
        }

        public void paintNow(final int frame, final int totalFrames, final int cycle) {
            trackAlpha = MAX_TRACK_ALPHA;
            if (frame > DELAY_FRAMES) {
                trackAlpha *= (float) (1 - (double) frame / totalFrames);
            }
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }

        @Override
        protected void paintCycleEnd() {
            trackAlpha = 0;
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }
    }

    protected class ThumbFadeInAnimator extends Animator {
        public ThumbFadeInAnimator() {
            super(
                "Thumb fadein", DarkScrollBarListener.FADEIN_FRAMES_COUNT / 2,
                DarkScrollBarListener.FADEIN_FRAMES_COUNT * DarkScrollBarListener.FADEIN_FRAME_COUNT_FACTOR, false
            );
        }

        @Override
        public void paintNow(final int frame, final int totalFrames, final int cycle) {
            thumbAlpha = ((float) frame * MAX_THUMB_ALPHA) / totalFrames;
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }

        @Override
        protected void paintCycleEnd() {
            thumbAlpha = MAX_THUMB_ALPHA;
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
                Point p = MouseInfo.getPointerInfo().getLocation();
                SwingUtilities.convertPointFromScreen(p, scrollbar);
                if (!ui.getThumbBounds().contains(p) && !scrollbar.getValueIsAdjusting()) {
                    mouseOverThumb = false;
                    resetThumbAnimator();
                }
            }
        }
    }

    protected class TrackFadeInAnimator extends Animator {
        public TrackFadeInAnimator() {
            super(
                "Track fadein", DarkScrollBarListener.FADEIN_FRAMES_COUNT,
                DarkScrollBarListener.FADEIN_FRAMES_COUNT * DarkScrollBarListener.FADEIN_FRAME_COUNT_FACTOR, false
            );
        }

        public void paintNow(final int frame, final int totalFrames, final int cycle) {
            trackAlpha = ((float) frame * MAX_TRACK_ALPHA) / totalFrames;
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }

        @Override
        protected void paintCycleEnd() {
            trackAlpha = MAX_TRACK_ALPHA;
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }
    }

    protected class ThumbFadeOutAnimator extends Animator {
        public ThumbFadeOutAnimator() {
            super(
                "Adjustment fadeout", DarkScrollBarListener.FADEOUT_FRAMES_COUNT,
                DarkScrollBarListener.FADEOUT_FRAMES_COUNT * DarkScrollBarListener.FADEOUT_FRAME_COUNT_FACTOR, false
            );
        }

        @Override
        public void paintNow(final int frame, final int totalFrames, final int cycle) {
            thumbAlpha = MAX_THUMB_ALPHA;
            if (frame > DELAY_FRAMES) {
                thumbAlpha *= (float) (1 - (double) frame / totalFrames);
            }
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }

        @Override
        protected void paintCycleEnd() {
            thumbAlpha = 0;
            if (scrollbar != null) {
                ((JComponent) scrollbar.getParent()).paintImmediately(scrollbar.getBounds());
            }
        }
    }
}
