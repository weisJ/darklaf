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
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.Animator;
import com.github.weisj.darklaf.graphics.DefaultInterpolator;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkScrollBarListener<T extends DarkScrollBarUI> extends MouseAdapter
        implements AdjustmentListener, ScrollBarConstants {

    private static final float MAX_TRACK_ALPHA = 0.3f;
    private static final float MAX_THUMB_ALPHA = 0.7f;

    protected static final int FADE_RESOLUTION = 10;

    protected static final int TRACK_FADE_OUT_DURATION = 400;
    protected static final int TRACK_FADE_OUT_DELAY = 100;
    protected static final int TRACK_FADE_IN_DURATION = 200;
    protected static final int TRACK_FADE_IN_DELAY = 0;

    protected static final int THUMB_FADE_OUT_DURATION = 400;
    protected static final int THUMB_FADE_OUT_DELAY = 100;
    protected static final int THUMB_FADE_IN_DURATION = 200;
    protected static final int THUMB_FADE_IN_DELAY = 0;


    protected final JScrollBar scrollbar;
    protected final T ui;

    private final Animator trackFadeoutAnimator;
    private final Animator trackFadeinAnimator;
    private final Animator thumbFadeoutAnimator;
    private final Animator thumbFadeinAnimator;

    protected boolean mouseOverThumb = false;
    protected boolean mouseOverTrack = false;
    protected float trackState;
    protected float thumbState;
    protected float trackAlpha;
    protected float thumbAlpha;

    public DarkScrollBarListener(final JScrollBar scrollbar, final T ui) {
        this.scrollbar = scrollbar;
        this.ui = ui;
        boolean animationsEnabled = UIManager.getBoolean("ScrollBar.animated");
        trackFadeoutAnimator = createTrackFadeoutAnimator();
        trackFadeinAnimator = createTrackFadeinAnimator();
        thumbFadeoutAnimator = createThumbFadeoutAnimator();
        thumbFadeinAnimator = createThumbFadeinAnimator();
        if (trackFadeoutAnimator != null) trackFadeoutAnimator.setEnabled(animationsEnabled);
        if (trackFadeinAnimator != null) trackFadeinAnimator.setEnabled(animationsEnabled);
        if (thumbFadeoutAnimator != null) thumbFadeoutAnimator.setEnabled(animationsEnabled);
        if (thumbFadeinAnimator != null) thumbFadeinAnimator.setEnabled(animationsEnabled);
    }

    public void uninstall() {
        dispose(trackFadeoutAnimator);
        dispose(thumbFadeoutAnimator);
        dispose(trackFadeinAnimator);
        dispose(trackFadeoutAnimator);
    }

    private void dispose(final Animator animator) {
        if (animator != null) animator.dispose();
    }

    public float getTrackAlpha() {
        return trackAlpha;
    }

    public float getTrackState() {
        return trackState;
    }

    public float getThumbState() {
        return thumbState;
    }

    public float getThumbAlpha() {
        return thumbAlpha;
    }

    @Override
    public void mouseWheelMoved(final MouseWheelEvent e) {
        if (scrollbar.getParent() instanceof JScrollPane
                && !((JScrollPane) scrollbar.getParent()).isWheelScrollingEnabled()) {
            return;
        }
        if ((scrollbar.getOrientation() == JScrollBar.VERTICAL && !e.isShiftDown())
                || (scrollbar.getOrientation() == JScrollBar.HORIZONTAL && e.isShiftDown())) {
            scrollbar.setValueIsAdjusting(true);
            JScrollPane sp = PropertyUtil.getObject(scrollbar, KEY_SCROLL_PANE_PARENT, JScrollPane.class);
            if (scrollbar.getParent() instanceof JScrollPane) {
                ScrollBarUtil.doScroll(scrollbar, ((JScrollPane) scrollbar.getParent()).getViewport(), e,
                        scrollbar.getParent().getComponentOrientation().isLeftToRight());
            } else if (sp != null) {
                ScrollBarUtil.doScroll(scrollbar, sp.getViewport(), e,
                        scrollbar.getParent().getComponentOrientation().isLeftToRight());
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

        boolean animateTrack = animateTrackOnScroll(scrollBar);
        boolean animateThumb = animateThumbOnScroll(scrollBar);

        if (animateTrack || animateThumb) {
            int extent = scrollBar.getModel().getExtent();
            int value = scrollBar.getValue() + extent;
            if (value == extent || value == scrollBar.getMaximum()) return;

            Point p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, scrollbar);
            if (animateTrack && !scrollBar.contains(p)) {
                runOnScrollTrackAnimation();
            }
            if (animateThumb && !ui.getThumbBounds().contains(p)) {
                runOnScrollThumbAnimation();
            }
        }
    }

    protected void runOnScrollTrackAnimation() {
        if (!trackFadeinAnimator.isRunning()) {
            mouseOverTrack = true;
            resetTrackAnimator();
        }
    }

    protected void runOnScrollThumbAnimation() {
        if (!thumbFadeinAnimator.isRunning()) {
            mouseOverThumb = true;
            resetThumbAnimator();
        }
    }

    protected boolean isOverThumb(final Point p) {
        final Rectangle bounds = ui.getThumbBounds();
        return bounds != null && bounds.contains(p);
    }

    protected void resetThumbAnimator() {
        if (thumbFadeinAnimator == null || thumbFadeoutAnimator == null) return;
        resetAnimators(thumbFadeinAnimator, thumbFadeoutAnimator, mouseOverThumb, thumbAlpha, MAX_THUMB_ALPHA);
    }

    protected void resetTrackAnimator() {
        if (trackFadeinAnimator == null || trackFadeoutAnimator == null) return;
        resetAnimators(trackFadeinAnimator, trackFadeoutAnimator, mouseOverTrack, trackAlpha, MAX_TRACK_ALPHA);
    }

    protected void resetAnimators(final Animator fadeInAnimator, final Animator fadeOutAnimator,
            final boolean overAnimatedComponent, final float currentAlpha, final float maxAlpha) {
        boolean fadeInRunning = fadeInAnimator.isRunning();
        boolean fadeOutRunning = fadeInAnimator.isRunning();
        fadeInAnimator.reset();
        fadeOutAnimator.reset();
        if (scrollbar != null && (scrollbar.getValueIsAdjusting() || overAnimatedComponent)) {
            fadeOutAnimator.suspend();
            int startFrame = (int) ((currentAlpha / maxAlpha) * fadeInAnimator.getTotalFrames());

            fadeInAnimator.resume(startFrame, fadeOutRunning, scrollbar);
        } else {
            fadeInAnimator.suspend();
            int startFrame = 0;
            if (currentAlpha < maxAlpha) {
                startFrame = (int) ((1.0 - currentAlpha / maxAlpha) * fadeOutAnimator.getTotalFrames());
            }
            fadeOutAnimator.resume(startFrame, fadeInRunning, scrollbar);
        }
    }

    protected boolean animateTrackOnScroll(final JScrollBar scrollbar) {
        return false;
    }

    protected boolean animateThumbOnScroll(final JScrollBar scrollbar) {
        return PropertyUtil.getBooleanProperty(scrollbar, KEY_HIGHLIGHT_ON_SCROLL);
    }

    protected int getFadeResolution() {
        return FADE_RESOLUTION;
    }

    protected int getTrackFadeOutDuration() {
        return TRACK_FADE_OUT_DURATION;
    }

    protected int getTrackFadeOutDelay() {
        return TRACK_FADE_OUT_DELAY;
    }

    protected int getTrackFadeInDuration() {
        return TRACK_FADE_IN_DURATION;
    }

    protected int getTrackFadeInDelay() {
        return TRACK_FADE_IN_DELAY;
    }

    protected int getThumbFadeOutDuration() {
        return THUMB_FADE_OUT_DURATION;
    }

    protected int getThumbFadeOutDelay() {
        return THUMB_FADE_OUT_DELAY;
    }

    protected int getThumbFadeInDuration() {
        return THUMB_FADE_IN_DURATION;
    }

    protected int getThumbFadeInDelay() {
        return THUMB_FADE_IN_DELAY;
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

    protected class TrackFadeInAnimator extends SBAnimator {

        public TrackFadeInAnimator() {
            super(getTrackFadeInDuration(), getFadeResolution(), getTrackFadeInDelay(), scrollbar, 0, MAX_TRACK_ALPHA,
                    true);
        }

        @Override
        protected void updateValue(final float state, final float value) {
            trackAlpha = value;
            trackState = state;
        }
    }

    protected class TrackFadeOutAnimator extends SBAnimator {

        public TrackFadeOutAnimator() {
            super(getTrackFadeOutDuration(), getFadeResolution(), getTrackFadeOutDelay(), scrollbar, 0, MAX_TRACK_ALPHA,
                    false);
        }

        @Override
        protected void updateValue(final float state, final float value) {
            trackAlpha = value;
            trackState = state;
        }
    }

    protected class ThumbFadeInAnimator extends SBAnimator {

        public ThumbFadeInAnimator() {
            super(getThumbFadeInDuration(), getFadeResolution(), getThumbFadeInDelay(), scrollbar, 0, MAX_THUMB_ALPHA,
                    true);
        }

        @Override
        protected void updateValue(final float state, final float value) {
            thumbAlpha = value;
            thumbState = state;
        }

        @Override
        protected void paintCycleEnd() {
            super.paintCycleEnd();
            if (scrollbar == null) return;
            Point p = MouseInfo.getPointerInfo().getLocation();
            SwingUtilities.convertPointFromScreen(p, scrollbar);
            if (!ui.getThumbBounds().contains(p) && !scrollbar.getValueIsAdjusting()) {
                mouseOverThumb = false;
                resetThumbAnimator();
            }
        }
    }

    protected class ThumbFadeOutAnimator extends SBAnimator {

        public ThumbFadeOutAnimator() {
            super(getThumbFadeOutDuration(), getFadeResolution(), getThumbFadeOutDelay(), scrollbar, 0, MAX_THUMB_ALPHA,
                    false);
        }

        @Override
        protected void updateValue(final float state, final float value) {
            thumbAlpha = value;
            thumbState = state;
        }
    }

    protected abstract static class SBAnimator extends Animator {

        private final JComponent component;
        private final float minValue;
        private final float maxValue;
        private final boolean fadeIn;

        public SBAnimator(final int duration, final int resolution, final int delay,
                final JComponent component, final float minValue, final float maxValue, final boolean fadeIn) {
            super(duration / resolution, duration, delay, false, true,
                    fadeIn ? DefaultInterpolator.EASE_OUT_CUBIC : DefaultInterpolator.EASE_IN_CUBIC);
            this.component = component;
            this.minValue = minValue;
            this.maxValue = maxValue;
            this.fadeIn = fadeIn;
        }

        protected abstract void updateValue(final float state, final float value);

        @Override
        public void paintNow(final float fraction) {
            float fr = fadeIn ? fraction : (1 - fraction);
            updateValue(fr, minValue + maxValue * fr);
            repaint();
        }

        @Override
        protected void paintCycleEnd() {
            updateValue(fadeIn ? 1 : 0, fadeIn ? maxValue : minValue);
            repaint();
        }

        private void repaint() {
            if (component != null) {
                ((JComponent) component.getParent()).paintImmediately(component.getBounds());
                component.getParent().repaint();
            }
        }

    }
}
