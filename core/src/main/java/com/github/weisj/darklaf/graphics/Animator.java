/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
 */
package com.github.weisj.darklaf.graphics;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.util.PropertyUtil;

public abstract class Animator {

    public enum RepeatMode {
        DO_NOT_REPEAT,
        DO_NOT_REPEAT_FREEZE,
        CYCLE,
        CYCLE_FLIP;

        public boolean isRepeating() {
            return switch (this) {
                case DO_NOT_REPEAT, DO_NOT_REPEAT_FREEZE -> false;
                default -> true;
            };
        }
    }

    public static final String ANIMATIONS_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "animations";
    private static final ScheduledExecutorService scheduler = createScheduler();
    private static final int DEFAULT_FPS = 60;

    static ScheduledExecutorService scheduler() {
        return scheduler;
    }

    private final long animationDurationMillis;
    private final long delayMillis;
    private final int fps;

    private RepeatMode repeatMode;
    private Interpolator interpolator;
    private boolean reverse = false;

    private final AtomicBoolean isScheduled = new AtomicBoolean(false);
    private ScheduledFuture<?> ticker;

    private double fraction;
    private double fractionDelta;

    private boolean enabled = true;

    public Animator(final long animationDurationMillis, final int fps) {
        this(animationDurationMillis, fps, DefaultInterpolator.LINEAR);
    }

    public Animator(final long animationDurationMillis, final int fps, final Interpolator interpolator) {
        this(animationDurationMillis, fps, interpolator, RepeatMode.DO_NOT_REPEAT);
    }

    public Animator(final long animationDurationMillis) {
        this(animationDurationMillis, DEFAULT_FPS);
    }

    public Animator(final long animationDurationMillis, final Interpolator interpolator) {
        this(animationDurationMillis, DEFAULT_FPS, interpolator, RepeatMode.DO_NOT_REPEAT);
    }

    public Animator(final long animationDurationMillis, final int fps,
            final Interpolator interpolator, final RepeatMode repeatMode) {
        this(animationDurationMillis, 0, fps, interpolator, repeatMode);
    }

    public Animator(final long animationDurationMillis, final long delayMillis, final int fps,
            final Interpolator interpolator, final RepeatMode repeatMode) {
        this.animationDurationMillis = animationDurationMillis;
        this.delayMillis = delayMillis;
        this.fps = fps;
        this.interpolator = interpolator;
        this.repeatMode = repeatMode;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(final boolean enabled) {
        this.enabled = enabled;
    }

    public void setReverse(final boolean reverse) {
        this.reverse = reverse;
    }

    public boolean isReverse() {
        return reverse;
    }

    public void setInterpolator(final Interpolator interpolator) {
        this.interpolator = interpolator;
    }

    public Interpolator interpolator() {
        return interpolator;
    }

    public boolean isRunning() {
        return ticker != null;
    }

    public double currentState() {
        return fraction;
    }

    public RepeatMode repeatMode() {
        return repeatMode;
    }

    public void setRepeatMode(final RepeatMode repeatMode) {
        this.repeatMode = repeatMode;
    }

    private boolean animationsEnabled() {
        return enabled && PropertyUtil.getSystemFlag(ANIMATIONS_FLAG);
    }

    public void play() {
        resumeAt(0, true);
    }

    public void playForwards() {
        playForwards(true);
    }

    /**
     * Plays the animation ensuring it will go forward.
     */
    public void playForwards(boolean skipDelay) {
        if (reverse || !isRunning()) {
            pause();
            reverse = false;
            resumeAt(0, skipDelay);
        }
    }

    public void playBackwards() {
        playBackwards(true);
    }

    /**
     * Plays the animation ensuring it will go backward.
     */
    public void playBackwards(boolean skipDelay) {
        if (!reverse || !isRunning()) {
            pause();
            reverse = true;
            resumeAt(1, skipDelay);
        }
    }

    public void resume(final JComponent target) {
        resumeAt(fraction, true, target);
    }

    public void resumeAt(final double startFraction, final boolean skipDelay, final JComponent target) {
        if (target != null && (!target.isVisible() || !target.isShowing())) {
            stop();
            return;
        }
        resumeAt(startFraction, skipDelay);
    }

    public void resume() {
        resumeAt(fraction, true);
    }

    public void resumeAt(final double startFraction, final boolean skipDelay) {
        if (startFraction < 0 || startFraction > 1) {
            throw new IllegalArgumentException("Starting fraction must be between 0.0 and 1.0.");
        }

        if (animationDurationMillis == 0 || !animationsEnabled()) {
            stop();
            return;
        }

        if (ticker == null) {
            this.fractionDelta = 1f / (fps * animationDurationMillis / 1000f);

            long initialDelay = skipDelay ? 0 : delayMillis;

            long millisPerFrame = (long) (animationDurationMillis * (fps / 1000f));

            isScheduled.set(false);
            ticker = scheduler.scheduleWithFixedDelay(() -> {
                if (isRunning()) {
                    if (tick()) submitAnimationFrame();
                }
            }, initialDelay, millisPerFrame, TimeUnit.MILLISECONDS);
        }
    }

    private void submitAnimationFrame() {
        if (!isScheduled.get()) {
            isScheduled.set(true);
            SwingUtilities.invokeLater(() -> {
                if (isRunning()) paintAnimationFrame(interpolator.interpolate((float) fraction));
                isScheduled.set(false);
            });
        }
    }

    private boolean tick() {
        final double oldFraction = fraction;

        if (reverse) {
            fraction = oldFraction - fractionDelta;
        } else {
            fraction = oldFraction + fractionDelta;
        }

        switch (repeatMode) {
            case DO_NOT_REPEAT:
                if (fraction > 1 && !reverse) {
                    stop();
                    return false;
                }
                if (fraction < 0 && reverse) {
                    stop();
                    return false;
                }
                break;
            case DO_NOT_REPEAT_FREEZE:
                if (fraction > 1 && !reverse) pause();
                if (fraction < 0 && reverse) pause();
                break;
            case CYCLE:
                if (reverse) {
                    if (fraction < 0) {
                        fraction = 1 + fraction;
                    }
                } else {
                    if (fraction > 1) {
                        fraction = fraction - 1;
                    }
                }
                break;
            case CYCLE_FLIP:
                if (reverse) {
                    if (fraction < 0) {
                        fraction = -fraction;
                        reverse = false;
                    }
                } else {
                    if (fraction > 1) {
                        fraction = 1 - (fraction - Math.floor(fraction));
                        reverse = true;
                    }
                }
                break;
        }
        fraction = Math.max(Math.min(1, fraction), 0);
        return true;
    }

    /**
     * Stops the animation setting it back to the beginning and calls {@code onAnimationFinished()}.
     *
     * @see #cancel()
     */
    public void stop() {
        if (cancel()) {
            onAnimationFinished();
        }
    }

    /**
     * Stops the animation without calling {@code onAnimationFinished()}.
     *
     * @see #stop()
     * @return true if the animation was running
     */
    public boolean cancel() {
        if (pause()) {
            reset();
            return true;
        }
        reset();
        return false;
    }

    public void reset() {
        fraction = reverse ? 1 : 0;
    }

    /**
     * Pauses the animation. Can be resumed.
     *
     * @return true if the animation was running
     */
    public boolean pause() {
        if (ticker != null) {
            ticker.cancel(false);
            ticker = null;
            return true;
        }
        return false;
    }

    protected abstract void paintAnimationFrame(float fraction);

    protected void onAnimationFinished() {}

    @SuppressWarnings("ThreadPriorityCheck")
    private static ScheduledExecutorService createScheduler() {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, r -> {
            final Thread thread = new Thread(r, "Animations Thread");
            thread.setDaemon(true);
            thread.setPriority(Thread.MAX_PRIORITY);
            return thread;
        });
        executor.setContinueExistingPeriodicTasksAfterShutdownPolicy(false);
        executor.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        return executor;
    }
}
