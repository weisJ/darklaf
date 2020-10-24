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
package com.github.weisj.darklaf.graphics;

import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.*;

/** @author Konstantin Bulenkov */
public abstract class Animator {
    private static final ScheduledExecutorService scheduler = createScheduler();

    private final int totalFrames;
    private final int cycleDuration;
    private final boolean repeatable;
    private final int delayFrames;

    private boolean forward;

    private Interpolator interpolator;

    private ScheduledFuture<?> ticker;
    private int startFrame;
    private int currentFrame;
    private long startTime;
    private long stopTime;
    private volatile boolean disposed = false;

    public Animator(final int totalFrames, final int cycleDuration, final int delayFrames) {
        this(totalFrames, cycleDuration, delayFrames, false);
    }

    public Animator(final int totalFrames, final int cycleDuration, final int delayFrames, final boolean repeatable) {
        this(totalFrames, cycleDuration, delayFrames, repeatable, true, DefaultInterpolator.LINEAR);
    }

    public Animator(final int totalFrames, final int cycleDuration, final boolean repeatable) {
        this(totalFrames, cycleDuration, 0, repeatable, true, DefaultInterpolator.LINEAR);
    }

    public Animator(final int totalFrames, final int cycleDuration, final boolean repeatable,
            final Interpolator interpolator) {
        this(totalFrames, cycleDuration, 0, repeatable, true, interpolator);
    }

    public Animator(final int totalFrames, final int cycleDuration, final int delayFrames, final boolean repeatable,
            final boolean forward, final Interpolator interpolator) {
        this.totalFrames = totalFrames;
        this.cycleDuration = cycleDuration;
        this.delayFrames = delayFrames;
        this.repeatable = repeatable;
        this.forward = forward;
        this.interpolator = interpolator;
        currentFrame = forward ? 0 : totalFrames;
        resetTime();
        reset();
    }

    public void setForward(final boolean forward) {
        this.forward = forward;
    }

    private void resetTime() {
        startTime = -1;
    }

    public void reset() {
        currentFrame %= totalFrames;
    }

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

    public void suspend() {
        resetTime();
        reset();
        stopTicker();
    }

    public void stopTicker() {
        if (ticker != null) {
            ticker.cancel(false);
            ticker = null;
        }
    }

    public void resume() {
        resume(0);
    }

    public void resume(final int startFrame) {
        if (startFrame < 0) {
            throw new IllegalArgumentException("Starting frame must be non negative.");
        }
        if (cycleDuration == 0 || startFrame >= totalFrames) {
            currentFrame = totalFrames - 1;
            paint();
            animationDone();
        } else if (ticker == null) {
            this.startFrame = startFrame;
            ticker = scheduler.scheduleWithFixedDelay(new Runnable() {
                final AtomicBoolean isScheduled = new AtomicBoolean(false);

                @Override
                public void run() {
                    if (!isScheduled.get() && !isDisposed()) {
                        isScheduled.set(true);
                        SwingUtilities.invokeLater(() -> {
                            onTick();
                            isScheduled.set(false);
                        });
                    }
                }
            }, 0, cycleDuration * 1000L / totalFrames, TimeUnit.MICROSECONDS);
        }
    }

    private void paint() {
        int frame = forward ? currentFrame : totalFrames - currentFrame - 1;
        if (frame > delayFrames) {
            paintNow(interpolator.interpolate(((float) frame) / totalFrames));
        }
    }

    private void animationDone() {
        stopTicker();
        SwingUtilities.invokeLater(this::paintCycleEnd);
    }

    public boolean isDisposed() {
        return disposed;
    }

    private void onTick() {
        if (isDisposed() || ticker == null) return;

        if (startTime == -1) {
            startTime = System.currentTimeMillis();
            stopTime = startTime + (((long) cycleDuration) * (totalFrames - currentFrame)) / totalFrames;
        }

        final double passedTime = System.currentTimeMillis() - startTime;
        final double totalTime = stopTime - startTime;

        final int newFrame = (int) (passedTime * totalFrames / totalTime) + startFrame;
        if (currentFrame > 0 && newFrame == currentFrame) return;
        currentFrame = newFrame;

        if (currentFrame >= totalFrames) {
            if (repeatable) {
                reset();
            } else {
                animationDone();
                return;
            }
        }

        paint();
    }

    public abstract void paintNow(float fraction);

    protected void paintCycleEnd() {}

    public void dispose() {
        disposed = true;
        stopTicker();
    }

    public boolean isRunning() {
        return ticker != null;
    }

    public final boolean isForward() {
        return forward;
    }

    public int getCurrentFrame() {
        return currentFrame;
    }

    public int getTotalFrames() {
        return totalFrames;
    }

    public Interpolator getInterpolator() {
        return interpolator;
    }

    public void setInterpolator(final Interpolator interpolator) {
        this.interpolator = interpolator;
    }
}
