package com.weis.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;


/**
 * @author Konstantin Bulenkov
 */
public abstract class Animator {
    private final static ScheduledExecutorService scheduler = createScheduler();
    private final int totalFrames;
    private final int cycleDuration;
    private final boolean forward;
    private final boolean repeatable;
    private ScheduledFuture<?> ticker;
    private int startFrame;
    private int currentFrame;
    private long startTime;
    private long stopTime;
    private volatile boolean disposed = false;

    public Animator(final String name,
                    final int totalFrames,
                    final int cycleDuration,
                    final boolean repeatable) {

        this(name, totalFrames, cycleDuration, repeatable, true);
    }

    public Animator(final String name,
                    final int totalFrames,
                    final int cycleDuration,
                    final boolean repeatable,
                    final boolean forward) {
        this.totalFrames = totalFrames;
        this.cycleDuration = cycleDuration;
        this.repeatable = repeatable;
        this.forward = forward;
        currentFrame = forward ? 0 : totalFrames;

        reset();
    }

    @NotNull
    private static ScheduledExecutorService createScheduler() {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, r -> {
            final Thread thread = new Thread(r, "Darcula Animations");
            thread.setDaemon(true);
            thread.setPriority(Thread.NORM_PRIORITY);
            return thread;
        });
        executor.setContinueExistingPeriodicTasksAfterShutdownPolicy(false);
        executor.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        return executor;

    }

    private void onTick() {
        if (isDisposed()) return;

        if (startTime == -1) {
            startTime = System.currentTimeMillis();
            stopTime = startTime + cycleDuration * (totalFrames - currentFrame) / totalFrames;
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

    private void paint() {
        paintNow(forward ? currentFrame : totalFrames - currentFrame - 1, totalFrames, cycleDuration);
    }

    private void animationDone() {
        stopTicker();

        SwingUtilities.invokeLater(this::paintCycleEnd);
    }

    private void stopTicker() {
        if (ticker != null) {
            ticker.cancel(false);
            ticker = null;
        }
    }

    protected void paintCycleEnd() {

    }

    public void suspend() {
        startTime = -1;
        stopTicker();
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
                    if (isScheduled.compareAndSet(false, true) && !isDisposed()) {
                        SwingUtilities.invokeLater(() -> {
                            isScheduled.set(false);
                            onTick();
                        });
                    }
                }
            }, 0, cycleDuration * 1000 / totalFrames, TimeUnit.MICROSECONDS);
        }
    }

    public abstract void paintNow(int frame, int totalFrames, int cycle);

    public void dispose() {
        disposed = true;
        stopTicker();
    }

    public boolean isRunning() {
        return ticker != null;
    }

    public void reset() {
        currentFrame = 0;
        startTime = -1;
    }

    @Contract(pure = true)
    public final boolean isForward() {
        return forward;
    }

    public boolean isDisposed() {
        return disposed;
    }

    public int getCurrentFrame() {
        return currentFrame;
    }

    public float getTotalFrames() {
        return totalFrames;
    }
}
