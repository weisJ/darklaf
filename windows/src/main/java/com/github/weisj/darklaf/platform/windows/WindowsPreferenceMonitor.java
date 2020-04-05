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
 */
package com.github.weisj.darklaf.platform.windows;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

public class WindowsPreferenceMonitor {

    private static final Logger LOGGER = Logger.getLogger(WindowsThemePreferenceProvider.class.getName());

    private final WindowsThemePreferenceProvider preferenceProvider;

    private boolean darkMode;
    private boolean highContrast;
    private long fontScaleFactor;
    private long eventHandle;
    private int color;
    private AtomicBoolean running = new AtomicBoolean(false);

    public WindowsPreferenceMonitor(final WindowsThemePreferenceProvider preferenceProvider) {
        this.preferenceProvider = preferenceProvider;
    }

    public void run() {
        LOGGER.info("Started preference monitoring.");
        while (running.get() && JNIThemeInfoWindows.waitPreferenceChange(eventHandle)) {
            boolean newDark = JNIThemeInfoWindows.isDarkThemeEnabled();
            boolean newHighContrast = JNIThemeInfoWindows.isHighContrastEnabled();
            long newFotScale = JNIThemeInfoWindows.getFontScaleFactor();
            int newColor = JNIThemeInfoWindows.getAccentColor();
            if (darkMode != newDark
                || color != newColor
                || fontScaleFactor != newFotScale
                || highContrast != newHighContrast) {
                darkMode = newDark;
                fontScaleFactor = newFotScale;
                highContrast = newHighContrast;
                color = newColor;
                preferenceProvider.reportPreferenceChange(highContrast, darkMode, fontScaleFactor, color);
            }
        }
        if (running.get()) {
            LOGGER.severe("Monitor encountered an error. Stopping preference monitoring.");
            running.set(false);
        } else {
            LOGGER.info("Stopped preference monitoring.");
        }
    }

    private void start() {
        darkMode = JNIThemeInfoWindows.isDarkThemeEnabled();
        highContrast = JNIThemeInfoWindows.isHighContrastEnabled();
        fontScaleFactor = JNIThemeInfoWindows.getFontScaleFactor();
        eventHandle = JNIThemeInfoWindows.createEventHandle();
        color = JNIThemeInfoWindows.getAccentColor();
        /*
         * In theory this shouldn't be necessary, but
         * it ensures that the registry listeners are actually unregistered.
         */
        Runtime.getRuntime().addShutdownHook(new Thread(this::stop));
        this.running.set(true);
        new Thread(this::run).start();
    }

    private void stop() {
        this.running.set(false);
        JNIThemeInfoWindows.notifyEventHandle(eventHandle);
    }

    public void setRunning(final boolean running) {
        if (running == this.running.get()) return;
        if (running) {
            start();
        } else {
            stop();
        }
    }

    public boolean isRunning() {
        return running.get();
    }
}
