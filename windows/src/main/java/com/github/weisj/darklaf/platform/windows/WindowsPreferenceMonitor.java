/*
 * MIT License
 *
 * Copyright (c) 2020-2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.windows;

import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.util.LogUtil;

public class WindowsPreferenceMonitor {

    private static final Logger LOGGER = LogUtil.getLogger(WindowsThemePreferenceProvider.class);

    private final WindowsThemePreferenceProvider preferenceProvider;

    private boolean darkMode;
    private boolean highContrast;
    private long fontScaleFactor;
    private long eventHandler;
    private int color;
    private boolean running;

    public WindowsPreferenceMonitor(final WindowsThemePreferenceProvider preferenceProvider) {
        this.preferenceProvider = preferenceProvider;
    }

    private void onNotification() {
        SwingUtilities.invokeLater(() -> {
            boolean newDark = JNIThemeInfoWindows.isDarkThemeEnabled();
            boolean newHighContrast = JNIThemeInfoWindows.isHighContrastEnabled();
            long newFotScale = JNIThemeInfoWindows.getFontScaleFactor();
            int newColor = JNIThemeInfoWindows.getAccentColor();
            if (darkMode != newDark || color != newColor || fontScaleFactor != newFotScale
                    || highContrast != newHighContrast) {
                darkMode = newDark;
                fontScaleFactor = newFotScale;
                highContrast = newHighContrast;
                color = newColor;
                preferenceProvider.reportPreferenceChange(highContrast, darkMode, fontScaleFactor, color);
            }
        });
    }

    private void start() {
        darkMode = JNIThemeInfoWindows.isDarkThemeEnabled();
        highContrast = JNIThemeInfoWindows.isHighContrastEnabled();
        fontScaleFactor = JNIThemeInfoWindows.getFontScaleFactor();
        color = JNIThemeInfoWindows.getAccentColor();
        eventHandler = JNIThemeInfoWindows.createEventHandler(this::onNotification);
        if (eventHandler == 0) {
            LOGGER.severe("Could not create notification listener. Monitoring will not be started");
            return;
        }
        running = true;
        LOGGER.info("Started preference monitoring.");
    }

    private void stop() {
        if (!running) return;
        LOGGER.info("Stopped preference monitoring.");
        running = false;
        JNIThemeInfoWindows.deleteEventHandler(eventHandler);
    }

    public void setRunning(final boolean running) {
        if (running == isRunning()) return;
        if (running) {
            start();
        } else {
            stop();
        }
    }

    public boolean isRunning() {
        return running;
    }
}
