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
 *
 */
package com.github.weisj.darklaf.platform.macos;

import java.awt.*;
import java.util.Objects;
import java.util.logging.Logger;

import com.github.weisj.darklaf.platform.macos.darkmode.MacOSDarkMode;

public class MacOSPreferenceMonitor {

    private static final Logger LOGGER = Logger.getLogger(MacOSThemePreferenceProvider.class.getName());

    private final MacOSThemePreferenceProvider preferenceProvider;

    private boolean darkMode;
    private boolean highContrast;
    private Color accentColor;
    private Color selectionColor;
    private long listenerHandle;
    private boolean running;

    public MacOSPreferenceMonitor(final MacOSThemePreferenceProvider preferenceProvider) {
        this.preferenceProvider = preferenceProvider;
    }

    private void onNotification() {
        boolean newDark = MacOSDarkMode.isDarkThemeEnabled();
        boolean newHighContrast = JNIThemeInfoMacOS.isHighContrastEnabled();
        Color newAccentColor = JNIThemeInfoMacOS.getAccentColor();
        Color newSelectionColor = JNIThemeInfoMacOS.getSelectionColor();
        if (darkMode != newDark
            || highContrast != newHighContrast
            || !Objects.equals(accentColor, newAccentColor)
            || !Objects.equals(selectionColor, newSelectionColor)) {
            darkMode = newDark;
            accentColor = newAccentColor;
            selectionColor = newSelectionColor;
            highContrast = newHighContrast;
            preferenceProvider.reportPreferenceChange(highContrast, darkMode, accentColor, selectionColor);
        }
    }

    private void start() {
        darkMode = MacOSDarkMode.isDarkThemeEnabled();
        highContrast = JNIThemeInfoMacOS.isHighContrastEnabled();
        accentColor = JNIThemeInfoMacOS.getAccentColor();
        selectionColor = JNIThemeInfoMacOS.getSelectionColor();
        listenerHandle = JNIThemeInfoMacOS.createPreferenceChangeListener(this::onNotification);
        if (listenerHandle == 0) {
            LOGGER.severe("Could not create notification listener. Monitoring will not be started");
            return;
        }
        running = true;
        LOGGER.info("Started preference monitoring.");
    }

    private void stop() {
        if (!running) return;
        running = false;
        LOGGER.info("Stopped preference monitoring.");
        JNIThemeInfoMacOS.deletePreferenceChangeListener(listenerHandle);
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
