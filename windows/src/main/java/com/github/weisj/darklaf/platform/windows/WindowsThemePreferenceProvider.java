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
package com.github.weisj.darklaf.platform.windows;

import java.awt.*;
import java.util.function.Consumer;

import com.github.weisj.darklaf.theme.info.*;

public class WindowsThemePreferenceProvider implements ThemePreferenceProvider {

    private final PreferredThemeStyle fallbackStyle = new PreferredThemeStyle(ContrastRule.STANDARD,
                                                                              ColorToneRule.LIGHT);
    private final WindowsPreferenceMonitor monitor = new WindowsPreferenceMonitor(this);
    private Consumer<PreferredThemeStyle> callback;

    @Override
    public PreferredThemeStyle getPreference() {
        if (!WindowsLibrary.get().isLoaded()) return fallbackStyle;
        boolean darkMode = JNIThemeInfoWindows.isDarkThemeEnabled();
        boolean highContrast = JNIThemeInfoWindows.isHighContrastEnabled();
        long fontScaling = JNIThemeInfoWindows.getFontScaleFactor();
        int accentColorRGB = JNIThemeInfoWindows.getAccentColor();
        return create(highContrast, darkMode, fontScaling, accentColorRGB);
    }

    private PreferredThemeStyle create(final boolean highContrast, final boolean darkMode,
                                       final long fontScaling, final int accentColorRGB) {
        ContrastRule contrastRule = highContrast ? ContrastRule.HIGH_CONTRAST : ContrastRule.STANDARD;
        ColorToneRule toneRule = darkMode ? ColorToneRule.DARK : ColorToneRule.LIGHT;
        FontSizeRule fontSizeRule = FontSizeRule.relativeAdjustment(fontScaling / 100f);
        AccentColorRule accentColorRule = AccentColorRule.fromColor(createColorFromRGB(accentColorRGB));
        return new PreferredThemeStyle(contrastRule, toneRule, accentColorRule, fontSizeRule);
    }

    private Color createColorFromRGB(final int rgb) {
        if (rgb == 0) return null;
        return new Color(rgb);
    }

    void reportPreferenceChange(final boolean highContrast, final boolean darkMode,
                                final long fontScaleFactor, final int accentColorRGB) {
        if (callback != null) {
            PreferredThemeStyle style = create(highContrast, darkMode, fontScaleFactor, accentColorRGB);
            callback.accept(style);
        }
    }

    @Override
    public void setReporting(final boolean reporting) {
        if (reporting && !WindowsLibrary.get().isLoaded()) WindowsLibrary.get().updateLibrary();
        synchronized (monitor) {
            monitor.setRunning(reporting);
        }
    }

    @Override
    public boolean isReporting() {
        return monitor.isRunning();
    }

    @Override
    public void initialize() {
        WindowsLibrary.get().updateLibrary();
    }

    @Override
    public void setCallback(final Consumer<PreferredThemeStyle> callback) {
        this.callback = callback;
    }

    @Override
    public boolean canReport() {
        return true;
    }

    @Override
    public boolean supportsNativeAccentColor() {
        return WindowsLibrary.get().isLoaded();
    }

    @Override
    public boolean supportsNativeFontSize() {
        return WindowsLibrary.get().isLoaded();
    }

    @Override
    public boolean supportsNativeTheme() {
        return WindowsLibrary.get().isLoaded();
    }
}
