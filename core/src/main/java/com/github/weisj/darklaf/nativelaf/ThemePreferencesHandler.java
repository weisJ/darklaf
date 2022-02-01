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
package com.github.weisj.darklaf.nativelaf;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.platform.preferences.NativeThemePreferenceProvider;
import com.github.weisj.darklaf.theme.event.ThemeEventSupport;
import com.github.weisj.darklaf.theme.event.ThemePreferenceChangeEvent;
import com.github.weisj.darklaf.theme.event.ThemePreferenceListener;
import com.github.weisj.darklaf.theme.spec.PreferredThemeStyle;
import com.github.weisj.darklaf.theme.spec.ThemePreferenceProvider;
import com.github.weisj.darklaf.util.PropertyUtil;

public class ThemePreferencesHandler {

    public static final String PREFERENCE_REPORTING_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "enableNativePreferences";

    private static ThemePreferencesHandler sharedInstance;
    private final ThemeEventSupport<ThemePreferenceChangeEvent, ThemePreferenceListener> changeSupport =
            new ThemeEventSupport<>();
    private final ThemePreferenceProvider preferenceProvider;

    public static ThemePreferencesHandler getSharedInstance() {
        if (sharedInstance == null) setSharedInstance(new ThemePreferencesHandler());
        return sharedInstance;
    }

    public static void setSharedInstance(final ThemePreferencesHandler handler) {
        ThemePreferencesHandler.sharedInstance = handler;
    }

    protected ThemePreferencesHandler() {
        if (isNativePreferencesEnabled()) {
            preferenceProvider = NativeThemePreferenceProvider.create();
        } else {
            preferenceProvider = NativeThemePreferenceProvider.createNullProvider();

        }
        preferenceProvider.initialize();
        preferenceProvider.setCallback(this::onChange);
    }

    private void onChange(final PreferredThemeStyle style) {
        SwingUtilities.invokeLater(() -> {
            changeSupport.dispatchEvent(new ThemePreferenceChangeEvent(style));
        });
    }

    public void addThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        changeSupport.addListener(listener);
    }

    public void removeThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        changeSupport.removeListener(listener);
    }

    public void enablePreferenceChangeReporting(final boolean enabled) {
        preferenceProvider.setReporting(enabled);
    }

    private boolean isNativePreferencesEnabled() {
        return PropertyUtil.getSystemFlag(PREFERENCE_REPORTING_FLAG)
                && PropertyUtil.getSystemFlag(DarkLaf.ALLOW_NATIVE_CODE_FLAG);
    }

    public boolean isPreferenceChangeReportingEnabled() {
        return preferenceProvider.canReport() && preferenceProvider.isReporting()
                && PropertyUtil.getSystemFlag(PREFERENCE_REPORTING_FLAG);
    }

    public boolean supportsNativeAccentColor() {
        return preferenceProvider.supportsNativeAccentColor();
    }

    public boolean supportsNativeSelectionColor() {
        return preferenceProvider.supportsNativeSelectionColor();
    }

    public boolean supportsNativeFontSize() {
        return preferenceProvider.supportsNativeFontSize();
    }

    public boolean supportsNativeTheme() {
        return preferenceProvider.supportsNativeTheme();
    }

    public PreferredThemeStyle getPreferredThemeStyle() {
        return preferenceProvider.getPreference();
    }

    public boolean canReport() {
        return preferenceProvider.canReport();
    }
}
