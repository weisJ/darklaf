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
import com.github.weisj.darklaf.platform.preferences.SystemPreferencesManager;
import com.github.weisj.darklaf.theme.event.ThemeEventSupport;
import com.github.weisj.darklaf.theme.event.ThemePreferenceChangeEvent;
import com.github.weisj.darklaf.theme.event.ThemePreferenceListener;
import com.github.weisj.darklaf.theme.spec.PreferredThemeStyle;
import com.github.weisj.darklaf.util.PropertyUtil;

public class ThemePreferencesHandler {

    public static final String PREFERENCE_REPORTING_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "enableNativePreferences";

    private static ThemePreferencesHandler sharedInstance;
    private final ThemeEventSupport<ThemePreferenceChangeEvent, ThemePreferenceListener> changeSupport =
            new ThemeEventSupport<>();
    private final SystemPreferencesManager systemPreferencesManager;

    public static ThemePreferencesHandler getSharedInstance() {
        if (sharedInstance == null) setSharedInstance(new ThemePreferencesHandler());
        return sharedInstance;
    }

    public static void setSharedInstance(final ThemePreferencesHandler handler) {
        ThemePreferencesHandler.sharedInstance = handler;
    }

    protected ThemePreferencesHandler() {
        systemPreferencesManager = new SystemPreferencesManager(isNativePreferencesEnabled());
        systemPreferencesManager.addListener(this::onChange);
    }

    private void onChange(final PreferredThemeStyle style) {
        SwingUtilities.invokeLater(() -> changeSupport.dispatchEvent(new ThemePreferenceChangeEvent(style)));
    }

    public void addThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        changeSupport.addListener(listener);
    }

    public void removeThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        changeSupport.removeListener(listener);
    }

    public void enablePreferenceChangeReporting(final boolean enabled) {
        systemPreferencesManager.enableReporting(enabled);
    }

    private boolean isNativePreferencesEnabled() {
        return PropertyUtil.getSystemFlag(PREFERENCE_REPORTING_FLAG)
                && PropertyUtil.getSystemFlag(DarkLaf.ALLOW_NATIVE_CODE_FLAG);
    }

    public boolean isPreferenceChangeReportingEnabled() {
        return systemPreferencesManager.isReportingEnabled() && PropertyUtil.getSystemFlag(PREFERENCE_REPORTING_FLAG);
    }

    public boolean canReport() {
        return systemPreferencesManager.provider().canReport();
    }

    public boolean supportsNativeFontSize() {
        return systemPreferencesManager.provider().supportsNativeFontSize();
    }

    public boolean supportsNativeAccentColor() {
        return systemPreferencesManager.provider().supportsNativeAccentColor();
    }

    public boolean supportsNativeSelectionColor() {
        return systemPreferencesManager.provider().supportsNativeSelectionColor();
    }

    public boolean supportsNativeTheme() {
        return systemPreferencesManager.provider().supportsNativeTheme();
    }

    public PreferredThemeStyle getPreferredThemeStyle() {
        return systemPreferencesManager.getPreferredThemeStyle();
    }
}
