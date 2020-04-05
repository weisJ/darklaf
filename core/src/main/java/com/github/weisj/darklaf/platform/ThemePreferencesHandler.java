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
package com.github.weisj.darklaf.platform;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.platform.macos.MacOSThemePreferenceProvider;
import com.github.weisj.darklaf.platform.windows.WindowsThemePreferenceProvider;
import com.github.weisj.darklaf.theme.event.ThemePreferenceChangeEvent;
import com.github.weisj.darklaf.theme.event.ThemePreferenceChangeSupport;
import com.github.weisj.darklaf.theme.event.ThemePreferenceListener;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;
import com.github.weisj.darklaf.theme.info.ThemePreferenceProvider;
import com.github.weisj.darklaf.util.PropertyValue;
import com.github.weisj.darklaf.util.SystemInfo;

public class ThemePreferencesHandler {

    public static final String PREFERENCE_REPORTING_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "enableNativePreferences";

    private static ThemePreferencesHandler sharedInstance;
    private final ThemePreferenceChangeSupport changeSupport = new ThemePreferenceChangeSupport();
    private ThemePreferenceProvider preferenceProvider;


    public static ThemePreferencesHandler getSharedInstance() {
        if (sharedInstance == null) setSharedInstance(new ThemePreferencesHandler());
        return sharedInstance;
    }

    public static void setSharedInstance(final ThemePreferencesHandler handler) {
        ThemePreferencesHandler.sharedInstance = handler;
    }

    protected ThemePreferencesHandler() {
        try {
            // Extend for different platforms.
            boolean enableNativePreferences = isNativePreferencesEnabled();
            if (SystemInfo.isWindows10 && enableNativePreferences) {
                preferenceProvider = new WindowsThemePreferenceProvider();
            } else if (SystemInfo.isMac && enableNativePreferences) {
                preferenceProvider = new MacOSThemePreferenceProvider();
            } else {
                preferenceProvider = new DefaultThemePreferenceProvider();
            }
        } catch (Throwable e) {
            // If native modules are not available disable them.
            preferenceProvider = new DefaultThemePreferenceProvider();
        }
        preferenceProvider.initialize();
        preferenceProvider.setCallback(this::onChange);
    }

    private void onChange(final PreferredThemeStyle style) {
        changeSupport.notifyPreferenceChange(new ThemePreferenceChangeEvent(style));
    }

    public void addThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        changeSupport.addThemePreferenceChangeListener(listener);
    }

    public void removeThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        changeSupport.removeThemePreferenceChangeListener(listener);
    }

    public void enablePreferenceChangeReporting(final boolean enabled) {
        preferenceProvider.setReporting(enabled);
    }

    private boolean isNativePreferencesEnabled() {
        return !PropertyValue.FALSE.equals(System.getProperty(PREFERENCE_REPORTING_FLAG))
               && !PropertyValue.FALSE.equals(System.getProperty(DarkLaf.ALLOW_NATIVE_CODE_FLAG));
    }

    public boolean isPreferenceChangeReportingEnabled() {
        return preferenceProvider.isReporting()
               && !PropertyValue.FALSE.equals(System.getProperty(PREFERENCE_REPORTING_FLAG));
    }

    public PreferredThemeStyle getPreferredThemeStyle() {
        return preferenceProvider.getPreference();
    }
}
