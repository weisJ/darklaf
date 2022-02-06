/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.preferences;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.platform.macos.MacOSThemePreferenceProvider;
import com.github.weisj.darklaf.platform.preferences.impl.DefaultThemePreferenceProvider;
import com.github.weisj.darklaf.platform.windows.WindowsThemePreferenceProvider;
import com.github.weisj.darklaf.theme.spec.PreferredThemeStyle;
import com.github.weisj.darklaf.theme.spec.ThemePreferenceProvider;

public class SystemPreferencesManager {

    private final List<SystemPreferenceChangeListener> listenerList = Collections.synchronizedList(new ArrayList<>());
    private final ThemePreferenceProvider preferenceProvider;

    public SystemPreferencesManager(final boolean allowNativeReporting) {
        preferenceProvider = createProvider(allowNativeReporting);
        preferenceProvider.setCallback(this::onPreferenceChange);
    }

    public SystemPreferencesManager() {
        this(true);
    }

    public ThemePreferenceProvider provider() {
        return preferenceProvider;
    }

    private void onPreferenceChange(final PreferredThemeStyle preferredThemeStyle) {
        synchronized (listenerList) {
            new ArrayList<>(listenerList).forEach(listener -> {
                if (listener != null) listener.onSystemPreferencesChanged(preferredThemeStyle);
            });
        }
    }

    public void addListener(final SystemPreferenceChangeListener listener) {
        listenerList.add(listener);
    }

    public void removeListener(final SystemPreferenceChangeListener listener) {
        listenerList.add(listener);
    }

    private static ThemePreferenceProvider createProvider(final boolean allowNativeReporting) {
        ThemePreferenceProvider nativeProvider;
        try {
            // Extend for different platforms.
            if (SystemInfo.isWindows10OrGreater && allowNativeReporting) {
                nativeProvider = new WindowsThemePreferenceProvider();
            } else if (SystemInfo.isMac && allowNativeReporting) {
                nativeProvider = new MacOSThemePreferenceProvider();
            } else {
                nativeProvider = new DefaultThemePreferenceProvider();
            }
        } catch (final Throwable e) {
            // If native modules are not available disable them.
            nativeProvider = new DefaultThemePreferenceProvider();
        }
        return nativeProvider;
    }

    /**
     * Enabled whether changes in the preferred theme style should be reported to
     * {@link SystemPreferenceChangeListener}s. On some platforms this setting may do nothing.
     *
     * <p>
     * Warning: If preference reporting is enabled it <b>needs</b> to be disabled for the classloader to
     * be eligible for garbage collection. This is only relevant if you actually need the classloader to
     * be unloaded.
     *
     * @param reportingEnabled true if changes should be reported.
     */
    public void enableReporting(final boolean reportingEnabled) {
        if (isReportingEnabled() != reportingEnabled) {
            preferenceProvider.setReporting(reportingEnabled);
            onPreferenceChange(getPreferredThemeStyle());
        }
    }

    public boolean isReportingEnabled() {
        return preferenceProvider.canReport() && preferenceProvider.isReporting();
    }

    public PreferredThemeStyle getPreferredThemeStyle() {
        return preferenceProvider.getPreference();
    }
}
