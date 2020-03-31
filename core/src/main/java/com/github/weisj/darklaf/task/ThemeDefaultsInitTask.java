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
package com.github.weisj.darklaf.task;

import com.github.weisj.darklaf.platform.Decorations;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.SystemInfo;

import javax.swing.*;
import javax.swing.text.html.HTMLEditorKit;
import javax.swing.text.html.StyleSheet;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

public class ThemeDefaultsInitTask implements DefaultsInitTask {

    private static final String GLOBAL_PREFIX = "global.";
    private static final String MAC_OS_MENU_BAR_KEY = "apple.laf.useScreenMenuBar";
    private final DefaultsInitTask userPreferenceInitTask = new UserPreferenceInitTask();

    @Override
    public void run(final Theme currentTheme, final Map<Object, Object> defaults) {
        if (!(defaults instanceof UIDefaults)) return;
        loadThemeDefaults(currentTheme, (UIDefaults) defaults);
    }

    private void loadThemeDefaults(final Theme currentTheme, final UIDefaults defaults) {
        Properties uiProps = new Properties();
        currentTheme.loadDefaults(uiProps, defaults);

        /*
         * User preferences need to be applied here so changes are applied to all
         * components that use the property.
         */
        userPreferenceInitTask.run(currentTheme, uiProps);

        currentTheme.loadGlobals(uiProps, defaults);
        installGlobals(uiProps, defaults);
        currentTheme.loadUIProperties(uiProps, defaults);
        currentTheme.loadIconProperties(uiProps, defaults);
        Decorations.loadDecorationProperties(uiProps, defaults);
        currentTheme.loadPlatformProperties(uiProps, defaults);
        adjustPlatformSpecifics(uiProps);
        defaults.putAll(uiProps);

        StyleSheet styleSheet = currentTheme.loadStyleSheet();
        new HTMLEditorKit().setStyleSheet(styleSheet);
    }

    private void installGlobals(final Properties uiProps, final Map<Object, Object> defaults) {
        final HashMap<String, Object> globalSettings = new HashMap<>();
        for (final Object key : uiProps.keySet()) {
            if (key instanceof String && ((String) key).startsWith(GLOBAL_PREFIX)) {
                globalSettings.put(((String) key).substring(GLOBAL_PREFIX.length()), uiProps.get(key));
            }
        }

        for (final Object key : defaults.keySet()) {
            if (key instanceof String && ((String) key).contains(".")) {
                final String s = (String) key;
                final String globalKey = s.substring(s.lastIndexOf('.') + 1);
                if (globalSettings.containsKey(globalKey)) {
                    defaults.put(key, globalSettings.get(globalKey));
                }
            }
        }
    }

    protected void adjustPlatformSpecifics(final Properties uiProps) {
        boolean useScreenMenuBar = Boolean.getBoolean(MAC_OS_MENU_BAR_KEY);
        // If user wants to use Apple menu bar, then we need to keep the default
        // component for MenuBarUI and MenuUI
        if (SystemInfo.isMac && useScreenMenuBar) {
            uiProps.remove("MenuBarUI");
        }
    }
}
