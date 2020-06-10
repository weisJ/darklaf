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
package com.github.weisj.darklaf.task;

import java.util.HashMap;
import java.util.Properties;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.platform.DecorationsHandler;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.SystemInfo;

public class ThemeDefaultsInitTask implements DefaultsInitTask {

    private static final String GLOBAL_PREFIX = "global.";
    private static final String MAC_OS_MENU_BAR_KEY = "apple.laf.useScreenMenuBar";
    private static final String[] UI_PROPERTIES = new String[]{"borders", "button", "cell", "checkBox", "colorChooser",
                                                               "comboBox", "fileChooser", "tristate",
                                                               "internalFrame", "label", "list", "menu", "menuBar",
                                                               "menuItem", "numberingPane", "optionPane", "panel",
                                                               "popupMenu", "progressBar", "radioButton", "rootPane",
                                                               "scrollBar", "scrollPane", "separator",
                                                               "slider", "spinner", "splitPane", "statusBar",
                                                               "tabbedPane", "tabFrame", "table", "taskPane", "text",
                                                               "toggleButton", "toolBar", "toolTip", "tree", "misc"};
    private static final String[] ICON_PROPERTIES = new String[]{"checkBox", "radioButton", "slider", "dialog", "files",
                                                                 "frame", "indicator", "menu", "misc", "navigation"};
    private final DefaultsAdjustmentTask userPreferenceAdjustment = new UserDefaultsAdjustmentTask();
    private final DefaultsAdjustmentTask accentColorAdjustment = new AccentColorAdjustmentTask();
    private final DefaultsAdjustmentTask foregroundGeneration = new ForegroundColorGenerationTask();

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        loadThemeDefaults(currentTheme, defaults);
    }

    private void loadThemeDefaults(final Theme currentTheme, final UIDefaults defaults) {
        Properties uiProps = new Properties();
        currentTheme.loadDefaults(uiProps, defaults);

        /*
         * User preferences need to be applied here so changes are applied to all
         * components that use the property.
         */
        userPreferenceAdjustment.run(currentTheme, uiProps);

        /*
         * Adjust the accent/selection colors.
         */
        accentColorAdjustment.run(currentTheme, uiProps);
        foregroundGeneration.run(currentTheme, uiProps);

        initGlobals(currentTheme, defaults, uiProps);
        initUIProperties(currentTheme, defaults, uiProps);
        initIconTheme(currentTheme, defaults, uiProps);
        initPlatformProperties(currentTheme, defaults, uiProps);

        DecorationsHandler.getSharedInstance().loadDecorationProperties(uiProps, defaults);
        adjustPlatformSpecifics(uiProps);
        defaults.putAll(uiProps);
    }

    private void initGlobals(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "globals", "properties/"), uiProps,
                                     defaults);

        currentTheme.customizeGlobals(uiProps, defaults);
        installGlobals(uiProps, defaults);
    }

    private void installGlobals(final Properties uiProps, final UIDefaults defaults) {
        final HashMap<String, Object> globalSettings = new HashMap<>();
        for (final Object key : uiProps.keySet()) {
            if (key instanceof String && ((String) key).startsWith(GLOBAL_PREFIX)) {
                globalSettings.put(((String) key).substring(GLOBAL_PREFIX.length()), uiProps.get(key));
            }
        }
        PropertyLoader.replaceProperties(defaults,
                                         e -> e.getKey() instanceof String && ((String) e.getKey()).contains("."),
                                         e -> {
                                             final String s = (String) e.getKey();
                                             final String globalKey = s.substring(s.lastIndexOf('.') + 1);
                                             return globalSettings.get(globalKey);
                                         });
    }

    private void initUIProperties(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        for (String property : UI_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "properties/ui/"),
                                         uiProps, defaults);
        }
        currentTheme.customizeUIProperties(uiProps, defaults);
    }

    private void initIconTheme(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        currentTheme.loadIconTheme(uiProps, defaults);
        for (String property : ICON_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "properties/icons/"),
                                         uiProps, defaults);
        }
        currentTheme.customizeIconTheme(uiProps, defaults);
    }

    private void initPlatformProperties(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, SystemInfo.getOsName(),
                                                                   "properties/platform/"),
                                     uiProps, defaults);
        currentTheme.customizePlatformProperties(uiProps, defaults);
    }

    private void adjustPlatformSpecifics(final Properties uiProps) {
        boolean useScreenMenuBar = Boolean.getBoolean(MAC_OS_MENU_BAR_KEY);
        // If user wants to use Apple menu bar, then we need to keep the default
        // component for MenuBarUI and MenuUI
        if (SystemInfo.isMac && useScreenMenuBar) {
            uiProps.remove("MenuBarUI");
        }
    }
}
