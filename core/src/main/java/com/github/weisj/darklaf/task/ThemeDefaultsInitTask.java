/*
 * MIT License
 *
 * Copyright (c) 2020-2023 Jannis Weis
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
package com.github.weisj.darklaf.task;

import java.awt.*;
import java.util.HashMap;
import java.util.Objects;
import java.util.Properties;
import java.util.function.Consumer;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.iconset.IconSet;
import com.github.weisj.darklaf.nativelaf.DecorationsHandler;
import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.properties.icons.IconResolver;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class ThemeDefaultsInitTask implements DefaultsInitTask {

    private static final String GLOBAL_PREFIX = "global.";
    private static final String MAC_OS_MENU_BAR_KEY = "apple.laf.useScreenMenuBar";
    private static final String[] UI_PROPERTIES = new String[] {"borders", "button", "cell", "checkBox", "colorChooser",
            "comboBox", "fileChooser", "tristate", "internalFrame", "label", "list", "menu", "menuBar", "menuItem",
            "numberingPane", "optionPane", "panel", "popupMenu", "progressBar", "radioButton", "rootPane", "scrollBar",
            "scrollPane", "separator", "slider", "spinner", "splitPane", "statusBar", "tabbedPane", "tabFrame", "table",
            "taskPane", "text", "toggleButton", "toolBar", "toolTip", "tree", "misc"};
    private static final String[] ICON_PROPERTIES =
            new String[] {"checkBox", "radioButton", "slider", "frame"};
    private static final String ACCENT_COLOR_KEY = "effectiveAccentColor";
    private static final String ACCENT_COLOR_BACKUP_KEY = "themeAccentColor";
    private static final String ACCENT_COLOR_SOURCE_KEY = "widgetFillDefault";
    private static final String SELECTION_COLOR_KEY = "effectiveSelectionColor";
    private static final String SELECTION_COLOR_BACKUP_KEY = "themeSelectionColor";
    private static final String SELECTION_COLOR_SOURCE_KEY = "textCompSelectionBackground";
    private final DefaultsAdjustmentTask userPreferenceAdjustment = new UserDefaultsAdjustmentTask();
    private final DefaultsAdjustmentTask accentColorAdjustment = new AccentColorAdjustmentTask();
    private final DefaultsAdjustmentTask foregroundGeneration = new ForegroundColorGenerationTask();

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        loadThemeDefaults(currentTheme, defaults);
    }

    private void loadThemeDefaults(final Theme currentTheme, final UIDefaults defaults) {
        Properties uiProps = new Properties();
        initPlatformPredefinitions(uiProps, defaults);

        currentTheme.loadDefaults(uiProps, defaults, DarkUIUtil.iconResolver());
        backupAccentColors(uiProps);

        /*
         * User preferences need to be applied here so changes are applied to all components that use the
         * property.
         */
        userPreferenceAdjustment.run(currentTheme, uiProps);

        /*
         * Adjust the accent/selection colors.
         */
        accentColorAdjustment.run(currentTheme, uiProps);
        foregroundGeneration.run(currentTheme, uiProps);

        initGlobals(currentTheme, defaults, uiProps);
        initTemporaryBindings(defaults, uiProps);
        initUIProperties(currentTheme, defaults, uiProps);
        initIconTheme(currentTheme, defaults, uiProps);
        initPlatformProperties(currentTheme, defaults, uiProps);

        DecorationsHandler.getSharedInstance().loadDecorationProperties(uiProps, defaults);
        adjustPlatformSpecifics(uiProps);

        initAccentProperties(currentTheme, uiProps);

        defaults.putAll(uiProps);
    }

    private void initPlatformPredefinitions(final Properties uiProps, final UIDefaults defaults) {
        IconResolver iconResolver = DarkUIUtil.iconResolver();
        Consumer<String> osPlatformLoader = osName -> PropertyLoader.putProperties(
                PropertyLoader.loadProperties(
                        DarkLaf.class, osName, "nativelaf/",
                        PropertyLoader.LoadMode.AllowMissing),
                uiProps, defaults, iconResolver);
        osPlatformLoader.accept("predef_base");
        setupOSSpecific("predef_", osPlatformLoader);
    }

    private void backupAccentColors(final Properties uiProps) {
        uiProps.put(ACCENT_COLOR_BACKUP_KEY, Objects.requireNonNull(
                uiProps.get(ACCENT_COLOR_SOURCE_KEY), ACCENT_COLOR_SOURCE_KEY));
        uiProps.put(SELECTION_COLOR_BACKUP_KEY, Objects.requireNonNull(
                uiProps.get(SELECTION_COLOR_SOURCE_KEY), SELECTION_COLOR_SOURCE_KEY));
    }

    private void initAccentProperties(final Theme currentTheme, final Properties uiProps) {
        Color accentColor = currentTheme.getAccentColorRule().getAccentColor();
        Color selectionColor = currentTheme.getAccentColorRule().getSelectionColor();
        uiProps.put(ACCENT_COLOR_KEY, accentColor != null ? accentColor : uiProps.get(ACCENT_COLOR_BACKUP_KEY));
        uiProps.put(SELECTION_COLOR_KEY,
                selectionColor != null ? selectionColor : uiProps.get(SELECTION_COLOR_BACKUP_KEY));
    }

    private void initGlobals(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        IconResolver iconResolver = DarkUIUtil.iconResolver();
        PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "globals", ""),
                uiProps, defaults, iconResolver);

        currentTheme.customizeGlobals(uiProps, defaults, iconResolver);
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
                e -> e.getKey() instanceof String && ((String) e.getKey()).contains("."), e -> {
                    final String s = (String) e.getKey();
                    final String globalKey = s.substring(s.lastIndexOf('.') + 1);
                    return globalSettings.get(globalKey);
                });
    }

    private void initUIProperties(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        IconResolver iconResolver = DarkUIUtil.iconResolver();
        for (String property : UI_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "ui/"),
                    uiProps, defaults, iconResolver);
        }
        currentTheme.customizeUIProperties(uiProps, defaults, iconResolver);
    }

    private void initIconTheme(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        IconResolver iconResolver = DarkUIUtil.iconResolver();
        currentTheme.loadIconTheme(uiProps, defaults, iconResolver);
        for (String property : ICON_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(IconSet.class, property, ""),
                    uiProps, defaults, iconResolver);
        }
        currentTheme.customizeIconTheme(uiProps, defaults, iconResolver);
    }

    private void initTemporaryBindings(final UIDefaults defaults, final Properties uiProps) {
        PropertyLoader.putProperties(
                PropertyLoader.loadProperties(DarkLaf.class, "tmp", ""),
                uiProps, defaults, DarkUIUtil.iconResolver());
    }

    private void initPlatformProperties(final Theme currentTheme, final UIDefaults defaults, final Properties uiProps) {
        IconResolver iconResolver = DarkUIUtil.iconResolver();
        setupOSSpecific("", osName -> PropertyLoader.putProperties(
                PropertyLoader.loadProperties(DarkLaf.class, osName, "nativelaf/"),
                uiProps, defaults, iconResolver));
        currentTheme.customizePlatformProperties(uiProps, defaults, iconResolver);
    }

    private String getOsName() {
        String osName = System.getProperty("darklaf.internal.osname");
        return osName != null ? osName : SystemInfo.getOsName();
    }

    private void setupOSSpecific(final String prefix, final Consumer<String> setupFunction) {
        setupFunction.accept(prefix + getOsName());
        if (SystemInfo.isWindows11()) {
            setupFunction.accept(prefix + "windows11");
        }
    }

    private void adjustPlatformSpecifics(final Properties uiProps) {
        PropertyUtil.installSystemProperty(MAC_OS_MENU_BAR_KEY, "true");
        boolean useScreenMenuBar = Boolean.getBoolean(MAC_OS_MENU_BAR_KEY);
        // If user wants to use Apple menu bar, then we need to keep the default
        // component for MenuBarUI and MenuUI
        if (SystemInfo.isMac && useScreenMenuBar) {
            uiProps.remove("MenuBarUI");
        }
    }
}
