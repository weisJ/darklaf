/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.theme;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.DarkMetalTheme;
import com.github.weisj.darklaf.util.PropertyLoader;
import com.github.weisj.darklaf.util.SystemInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.html.StyleSheet;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 */
public abstract class Theme {
    private static final Logger LOGGER = Logger.getLogger(Theme.class.getName());
    private static final String[] UI_PROPERTIES = new String[]{
            "borders", "button", "checkBox", "colorChooser", "comboBox", "fileChooser", "tristate",
            "internalFrame", "label", "list", "menu", "menuBar", "menuItem", "numberingPane", "optionPane", "panel",
            "popupMenu", "progressBar", "radioButton", "rootPane", "scrollBar", "scrollPane", "separator",
            "slider", "spinner", "splitPane", "statusBar", "tabbedPane", "tabFrame", "table", "taskPane", "text",
            "toggleButton", "toolBar", "toolTip", "tree",
    };

    /**
     * Called in the constructor of the look and feel.
     */
    public void beforeInstall() {
        if (SystemInfo.isWindows || SystemInfo.isLinux) {
            MetalLookAndFeel.setCurrentTheme(new DarkMetalTheme());
        }
    }

    /**
     * Load the theme defaults.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadDefaults(@NotNull final Properties properties, final UIDefaults currentDefaults) {
        var name = getResourcePath() + getName() + "_defaults.properties";
        PropertyLoader.putProperties(load(name), properties, currentDefaults);
    }

    protected String getResourcePath() {
        return "";
    }

    public abstract String getName();

    protected Properties load(final String name) {
        final Properties properties = new Properties();
        try (InputStream stream = getResourceAsStream(name)) {
            properties.load(stream);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Could not load " + name + ".properties. " + e.getMessage(), e.getStackTrace());
        }
        return properties;
    }

    protected InputStream getResourceAsStream(final String name) {
        return getLoaderClass().getResourceAsStream(name);
    }

    protected Class<? extends Theme> getLoaderClass() {
        return this.getClass();
    }

    /**
     * Load the global values.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadGlobals(@NotNull final Properties properties, final UIDefaults currentDefaults) {
        PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "globals", "properties/"),
                                     properties, currentDefaults);
    }

    /**
     * Load the platform defaults.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadPlatformProperties(final Properties properties, final UIDefaults currentDefaults) {
        final String osPrefix = SystemInfo.isMac ? "mac" : SystemInfo.isWindows ? "windows" : "linux";
        PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, osPrefix, "properties/platform/"),
                                     properties, currentDefaults);
    }

    /**
     * Load the ui defaults.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadUIProperties(final Properties properties, final UIDefaults currentDefaults) {
        for (var property : UI_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "properties/ui/"),
                                         properties, currentDefaults);
        }
    }

    public StyleSheet loadStyleSheet() {
        StyleSheet styleSheet = new StyleSheet();
        try (var in = getResourceAsStream(getResourcePath() + getName() + "_styleSheet.css");
             var inReader = new InputStreamReader(in, StandardCharsets.UTF_8);
             var r = new BufferedReader(inReader)) {
            styleSheet.loadRules(r, null);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
        return styleSheet;
    }

    public boolean useCustomDecorations() {
        return true;
    }

    public abstract boolean isDark();
}
