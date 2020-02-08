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
    private static final String[] ICON_PROPERTIES = new String[]{
            "control", "dialog", "files", "indicator", "menu", "misc", "navigation", "window"
    };

    public UIManager.LookAndFeelInfo createLookAndFeelInfo() {
        return new UIManager.LookAndFeelInfo(getName(), DarkLaf.class.getCanonicalName());
    }

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
        String name = getResourcePath() + getName() + "_defaults.properties";
        PropertyLoader.putProperties(load(name), properties, currentDefaults);
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
     * Load the icon defaults.
     *
     * @param properties      the properties to load the value into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadIconProperties(final Properties properties, final UIDefaults currentDefaults) {
        loadIconTheme(properties, currentDefaults);
        for (String property : ICON_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "properties/icons/"),
                                         properties, currentDefaults);
        }
    }

    /**
     * Load the general properties file for the icon themes.
     *
     * @param properties      the properties to load the value into.
     * @param currentDefaults the current ui defaults.
     */
    protected void loadIconTheme(final Properties properties, final UIDefaults currentDefaults) {
        IconTheme iconTheme = getPresetIconTheme();
        Properties props;
        switch (iconTheme) {
            case DARK:
                props = PropertyLoader.loadProperties(DarkLaf.class, "dark_icons", "properties/icons/presets/");
                break;
            case LIGHT:
                props = PropertyLoader.loadProperties(DarkLaf.class, "light_icons", "properties/icons/presets/");
                break;
            case NONE:
            default:
                props = load(getResourcePath() + getName() + "_icons.properties");
        }
        PropertyLoader.putProperties(props, properties, currentDefaults);
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
        for (String property : UI_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "properties/ui/"),
                                         properties, currentDefaults);
        }
    }

    /**
     * The preset icon theme.
     *
     * @return the icon theme.
     */
    protected abstract IconTheme getPresetIconTheme();

    /**
     * Load a properties file using {@link #getResourceAsStream(String)}.
     *
     * @param name the properties file to load.
     * @return the properties.
     */
    protected Properties load(final String name) {
        final Properties properties = new Properties();
        try (InputStream stream = getResourceAsStream(name)) {
            properties.load(stream);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Could not load " + name + ".properties. " + e.getMessage(), e.getStackTrace());
        }
        return properties;
    }

    /**
     * The path to the resources location relative to the classpath of {@link #getLoaderClass()}.
     *
     * @return the relative resource path
     */
    protected String getResourcePath() {
        return "";
    }

    /**
     * Get the name of this theme.
     *
     * @return the name of the theme.
     */
    public abstract String getName();

    /**
     * Load a resource as an InputStream. By default this will the {@link ClassLoader#getResourceAsStream(String)}
     * method from the class returned by {@link #getLoaderClass()}.
     *
     * @param name resource to load.
     * @return the InputStream.
     */
    protected InputStream getResourceAsStream(final String name) {
        return getLoaderClass().getResourceAsStream(name);
    }

    /**
     * The class used to determine the runtime location of resources. By default this is the same class as the theme.
     *
     * @return the loader class.
     */
    protected Class<? extends Theme> getLoaderClass() {
        return this.getClass();
    }

    /**
     * Load the css style sheet used for html display in text components with a {@link
     * javax.swing.text.html.HTMLEditorKit}.
     *
     * @return the {@link StyleSheet}.
     */
    public StyleSheet loadStyleSheet() {
        StyleSheet styleSheet = new StyleSheet();
        try (InputStream in = getResourceAsStream(getResourcePath() + getName() + "_styleSheet.css");
             InputStreamReader inReader = new InputStreamReader(in, StandardCharsets.UTF_8);
             BufferedReader r = new BufferedReader(inReader)) {
            styleSheet.loadRules(r, null);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
        return styleSheet;
    }

    /**
     * Returns whether this theme should use custom decorations if available.
     *
     * @return true if decoration should be used.
     */
    public boolean useCustomDecorations() {
        return true;
    }

    /**
     * Returns whether this theme uses dark icons as the default for [aware] icon.
     *
     * @return true if dark icons are used.
     */
    public abstract boolean useDarkIcons();

    protected enum IconTheme {
        NONE,
        DARK,
        LIGHT
    }
}
