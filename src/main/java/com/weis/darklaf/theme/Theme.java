package com.weis.darklaf.theme;

import com.weis.darklaf.DarkLaf;
import com.weis.darklaf.DarkMetalTheme;
import com.weis.darklaf.util.PropertyLoader;
import com.weis.darklaf.util.SystemInfo;
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
            "internalFrame", "label", "list", "menu", "menuBar", "menuItem", "optionPane", "panel",
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

    /**
     * Load the global values.
     *
     * @param properties the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadGlobals(@NotNull final Properties properties, final UIDefaults currentDefaults) {
        PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, "globals", "properties/"),
                                     properties, currentDefaults);
    }

    /**
     * Load the platform defaults.
     *
     * @param properties the properties to load the values into.
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
     * @param properties the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadUIProperties(final Properties properties, final UIDefaults currentDefaults) {
        for (var property : UI_PROPERTIES) {
            PropertyLoader.putProperties(PropertyLoader.loadProperties(DarkLaf.class, property, "properties/ui/"),
                                         properties, currentDefaults);
        }
    }


    protected Properties load(final String name) {
        final Properties properties = new Properties();
        try (InputStream stream = getResourceAsStream(name)) {
            properties.load(stream);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Could not load" + name + ".properties", e.getMessage());
        }
        return properties;
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

    protected String getResourcePath() {
        return "";
    }

    public abstract String getName();

    protected InputStream getResourceAsStream(final String name) {
        return getResourceAsStream(this.getClass(), name);
    }

    protected InputStream getResourceAsStream(@NotNull final Class<?> clazz, final String name) {
        return clazz.getResourceAsStream(name);
    }

    public boolean useCustomDecorations() {
        return true;
    }

    public abstract boolean isDark();
}
