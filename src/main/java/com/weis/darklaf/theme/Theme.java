package com.weis.darklaf.theme;

import com.weis.darklaf.DarkLaf;
import com.weis.darklaf.DarkMetalTheme;
import com.weis.darklaf.util.LafUtil;
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

public abstract class Theme {
    private static final Logger LOGGER = Logger.getLogger(Theme.class.getName());
    private static final String[] UI_PROPERTIES = new String[]{
            "borders", "button", "checkBox", "colorChooser", "comboBox", "fileChooser",
            "internalFrame", "label", "list", "menu", "menuBar", "menuItem", "optionPane", "panel", "popupMenu",
            "progressBar", "radioButton", "rootPane", "scrollBar", "scrollPane", "separator", "slider", "spinner",
            "splitPane", "statusBar", "tabbedPane", "table", "text", "toggleButton", "toolBar", "toolTip", "tree",
    };

    public void beforeInstall() {
        if (SystemInfo.isWindows || SystemInfo.isLinux) {
            MetalLookAndFeel.setCurrentTheme(new DarkMetalTheme());
        }
    }

    public void loadDefaults(@NotNull final Properties properties, final UIDefaults currentDefaults) {
        var name = getResourcePath() + getName() + "_defaults.properties";
        putProperties(load(name), properties, currentDefaults);
    }

    public void loadGlobals(@NotNull final Properties properties, final UIDefaults currentDefaults) {
        putProperties(LafUtil.loadProperties(DarkLaf.class, "globals", "properties/"), properties,
                      currentDefaults);
    }

    public void loadPlatformProperties(final Properties properties, final UIDefaults currentDefaults) {
        final String osPrefix = SystemInfo.isMac ? "mac" : SystemInfo.isWindows ? "windows" : "linux";
        putProperties(LafUtil.loadProperties(DarkLaf.class, osPrefix, "properties/platform/"), properties,
                      currentDefaults);
    }

    public void loadUIProperties(final Properties properties, final UIDefaults currentDefaults) {
        for (var property : UI_PROPERTIES) {
            putProperties(LafUtil.loadProperties(DarkLaf.class, property, "properties/ui/"), properties,
                          currentDefaults);
        }
    }

    protected void putProperties(@NotNull final Properties properties, final Properties accumulator,
                                 final UIDefaults currentDefaults) {
        for (final String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            var parsed = LafUtil.parseValue(key, value, accumulator);
            if (parsed != null) {
                accumulator.put(key, parsed);
            } else {
                currentDefaults.remove(key);
            }
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
