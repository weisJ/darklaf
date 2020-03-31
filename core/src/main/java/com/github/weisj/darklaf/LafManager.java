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
package com.github.weisj.darklaf;

import com.github.weisj.darklaf.task.DefaultsAdjustmentTask;
import com.github.weisj.darklaf.theme.DarculaTheme;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.DefaultThemeProvider;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;
import com.github.weisj.darklaf.theme.info.ThemeProvider;

import javax.swing.*;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.HashSet;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * Manager for the Look and Feel.
 *
 * @author Jannis Weis
 */
public final class LafManager {

    private static ThemeProvider themeProvider;
    private static Theme theme;
    private static boolean logEnabled = false;
    private static boolean decorationsOverwrite = true;
    private static final Collection<DefaultsAdjustmentTask> uiDefaultsTasks = new HashSet<>();

    static {
        enableLogging(true);
    }

    /**
     * Enable logging for the Look and Feel.
     *
     * @param logEnabled true if messages should be logged.
     */
    public static void enableLogging(final boolean logEnabled) {
        if (logEnabled == LafManager.logEnabled) return;
        if (!logEnabled) {
            LogManager.getLogManager().reset();
        } else {
            try (InputStream inputStream = DarkLaf.class.getClassLoader()
                                                        .getResourceAsStream(
                                                            "com/github/weisj/darklaf/log/logging.properties")) {
                if (inputStream != null) {
                    Logger.getGlobal().fine("Loading logging configuration.");
                    LogManager.getLogManager().readConfiguration(inputStream);
                }
                Logger.getGlobal().fine(() -> "Loaded logging config" + LogManager.getLogManager().toString());
            } catch (IOException e) {
                Logger.getGlobal().log(Level.SEVERE, "init logging system", e);
            }
        }
        LafManager.logEnabled = logEnabled;
    }

    /**
     * Returns whther custom decorations should be used. If this returns true decorations still might not be used if the
     * theme or platform don't support them.
     *
     * @return true if decorations should be used.
     */
    public static boolean isDecorationsEnabled() {
        return decorationsOverwrite;
    }

    /**
     * Set globally whether decorations are enabled. By default, this is true. Decorations are used if this value is set
     * to true and the current platform and theme support custom decorations.
     *
     * @param enabled true if decorations should be used if available.
     */
    public static void setDecorationsEnabled(final boolean enabled) {
        if (decorationsOverwrite != enabled) {
            decorationsOverwrite = enabled;
            if (!decorationsOverwrite) {
                updateLaf();
            }
        }
    }

    public static ThemeProvider getThemeProvider() {
        if (themeProvider == null) themeProvider = createDefaultThemeProvider();
        return themeProvider;
    }

    public static void setThemeProvider(final ThemeProvider themeProvider) {
        LafManager.themeProvider = themeProvider;
    }

    private static ThemeProvider createDefaultThemeProvider() {
        return new DefaultThemeProvider();
    }

    /**
     * Get the associated theme for the given preferred style.
     *
     * @param style the preferred theme style.
     * @return the associated Theme or best match if there is none associated.
     */
    public static Theme themeForPreferredStyle(final PreferredThemeStyle style) {
        return getThemeProvider().getTheme(style);
    }

    /**
     * Get the current theme. This method will never return null even if the LaF isn#t currently installed.
     *
     * @return the current theme.
     */
    public static Theme getTheme() {
        if (theme == null) {
            setTheme(themeForPreferredStyle(null));
        }
        return theme;
    }

    /**
     * Set the current theme.
     *
     * @param theme The theme to use.
     */
    public static void setTheme(final Theme theme) {
        LafManager.theme = theme;
    }

    /**
     * Install the theme suited for the given preferred theme style.
     *
     * @param preferredThemeStyle the preferred theme style.
     */
    public static void installTheme(final PreferredThemeStyle preferredThemeStyle) {
        setTheme(preferredThemeStyle);
        install();
    }

    /**
     * Sets the theme suited for the given preferred theme style.
     *
     * @param preferredThemeStyle the preferred theme style.
     */
    public static void setTheme(final PreferredThemeStyle preferredThemeStyle) {
        setTheme(themeForPreferredStyle(preferredThemeStyle));
    }


    /**
     * Sets the current theme and installs the LaF. If the LaF is already installed the theme is switched. This behaves
     * exactly like {@link #setTheme(Theme)} followed by {@link #install()}
     *
     * @param theme the theme to install.
     */
    public static void installTheme(final Theme theme) {
        setTheme(theme);
        install();
    }

    /**
     * Reloads the icon theme. Forces icons to update their colors.
     */
    public static void reloadIconTheme() {
        try {
            setTheme(getTheme().getClass().getDeclaredConstructor().newInstance());
        } catch (InstantiationException
            | IllegalAccessException
            | NoSuchMethodException
            | InvocationTargetException e) {
            e.printStackTrace();
        }
    }

    /**
     * Overload for {@link #installTheme(Theme)}.
     *
     * @param theme the theme to install.
     * @see #installTheme(Theme) installTheme().
     */
    public static void install(final Theme theme) {
        installTheme(theme);
    }

    /**
     * Install the current theme. If no theme is installed, the default is {@link DarculaTheme}. This sets the current
     * LaF and applies the given theme.
     */
    public static void install() {
        try {
            getTheme();
            UIManager.setLookAndFeel(DarkLaf.class.getCanonicalName());
            updateLaf();
        } catch (final ClassNotFoundException
            | InstantiationException
            | IllegalAccessException
            | UnsupportedLookAndFeelException e) {
            e.printStackTrace();
        }
    }

    /**
     * Update the component ui classes for all current windows.
     */
    public static void updateLaf() {
        for (final Window w : Window.getWindows()) {
            updateLafRecursively(w);
        }
    }

    private static void updateLafRecursively(final Window window) {
        for (final Window childWindow : window.getOwnedWindows()) {
            updateLafRecursively(childWindow);
        }
        SwingUtilities.updateComponentTreeUI(window);
    }

    /**
     * Register a task to modify the ui defaults.
     *
     * @param task the defaults init task.
     */
    public static void registerDefaultsAdjustmentTask(final DefaultsAdjustmentTask task) {
        uiDefaultsTasks.add(task);
    }

    /**
     * Remove a registered task to modify the ui defaults.
     *
     * @param task the defaults init task.
     */
    public static void removeDefaultsAdjustmentTask(final DefaultsAdjustmentTask task) {
        uiDefaultsTasks.remove(task);
    }

    /**
     * Get a view of all currently registered init tasks. Modification will also mutate the collection itself.
     *
     * @return collection of init tasks.
     */
    public static Collection<DefaultsAdjustmentTask> getUserAdjustmentTasks() {
        return uiDefaultsTasks;
    }

}
