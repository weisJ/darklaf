/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf;

import java.awt.*;
import java.util.*;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.components.renderer.SimpleListCellRenderer;
import com.github.weisj.darklaf.platform.DecorationsHandler;
import com.github.weisj.darklaf.platform.ThemePreferencesHandler;
import com.github.weisj.darklaf.settings.ThemeSettings;
import com.github.weisj.darklaf.synthesised.ThemedDarklafInfo;
import com.github.weisj.darklaf.task.DefaultsAdjustmentTask;
import com.github.weisj.darklaf.task.DefaultsInitTask;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.event.*;
import com.github.weisj.darklaf.theme.info.PreferredThemeStyle;
import com.github.weisj.darklaf.theme.info.ThemeProvider;
import com.github.weisj.darklaf.theme.laf.ThemedLookAndFeel;
import com.github.weisj.darklaf.util.LogUtil;

/**
 * Manager for the Look and Feel.
 *
 * @author Jannis Weis
 */
public final class LafManager {

    private static final Logger LOGGER = LogUtil.getLogger(LafManager.class);
    private static final LafInstaller installer = new LafInstaller();
    private static ThemeProvider themeProvider;
    private static Theme theme;
    private static Theme installedTheme;
    private static final List<Theme> registeredThemes = new ArrayList<>();
    private static final Collection<DefaultsAdjustmentTask> uiDefaultsTasks = new ArrayList<>();
    private static final Collection<DefaultsInitTask> uiInitTasks = new ArrayList<>();

    static {
        setLogLevel(Level.WARNING);
        ServiceLoader.load(Theme.class).forEach(LafManager::registerTheme);
    }

    /**
     * Enable logging for the Look and Feel. true means Level.INFO, false Level.SEVERE.
     *
     * @see #setLogLevel(Level)
     * @param logEnabled true if messages should be logged.
     * @deprecated Use {@link #setLogLevel(Level)} instead.
     */
    @Deprecated
    public static void enableLogging(final boolean logEnabled) {
        LOGGER.warning("LafManger#enableLogging will overwrite specific log levels and is deprecated. "
                + "Use LafManager#setLogLevel instead.");
        setLogLevel(logEnabled ? Level.INFO : Level.SEVERE);
    }

    /**
     * Sets the log level.
     *
     * @param level the new log level.
     */
    public static void setLogLevel(final Level level) {
        LogUtil.setLevel(level);
        LOGGER.fine("Setting log level to " + level);
    }

    /**
     * Get the current log level.
     *
     * @return the log level.
     */
    public static Level getLogLevel() {
        return LogUtil.getLevel();
    }

    /**
     * Returns whether custom decorations should be used. If this returns true decorations still might
     * not be used if the theme or platform don't support them.
     *
     * @return true if decorations should be used.
     */
    public static boolean isDecorationsEnabled() {
        return DecorationsHandler.getSharedInstance().isCustomDecorationSupported();
    }

    /**
     * Set globally whether decorations are enabled. By default, this is true. Decorations are used if
     * this value is set to true and the current platform and theme support custom decorations.
     *
     * @param enabled true if decorations should be used if available.
     */
    public static void setDecorationsEnabled(final boolean enabled) {
        boolean isEnabled = isDecorationsEnabled();
        DecorationsHandler.getSharedInstance().setDecorationsEnabled(enabled);
        if (isEnabled != enabled && LafManager.isInstalled()) {
            updateLaf();
        }
    }

    /**
     * Enabled whether changes in the preferred theme style should be reported to
     * {@link ThemePreferenceListener}s. On some platforms this setting may do nothing.
     *
     * <p>
     * Warning: If preference reporting is enabled it <b>needs</b> to be disabled before closing the
     * program. Not doing so can result in memory leaks and prevent the classloader from being garbage
     * collected.
     *
     * @param enabled true if changes should be reported.
     */
    public static void enabledPreferenceChangeReporting(final boolean enabled) {
        LOGGER.info("Preference reporting is set to: " + enabled);
        ThemePreferencesHandler.getSharedInstance().enablePreferenceChangeReporting(enabled);
        if (ThemeSettings.isInitialized()) ThemeSettings.getInstance().setSystemPreferencesEnabled(enabled);
    }

    /**
     * Returns whether changes to the preferred theme style should be reported to
     * {@link ThemePreferenceListener}s.
     *
     * @return true if enabled and currently reporting.
     */
    public static boolean isPreferenceChangeReportingEnabled() {
        return ThemePreferencesHandler.getSharedInstance().isPreferenceChangeReportingEnabled();
    }

    /**
     * Adds a {@link ThemePreferenceListener}. If preference change reporting is enabled the handler
     * will receive events if preferences have changed.
     *
     * @param listener the listener to add.
     * @see ThemePreferenceListener
     * @see #enabledPreferenceChangeReporting(boolean)
     * @see #isPreferenceChangeReportingEnabled()
     */
    public static void addThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        ThemePreferencesHandler.getSharedInstance().addThemePreferenceChangeListener(listener);
    }

    /**
     * Removes a {@link ThemePreferenceListener}.
     *
     * @param listener the listener to remove.
     * @see ThemePreferenceListener
     * @see #enabledPreferenceChangeReporting(boolean)
     * @see #isPreferenceChangeReportingEnabled()
     */
    public static void removeThemePreferenceChangeListener(final ThemePreferenceListener listener) {
        ThemePreferencesHandler.getSharedInstance().removeThemePreferenceChangeListener(listener);
    }

    /**
     * Adds a {@link ThemeChangeListener}. The listener will receive events if the theme is changed.
     *
     * @param listener the listener to add.
     */
    public static void addThemeChangeListener(final ThemeChangeListener listener) {
        installer.addThemeChangeListener(listener);
    }

    /**
     * Removes a {@link ThemeChangeListener}.
     *
     * @param listener the listener to add.
     */
    public static void removeThemeChangeListener(final ThemeChangeListener listener) {
        installer.removeThemeChangeListener(listener);
    }

    /**
     * Gets the preferred theme style. If theme preference change reporting is enabled this may use
     * native os settings to determine these values.
     *
     * @return the preferred theme style.
     * @see #isPreferenceChangeReportingEnabled()
     * @see #enabledPreferenceChangeReporting(boolean)
     */
    public static PreferredThemeStyle getPreferredThemeStyle() {
        return ThemePreferencesHandler.getSharedInstance().getPreferredThemeStyle();
    }

    /**
     * Get the current theme provider. The theme provider is responsible the produce available themes
     * for a given preferred theme style.
     *
     * @return the theme provider.
     * @see PreferredThemeStyle
     */
    public static ThemeProvider getThemeProvider() {
        if (themeProvider == null) themeProvider = createDefaultThemeProvider();
        return themeProvider;
    }

    /**
     * Set the current theme provider. The theme provider is responsible the produce available themes
     * for a given preferred theme style.
     *
     * @param themeProvider the theme provider.
     * @see PreferredThemeStyle
     */
    public static void setThemeProvider(final ThemeProvider themeProvider) {
        LafManager.themeProvider = themeProvider;
    }

    /*
     * Default theme provider. Defaults to IntelliJ/Darcula Light/Dark high contrast themes.
     */
    private static ThemeProvider createDefaultThemeProvider() {
        return ThemeProvider.createDefault();
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
     * Register all themes in array. Registered themes are returned in {@link #getRegisteredThemes()}.
     *
     * @param themes the themes to register.
     */
    public static void registerTheme(final Theme... themes) {
        if (themes == null) return;
        for (Theme theme : themes) {
            registerTheme(theme);
        }
    }

    /**
     * Register a theme. Registered themes are returned in {@link #getRegisteredThemes()}.
     *
     * @param theme the theme to register.
     */
    public static void registerTheme(final Theme theme) {
        LOGGER.fine("Registering " + theme);
        registeredThemes.add(theme);
    }

    /**
     * Remove a register a theme. Registered themes are returned in {@link #getRegisteredThemes()}.
     *
     * @param theme the theme to register.
     * @return returns whether the theme was previously registered.
     */
    public static boolean unregisterTheme(final Theme theme) {
        return registeredThemes.remove(theme);
    }

    /**
     * Get all currently registered themes.
     *
     * @return all themes currently registered as array.
     */
    public static Theme[] getRegisteredThemes() {
        Theme[] themes = registeredThemes.toArray(new Theme[0]);
        Arrays.sort(themes);
        return themes;
    }

    /**
     * Get {@link javax.swing.UIManager.LookAndFeelInfo}s for all currently registered themes. These can
     * be directly used to install the specified theme. Only themes that are annotated
     * using @SynthesiseLaf and compiled with the corresponding annotations processor may contribute to
     * this list.
     *
     * @return all look and feel infos for currently registered themes as array.
     */
    public static UIManager.LookAndFeelInfo[] getRegisteredThemeInfos() {
        return registeredThemes.stream().sorted().map(ThemedDarklafInfo::new).filter(ThemedDarklafInfo::exists)
                .toArray(UIManager.LookAndFeelInfo[]::new);
    }

    /**
     * Replaces a registered theme. The newTheme is only registered if the old theme is currently
     * registerd.
     *
     * @param oldTheme the old theme to unregister.
     * @param newTheme the new theme to register.
     */
    public static void replaceTheme(final Theme oldTheme, final Theme newTheme) {
        if (unregisterTheme(oldTheme)) {
            registerTheme(newTheme);
        }
    }

    /**
     * Get a combobox model for all currently registered themes.
     *
     * @return the combo box model.
     */
    public static ComboBoxModel<Theme> getThemeComboBoxModel() {
        return new DefaultComboBoxModel<>(getRegisteredThemes());
    }

    /**
     * Creates a {@link ListCellRenderer} for themes.
     *
     * @return the list cell render.
     */
    public static ListCellRenderer<Theme> getThemeListCellRenderer() {
        return SimpleListCellRenderer.create(Theme::getDisplayName);
    }

    /**
     * Get the current theme. This method will never return null even if the LaF isn#t currently
     * installed.
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
     * Returns the currently installed theme. This differs from {@link #getTheme()} by the fact that
     * {@link #getTheme()} can return a different theme than the currently installed one.
     *
     * @return the currently installed theme.
     */
    public static Theme getInstalledTheme() {
        return installedTheme != null ? installedTheme : getTheme();
    }

    static void setInstalledTheme(final Theme theme) {
        LOGGER.info("Setting installed theme: " + theme);
        installedTheme = theme;
    }

    /**
     * Checks whether darklaf is currently installed.
     *
     * @return true if installed.
     */
    public static boolean isInstalled() {
        return UIManager.getLookAndFeel() instanceof ThemedLookAndFeel;
    }

    /**
     * Set the current theme.
     *
     * @param theme The theme to use.
     */
    public static void setTheme(final Theme theme) {
        Theme old = LafManager.theme;
        LafManager.theme = theme;
        installer.notifyThemeChanged(old, theme);
        if (ThemeSettings.isInitialized()) ThemeSettings.getInstance().refresh();
    }

    /**
     * Install the theme reported by the {@link ThemePreferenceChangeEvent}.
     *
     * @param changeEvent the change event.
     */
    public static void installTheme(final ThemePreferenceChangeEvent changeEvent) {
        setTheme(changeEvent.getPreferredThemeStyle());
        install();
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
     * Sets the current theme and installs the LaF. If the LaF is already installed the theme is
     * switched. This behaves exactly like {@link #setTheme(Theme)} followed by {@link #install()}
     *
     * @param theme the theme to install.
     */
    public static void installTheme(final Theme theme) {
        if (theme == getTheme() && isInstalled()) return;
        setTheme(theme);
        install();
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
     * Install the current theme. If no theme is installed, the default is based on the current
     * {@link ThemeProvider}. This sets the current LaF and applies the given theme.
     */
    public static void install() {
        Theme theme = getTheme();
        installer.install(theme);
        setInstalledTheme(theme);
    }

    /** Update the component ui classes for all current windows. */
    public static void updateLaf() {
        installer.updateLaf();
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
     * Get a view of all currently registered defaults init tasks. Modification will also mutate the
     * collection itself.
     *
     * @return collection of init tasks.
     */
    public static Collection<DefaultsAdjustmentTask> getUserDefaultsAdjustmentTasks() {
        return uiDefaultsTasks;
    }

    /**
     * Register a task to modify the laf defaults.
     *
     * @param task the defaults init task.
     */
    public static void registerInitTask(final DefaultsInitTask task) {
        uiInitTasks.add(task);
    }

    /**
     * Remove a registered task to modify the laf defaults.
     *
     * @param task the defaults init task.
     */
    public static void removeInitTask(final DefaultsInitTask task) {
        uiInitTasks.remove(task);
    }

    /**
     * Get a view of all currently registered ui init tasks. Modification will also mutate the
     * collection itself.
     *
     * @return collection of init tasks.
     */
    public static Collection<DefaultsInitTask> getUserInitTasks() {
        return uiInitTasks;
    }

    /**
     * Get the closest match of a registered theme for the given theme.
     *
     * @param theme the theme to match to.
     * @return the closes match. NonNull.
     */
    public static Theme getClosestMatchForTheme(final Theme theme) {
        if (theme == null) return themeForPreferredStyle(null);
        for (Theme registered : getRegisteredThemes()) {
            if (registered.equals(theme)) return registered;
        }
        for (Theme registered : getRegisteredThemes()) {
            if (registered.getThemeClass().equals(theme.getThemeClass())) return registered;
        }
        return themeForPreferredStyle(null);
    }

    static void notifyThemeInstalled() {
        installer.notifyThemeInstalled(getTheme());
    }
}
