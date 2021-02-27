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
package com.github.weisj.darklaf.settings;

import java.io.Serializable;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.platform.ThemePreferencesHandler;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.theme.info.FontSizeRule;

public class SettingsConfiguration implements Serializable {

    private boolean isSystemPreferencesEnabled;
    private boolean isAccentColorFollowsSystem;
    private boolean isFontSizeFollowsSystem;
    private boolean isThemeFollowsSystem;
    private boolean isSelectionColorFollowsSystem;

    private AccentColorRule accentColorRule;
    private FontSizeRule fontSizeRule;

    private Theme theme;

    public void load(final SettingsConfiguration config) {
        setSystemPreferencesEnabled(config.isSystemPreferencesEnabled());
        setAccentColorFollowsSystem(config.isAccentColorFollowsSystem());
        setSelectionColorFollowsSystem(config.isSelectionColorFollowsSystem());
        setFontSizeFollowsSystem(config.isFontSizeFollowsSystem());
        setThemeFollowsSystem(config.isThemeFollowsSystem());

        setFontSizeRule(config.getFontSizeRule());
        setAccentColorRule(config.getAccentColorRule());
        setTheme(config.getTheme());
    }

    /**
     * Returns whether the option to follow the system settings is enabled.
     *
     * @return true if enabled.
     * @see #isSystemPreferencesEnabled() (boolean)
     * @see #isThemeFollowsSystem()
     * @see #isAccentColorFollowsSystem()
     * @see #isSelectionColorFollowsSystem()
     * @see #isFontSizeFollowsSystem()
     */
    public boolean isSystemPreferencesEnabled() {
        return isSystemPreferencesEnabled;
    }

    /**
     * Returns whether the accent color follows the system settings.
     *
     * @return true if accent color follows system settings.
     * @see #setAccentColorFollowsSystem(boolean)
     */
    public boolean isAccentColorFollowsSystem() {
        return isAccentColorFollowsSystem;
    }

    /**
     * Returns whether the font size follows the system settings.
     *
     * @return true if font size follows system settings.
     * @see #setFontSizeFollowsSystem(boolean)
     */
    public boolean isFontSizeFollowsSystem() {
        return isFontSizeFollowsSystem;
    }

    /**
     * Returns whether the selection color follows the system settings.
     *
     * @return true if selection color follows system settings.
     * @see #setSelectionColorFollowsSystem(boolean)
     */
    public boolean isSelectionColorFollowsSystem() {
        return isSelectionColorFollowsSystem;
    }

    /**
     * Returns whether the theme follows the system settings.
     *
     * @return true if theme follows system settings.
     * @see #setThemeFollowsSystem(boolean)
     */
    public boolean isThemeFollowsSystem() {
        return isThemeFollowsSystem;
    }

    /**
     * Get the currently selected accent color rule. This is not the same as the rule of
     * {@link LafManager#getTheme()} as the current settings might not have been applied.
     *
     * @return the current selected accent color rule.
     * @see #setAccentColorRule(AccentColorRule)
     */
    public AccentColorRule getAccentColorRule() {
        return accentColorRule;
    }

    /**
     * Get the currently selected font size rule. This is not the same as the rule of
     * {@link LafManager#getTheme()} as the current settings might not have been applied.
     *
     * @return the current selected font size rule.
     * @see #setFontSizeRule(FontSizeRule)
     */
    public FontSizeRule getFontSizeRule() {
        return fontSizeRule;
    }

    /**
     * Get the currently selected theme. This is not the same as {@link LafManager#getTheme()} as the
     * current settings might not have been applied.
     *
     * @return the current selected theme.
     * @see #setTheme(Theme)
     */
    public Theme getTheme() {
        return theme;
    }

    /**
     * Enables the option to follow system preferences.
     *
     * @param enabled true if enabled.
     * @see #setAccentColorFollowsSystem(boolean)
     * @see #setSelectionColorFollowsSystem(boolean)
     * @see #setFontSizeFollowsSystem(boolean)
     * @see #setThemeFollowsSystem(boolean)
     */
    public void setSystemPreferencesEnabled(final boolean enabled) {
        isSystemPreferencesEnabled = enabled;
    }

    /**
     * Sets whether the accent color should follow the system settings. This only works if
     * {@link #isSelectionColorFollowsSystem()} is true.
     *
     * @param accentColorFollowsSystem true if accent color should follow system.
     * @see #isAccentColorFollowsSystem()
     */
    public void setAccentColorFollowsSystem(final boolean accentColorFollowsSystem) {
        isAccentColorFollowsSystem = accentColorFollowsSystem;
    }

    /**
     * Sets whether the font size should follow the system settings. This only works if
     * {@link #isSelectionColorFollowsSystem()} is true.
     *
     * @param fontSizeFollowsSystem true if font size should follow system.
     * @see #isFontSizeFollowsSystem()
     */
    public void setFontSizeFollowsSystem(final boolean fontSizeFollowsSystem) {
        isFontSizeFollowsSystem = fontSizeFollowsSystem;
    }

    /**
     * Sets whether the selection color should follow the system settings. This only works if
     * {@link #isSelectionColorFollowsSystem()} is true.
     *
     * @param selectionColorFollowsSystem true if selection color should follow system.
     * @see #isSelectionColorFollowsSystem()
     */
    public void setSelectionColorFollowsSystem(final boolean selectionColorFollowsSystem) {
        isSelectionColorFollowsSystem = selectionColorFollowsSystem;
    }

    /**
     * Sets whether the theme should follow the system settings. This only works if
     * {@link #isSelectionColorFollowsSystem()} is true.
     *
     * @param themeFollowsSystem true if theme should follow system.
     * @see #isThemeFollowsSystem()
     */
    public void setThemeFollowsSystem(final boolean themeFollowsSystem) {
        isThemeFollowsSystem = themeFollowsSystem;
    }

    /**
     * Sets the font accent color rule.
     *
     * @param accentColorRule the accent color rule.
     * @see #getAccentColorRule()
     */
    public void setAccentColorRule(final AccentColorRule accentColorRule) {
        this.accentColorRule = accentColorRule;
    }

    /**
     * Sets the font size rule.
     *
     * @param fontSizeRule the font size rule.
     * @see #getFontSizeRule()
     */
    public void setFontSizeRule(final FontSizeRule fontSizeRule) {
        this.fontSizeRule = fontSizeRule;
    }

    /**
     * Sets the theme.
     *
     * @param theme the theme.
     * @see #getTheme()
     */
    public void setTheme(final Theme theme) {
        this.theme = theme;
    }

    public boolean isSystemPreferencesSupported() {
        return ThemePreferencesHandler.getSharedInstance().canReport();
    }

    public boolean isSystemAccentColorSupported() {
        return isSystemAccentColorSupported(true);
    }

    public boolean isSystemFontSizeSupported() {
        return isSystemFontSizeSupported(true);
    }

    public boolean isSystemSelectionColorSupported() {
        return isSystemSelectionColorSupported(true);
    }

    public boolean isSystemThemeSupported() {
        return isSystemThemeSupported(true);
    }

    protected final boolean isSystemAccentColorSupported(final boolean checkSystemPreferencesEnabled) {
        return (!checkSystemPreferencesEnabled || isSystemPreferencesEnabled())
                && ThemePreferencesHandler.getSharedInstance().supportsNativeAccentColor();

    }

    protected final boolean isSystemFontSizeSupported(final boolean checkSystemPreferencesEnabled) {
        return (!checkSystemPreferencesEnabled || isSystemPreferencesEnabled())
                && ThemePreferencesHandler.getSharedInstance().supportsNativeFontSize();
    }

    protected final boolean isSystemSelectionColorSupported(final boolean checkSystemPreferencesEnabled) {
        return (!checkSystemPreferencesEnabled || isSystemPreferencesEnabled())
                && ThemePreferencesHandler.getSharedInstance().supportsNativeSelectionColor();
    }

    protected final boolean isSystemThemeSupported(final boolean checkSystemPreferencesEnabled) {
        return (!checkSystemPreferencesEnabled || isSystemPreferencesEnabled())
                && ThemePreferencesHandler.getSharedInstance().supportsNativeTheme();
    }

    @Override
    public String toString() {
        return "SettingsConfiguration{" + "isSystemPreferencesEnabled=" + isSystemPreferencesEnabled
                + ", isAccentColorFollowsSystem=" + isAccentColorFollowsSystem + ", isFontSizeFollowsSystem="
                + isFontSizeFollowsSystem + ", isThemeFollowsSystem=" + isThemeFollowsSystem
                + ", isSelectionColorFollowsSystem=" + isSelectionColorFollowsSystem + ", accentColorRule="
                + accentColorRule + ", fontSizeRule=" + fontSizeRule + ", theme=" + theme + '}';
    }
}
