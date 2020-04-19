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
package com.github.weisj.darklaf.theme;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;
import java.util.Objects;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.text.html.StyleSheet;

import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.theme.info.*;

/**
 * @author Jannis Weis
 */
public abstract class Theme implements Comparable<Theme>, Comparator<Theme> {
    private static final Logger LOGGER = Logger.getLogger(Theme.class.getName());

    private final FontSizeRule fontSizeRule;
    private final AccentColorRule accentColorRule;

    public Theme() {
        this(null, null);
    }

    public Theme(final FontSizeRule fontSizeRule, final AccentColorRule accentColorRule) {
        this.fontSizeRule = fontSizeRule != null ? fontSizeRule : FontSizeRule.getDefault();
        this.accentColorRule = accentColorRule != null ? accentColorRule : AccentColorRule.getDefault();
    }

    /**
     * Create a derived theme with the given {@link FontSizeRule} and {@link AccentColorRule}.
     *
     * @param  fontSizeRule    the font size rule.
     * @param  accentColorRule the accent color rule.
     * @return                 the derived theme.
     */
    public Theme derive(final FontSizeRule fontSizeRule, final AccentColorRule accentColorRule) {
        return new ThemeDelegate(this, fontSizeRule, accentColorRule);
    }

    /**
     * Creates a copy of this theme. This is not equivalent to {@link #clone()} in the sense that
     * <code>clone().getClass() == this.getClass()</code> and <code>copy().getClass() != this.getClass()</code>.
     * Nonetheless the copy theme behaves exactly the same as the original.
     *
     * @return a copy of the theme.
     */
    public Theme copy() {
        return new ThemeDelegate(this);
    }

    /**
     * Returns whether the theme is a dark theme. This is used to determine the default mode for [aware] icons.
     *
     * @param  theme the theme.
     * @return       true if dark.
     */
    public static boolean isDark(final Theme theme) {
        if (theme == null) return false;
        return theme.getColorToneRule() == ColorToneRule.DARK;
    }

    /**
     * Returns whether the theme is a high contrast theme.
     *
     * @param  theme the theme.
     * @return       true if the theme is a high contrast theme.
     */
    public static boolean isHighContrast(final Theme theme) {
        if (theme == null) return false;
        return theme.getContrastRule() == ContrastRule.HIGH_CONTRAST;
    }

    /**
     * Load the theme defaults.
     * <p>
     * Note: When overwriting a theme you also have overwrite {@link #getLoaderClass()} to return the class of the theme
     * you are overwriting. In this case you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadDefaults(final Properties properties, final UIDefaults currentDefaults) {
        PropertyLoader.putProperties(loadPropertyFile("defaults"), properties, currentDefaults);
    }

    /**
     * Customize the global values.
     * <p>
     * Note: When overwriting a theme you also have overwrite {@link #getLoaderClass()} to return the class of the theme
     * you are overwriting. In this case you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void customizeGlobals(final Properties properties, final UIDefaults currentDefaults) {}

    /**
     * Customize the icon defaults.
     * <p>
     * Note: When overwriting a theme you also have overwrite {@link #getLoaderClass()} to return the class of the theme
     * you are overwriting. In this case you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param properties      the properties to load the value into.
     * @param currentDefaults the current ui defaults.
     */
    public void customizeIconTheme(final Properties properties, final UIDefaults currentDefaults) {}

    /**
     * Load the general properties file for the icon themes.
     * <p>
     * Note: When overwriting a theme you also have overwrite {@link #getLoaderClass()} to return the class of the theme
     * you are overwriting. In this case you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param properties      the properties to load the value into.
     * @param currentDefaults the current ui defaults.
     */
    public void loadIconTheme(final Properties properties, final UIDefaults currentDefaults) {
        PresetIconRule iconTheme = getPresetIconRule();
        Properties props;
        switch (iconTheme) {
            case DARK :
                props = PropertyLoader.loadProperties(Theme.class, "dark_icons", "icon_presets/");
                break;
            case LIGHT :
                props = PropertyLoader.loadProperties(Theme.class, "light_icons", "icon_presets/");
                break;
            case NONE :
            default :
                props = loadPropertyFile("icons");
        }
        PropertyLoader.putProperties(props, properties, currentDefaults);
    }

    /**
     * Customize the platform defaults.
     * <p>
     * Note: When overwriting a theme you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void customizePlatformProperties(final Properties properties, final UIDefaults currentDefaults) {}

    /**
     * Customize the ui defaults.
     * <p>
     * Note: When overwriting a theme you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param properties      the properties to load the values into.
     * @param currentDefaults the current ui defaults.
     */
    public void customizeUIProperties(final Properties properties, final UIDefaults currentDefaults) {}

    /**
     * The preset icon theme.
     *
     * @return the icon theme.
     */
    protected abstract PresetIconRule getPresetIconRule();

    /**
     * Load custom properties that are located under {@link #getResourcePath()}, with the name {@link
     * #getPrefix()}_{propertySuffix}.properties
     * <p>
     * Note: When overwriting a theme you should use {@link #loadWithClass(String, Class)} instead of {@link
     * #load(String)}.
     *
     * @param propertySuffix  the property suffix.
     * @param properties      the properties to load into.
     * @param currentDefaults the current ui defaults.
     */
    protected final void loadCustomProperties(final String propertySuffix, final Properties properties,
                                              final UIDefaults currentDefaults) {
        PropertyLoader.putProperties(loadPropertyFile(propertySuffix), properties, currentDefaults);
    }

    /**
     * Load a .properties file using {@link #getLoaderClass()}} to resolve the file path.
     * <p>
     * Note: When overwriting a theme you should use {@link #loadWithClass(String, Class)} instead.
     *
     * @param  name the properties file to load.
     * @return      the properties.
     */
    protected final Properties load(final String name) {
        return loadWithClass(name, getLoaderClass());
    }

    /**
     * Load a .properties file.
     *
     * @param  name        the properties file to load.
     * @param  loaderClass the class to resolve the file location from.
     * @return             the properties.
     */
    protected final Properties loadWithClass(final String name, final Class<?> loaderClass) {
        final Properties properties = new Properties();
        try (InputStream stream = loaderClass.getResourceAsStream(name)) {
            properties.load(stream);
        } catch (Exception e) {
            LOGGER.log(Level.SEVERE, "Could not load " + name + ".properties. " + e.getMessage(), e.getStackTrace());
        }
        return properties;
    }

    /**
     * Load the css style sheet used for html display in text components with a {@link
     * javax.swing.text.html.HTMLEditorKit}.
     *
     * @return the {@link StyleSheet}.
     */
    public final StyleSheet loadStyleSheet() {
        return loadStyleSheetWithClass(getLoaderClass());
    }

    /**
     * Load the css style sheet used for html display in text components with a {@link
     * javax.swing.text.html.HTMLEditorKit}.
     *
     * @param  loaderClass the class to resolve the location of the style sheet.
     * @return             the {@link StyleSheet}.
     */
    public final StyleSheet loadStyleSheetWithClass(final Class<?> loaderClass) {
        StyleSheet styleSheet = new StyleSheet();
        try (InputStream in = loaderClass.getResourceAsStream(getResourcePath() + getPrefix() + "_styleSheet.css");
             InputStreamReader inReader = new InputStreamReader(in, StandardCharsets.UTF_8);
             BufferedReader r = new BufferedReader(inReader)) {
            styleSheet.loadRules(r, null);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, e.toString(), e.getStackTrace());
        }
        return styleSheet;
    }

    @Override
    public String toString() {
        return getName();
    }

    /**
     * The path to the resource location relative to the classpath of {@link #getLoaderClass()}.
     *
     * @return the relative resource path
     */
    protected String getResourcePath() {
        return "";
    }

    /**
     * Get the prefix for resource loading.
     *
     * @return the prefix for loading resources.
     */
    public abstract String getPrefix();

    /**
     * Get the name of this theme.
     *
     * @return the name of the theme.
     */
    public abstract String getName();

    /**
     * The class used to determine the runtime location of resources. It is advised to explicitly return the class
     * instead of using {@link #getClass()} to protect against extending the theme.
     *
     * @return the loader class.
     */
    protected abstract Class<? extends Theme> getLoaderClass();

    /**
     * Get the path for the file [prefix]_[name].properties in the themes resource location.
     *
     * @param  name the of the file.
     * @return      the path relative to the location of {@link #getLoaderClass()}.
     */
    protected String getPropertyFilePath(final String name) {
        return getResourcePath() + getPrefix() + "_" + name + ".properties";
    }

    /**
     * Load the theme property file with the specified name. The name gets resolved to the resource location of the
     * theme adds the theme property prefix and appends ".properties" e.g. "test" ->
     * [resource_location]/[prefix_of_theme]_test.properties.
     *
     * @param  name the properties name.
     * @return      the properties.
     */
    public final Properties loadPropertyFile(final String name) {
        return loadPropertyFile(name, false);
    }

    /**
     * Load the theme property file with the specified name. The name gets resolved to the resource location of the
     * theme adds the theme property prefix and appends ".properties" e.g. "test" ->
     * [resource_location]/[prefix_of_theme]_test.properties.
     *
     * @param  name   the properties name.
     * @param  silent if true no warnings are issues if the file is not present. Instead, an empty property instance is
     *                returned.
     * @return        the properties.
     */
    public final Properties loadPropertyFile(final String name, final boolean silent) {
        Level level = LOGGER.getLevel();
        if (silent) LOGGER.setLevel(Level.OFF);
        Properties properties = load(getPropertyFilePath(name));
        LOGGER.setLevel(level);
        return properties;
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
     * Returns whether this theme supports custom accent colors.
     *
     * @return true if supported.
     */
    public boolean supportsCustomAccentColor() {
        return false;
    }

    /**
     * Returns whether this theme supports custom selection colors.
     *
     * @return true if supported.
     */
    public boolean supportsCustomSelectionColor() {
        return false;
    }

    /**
     * Returns the style rule for this theme.
     *
     * @return the style rule.
     */
    public abstract ColorToneRule getColorToneRule();

    /**
     * Returns contrast rule for the theme.
     *
     * @return the contrast rule.
     */
    public ContrastRule getContrastRule() {
        return ContrastRule.STANDARD;
    }

    /**
     * Get the font size rule for this theme.
     *
     * @return the font size rule.
     */
    public FontSizeRule getFontSizeRule() {
        return fontSizeRule;
    }

    /**
     * Get the accent color rule.
     *
     * @return the accent color rule.
     */
    public AccentColorRule getAccentColorRule() {
        return accentColorRule;
    }

    @Override
    public int compareTo(final Theme o) {
        if (o == null) return 1;
        int stringComp = getName().compareTo(o.getName());
        int contrastCompare = Boolean.compare(isHighContrast(this), isHighContrast(o));
        int toneCompare = Boolean.compare(isDark(this), isDark(o));
        if (contrastCompare != 0) return contrastCompare;
        if (toneCompare != 0) return toneCompare;
        return stringComp;
    }

    @Override
    public int compare(final Theme o1, final Theme o2) {
        if (o1 == null) return -1;
        return o1.compareTo(o2);
    }

    public Class<? extends Theme> getThemeClass() {
        return getClass();
    }

    /**
     * Returns whether the appearance of the given theme is equal to the appearance if [this].
     *
     * @param  theme the other theme.
     * @return       true if they appear equal.
     */
    public boolean appearsEqualTo(final Theme theme) {
        if (theme == null) return false;
        if (!Objects.equals(getThemeClass(), theme.getThemeClass())) return false;
        return Objects.equals(getAccentColorRule(), theme.getAccentColorRule())
               && Objects.equals(getColorToneRule(), theme.getColorToneRule())
               && Objects.equals(getContrastRule(), theme.getContrastRule())
               && Objects.equals(getFontSizeRule(), theme.getFontSizeRule());
    }
}
