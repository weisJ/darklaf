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
package com.github.weisj.darklaf.icons;

import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.util.LazyValue;
import com.github.weisj.darklaf.util.LogUtil;

/**
 * @author Jannis Weis
 */
public final class IconLoader {
    private static final Logger LOGGER = LogUtil.getLogger(IconLoader.class);
    private static final Map<Class<?>, IconLoader> iconLoaderMap = new HashMap<>();
    private static final LazyValue<IconLoader> instance = new LazyValue<>(() -> get(null));

    private static Object currentThemeKey;
    private static AwareIconStyle currentAwareStyle;

    private static final int DEFAULT_W = 16;
    private static final int DEFAULT_H = 16;
    private final Class<?> parentClass;

    private boolean cacheEnabled = true;
    private final Map<IconKey, DarkUIAwareIcon> awareIconMap = new HashMap<>();
    private final Map<IconKey, Icon> iconMap = new HashMap<>();

    private IconLoader(final Class<?> parentClass) {
        this.parentClass = parentClass;
        iconLoaderMap.put(parentClass, this);
    }

    /**
     * Get the default icon loader which resolves resources from the root directory of the jar.
     *
     * @return the default icon loader.
     */
    public static IconLoader get() {
        return instance.get();
    }

    /**
     * Get an icon loader which resolves resources from the class directory of the parent class.
     *
     * @return the default icon loader.
     */
    public static IconLoader get(final Class<?> parentClass) {
        if (iconLoaderMap.containsKey(parentClass)) {
            return iconLoaderMap.get(parentClass);
        } else {
            IconLoader loader = new IconLoader(parentClass);
            iconLoaderMap.put(parentClass, loader);
            return loader;
        }
    }

    /**
     * Sets whether icons should be cached or
     *
     * @param cacheEnabled true if caching is enabled.
     */
    public void setCacheEnabled(final boolean cacheEnabled) {
        this.cacheEnabled = cacheEnabled;
    }

    /**
     * Returns whether icons are cached when creating them.
     *
     * @return true if caching is enabled.
     */
    public boolean isCacheEnabled() {
        return cacheEnabled;
    }

    /**
     * Updates the style of aware icons.
     * Changing it will force aware icons to change their appearance accordingly.
     *
     * @param style the new style.
     */
    public static void updateAwareStyle(final AwareIconStyle style) {
        currentAwareStyle = style;
    }

    /**
     * Updates the object associated with the current theme.
     * Changing it will force themed icons to refresh their colors.
     * This doesn't need to be any specific type as it is simply a marker object.
     *
     * @param theme the new theme object.
     */
    public static void updateThemeStatus(final Object theme) {
        currentThemeKey = theme;
    }

    /**
     * Get the current aware icon style.
     *
     * @return the aware icon style.
     */
    public static AwareIconStyle getAwareStyle() {
        return currentAwareStyle;
    }

    /**
     * Get the object associated to the current theme.
     * This may not be any specific type as it is simply a marker object.
     *
     * @return the current theme object.
     */
    public static Object getThemeStatus() {
        return currentThemeKey;
    }

    /**
     * Get an aware icon. If [path] is the search root of the current icon loader then the icon resource will be
     * resolved to [path]/dark/[icon_path] and [path]/light/[icon_path]
     * Uses 16x16 icons by default.
     *
     * @param  path the path to the icon resource described as above.
     * @return      the icon.
     */
    public DarkUIAwareIcon getUIAwareIcon(final String path) {
        return getUIAwareIcon(path, DEFAULT_W, DEFAULT_H);
    }

    /**
     * Get an aware icon. If [path] is the search root of the current icon loader then the icon resource will be
     * resolved to [path]/dark/[icon_path] and [path]/light/[icon_path]
     *
     * @param  path the path to the icon resource described as above.
     * @param  w    the icon width.
     * @param  h    the icon height.
     * @return      the icon.
     */
    public DarkUIAwareIcon getUIAwareIcon(final String path, final int w, final int h) {
        IconKey key = new IconKey(path, w, h);
        if (isCacheEnabled() && awareIconMap.containsKey(key)) {
            return awareIconMap.get(key);
        } else {
            DarkUIAwareIcon icon = createUIAwareIcon(path, w, h);
            cache(awareIconMap, key, icon);
            return icon;
        }
    }

    /*
     * Helper method to create the icons.
     */
    protected DarkUIAwareIcon createUIAwareIcon(final String name, final int w, final int h) {
        return new DarkUIAwareIcon("dark/" + name, "light/" + name, w, h, parentClass);
    }

    /**
     * Get an icon at the specified location. The icon type is deduced from the file name. i.e. "folder/icon.svg"
     * will be loaded as an svg.icon.
     * Uses 16x16 icons by default.
     *
     * @see         #get(Class)
     * @see         #get
     * @param  path the path to the icon with respect to the IconLoader resource root.
     * @return      the icon.
     */
    public Icon getIcon(final String path) {
        return getIcon(path, DEFAULT_W, DEFAULT_H);
    }

    /**
     * Get an icon at the specified location. The icon type is deduced from the file name. i.e. "folder/icon.svg"
     * will be loaded as an svg.icon.
     * Uses 16x16 icons by default.
     *
     * @see           #get(Class)
     * @see           #get
     * @param  path   the path to the icon with respect to the IconLoader resource root.
     * @param  themed determines whether the icon is themed. This only has an effect on svg icons.
     * @return        the icon.
     */
    public Icon getIcon(final String path, final boolean themed) {
        return getIcon(path, DEFAULT_W, DEFAULT_H, themed);
    }

    /**
     * Get an icon at the specified location. The icon type is deduced from the file name. i.e. "folder/icon.svg"
     * will be loaded as an svg.icon.
     *
     * @see         #get(Class)
     * @see         #get
     * @param  path the path to the icon with respect to the IconLoader resource root.
     * @param  w    the icon width.
     * @param  h    the icon height.
     * @return      the icon.
     */
    public Icon getIcon(final String path, final int w, final int h) {
        return getIcon(path, w, h, false);
    }

    /**
     * Get an icon at the specified location. The icon type is deduced from the file name. i.e. "folder/icon.svg"
     * will be loaded as an svg.icon.
     *
     * @see           #get(Class)
     * @see           #get
     * @param  path   the path to the icon with respect to the IconLoader resource root.
     * @param  w      the icon width.
     * @param  h      the icon height.
     * @param  themed determines whether the icon is themed. This only has an effect on svg icons.
     * @return        the icon.
     */
    public Icon getIcon(final String path, final int w, final int h, final boolean themed) {
        IconKey key = new IconKey(path, w, h);

        if (isCacheEnabled()) {
            if (iconMap.containsKey(key)) {
                return iconMap.get(key);
            } else if (awareIconMap.containsKey(key)) {
                return awareIconMap.get(key);
            }
            key.w = -1; // Enable wild card search. Find any icon that matches path.
            if (iconMap.containsKey(key)) {
                Icon icon = iconMap.get(key);
                if (icon instanceof DerivableIcon) {
                    // If the desired icon is an DarkSVGIcon we can create a view that shares the underlying svg with
                    // the existing icon.
                    Icon derived = ((DerivableIcon<Icon>) icon).derive(w, h);
                    key.w = w;
                    cache(iconMap, key, derived);
                    return derived;
                }
            }
        }

        // Icon not found or caching is disabled.

        key.w = w; // Restore key.
        if (path.endsWith(".svg")) {
            Icon icon = loadSVGIcon(path, w, h, themed);
            cache(iconMap, key, icon);
            return icon;
        } else {
            Icon icon = new LazyImageIcon(path, key, parentClass);
            cache(iconMap, key, icon);
            return icon;
        }
    }

    private <T extends Icon> void cache(final Map<IconKey, T> iconMap, final IconKey key, final T icon) {
        if (cacheEnabled) {
            iconMap.put(key, icon);
        }
    }

    /**
     * Get an svg icon at the specified location.
     * will be loaded as an svg.icon.
     * Uses 16x16 icons by default.
     *
     * @see           #get(Class)
     * @see           #get
     * @param  path   the path to the icon with respect to the IconLoader resource root.
     * @param  themed determines whether the icon is themed. This only has an effect on svg icons.
     * @return        the icon.
     */
    public Icon loadSVGIcon(final String path, final boolean themed) {
        return loadSVGIcon(path, DEFAULT_W, DEFAULT_H, themed);
    }

    /**
     * Get an svg icon at the specified location.
     * will be loaded as an svg.icon.
     *
     * @see           #get(Class)
     * @see           #get
     * @param  path   the path to the icon with respect to the IconLoader resource root.
     * @param  w      the icon width.
     * @param  h      the icon height.
     * @param  themed determines whether the icon is themed. This only has an effect on svg icons.
     * @return        the icon.
     */
    public Icon loadSVGIcon(final String path, final int w, final int h, final boolean themed) {
        return loadSVGIcon(path, w, h, themed, null);
    }

    /**
     * Get an svg icon at the specified location.
     * will be loaded as an svg.icon.
     *
     * @see                #get(Class)
     * @see                #get
     * @param  path        the path to the icon with respect to the IconLoader resource root.
     * @param  w           the icon width.
     * @param  h           the icon height.
     * @param  themed      determines whether the icon is themed. This only has an effect on svg icons.
     * @param  propertyMap the property map for resolving themed icon properties. If null the UIDefaults will be used.
     * @return             the icon.
     */
    public Icon loadSVGIcon(final String path, final int w, final int h, final boolean themed,
                            final Map<Object, Object> propertyMap) {
        try {
            if (themed) {
                final URI uri = Objects.requireNonNull(getResource(path).toURI());
                if (propertyMap != null) {
                    return new CustomThemedIcon(uri, w, h, propertyMap);
                } else {
                    return new ThemedSVGIcon(uri, w, h);
                }
            } else {
                return new DarkSVGIcon(Objects.requireNonNull(getResource(path).toURI()), w, h);
            }
        } catch (NullPointerException | URISyntaxException e) {
            LOGGER.log(Level.SEVERE, "Exception while loading '" + path + "'" + ". Resolving from " + parentClass,
                       e.getStackTrace());
        }
        return EmptyIcon.create(0);
    }

    /**
     * Create an image icon.
     *
     * @param  path        the path to the icon with respect to the IconLoader resource root.
     * @param  description description of the icon as described in {@link ImageIcon#setDescription(String)}
     * @return             the ImageIcon.
     */
    public ImageIcon createImageIcon(final String path,
                                     final String description) {
        java.net.URL imgURL = getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL, description);
        } else {
            LOGGER.severe("Could not find icon file: '" + path + "'");
            return null;
        }
    }

    protected URL getResource(final String name) {
        if (parentClass != null) {
            return parentClass.getResource(name);
        } else {
            return getClass().getClassLoader().getResource(name);
        }
    }

    protected static final class IconKey {
        final String path;
        int w;
        int h;

        private IconKey(final String path, final int w, final int h) {
            this.path = path;
            this.w = w;
            this.h = h;
        }

        @Override
        public int hashCode() {
            return Objects.hash(path, w, h);
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            IconKey iconKey = (IconKey) o;

            if (iconKey.w == -1 || iconKey.h == -1) {
                // Math any size.
                return Objects.equals(path, iconKey.path);
            }
            if (w != iconKey.w) return false;
            if (h != iconKey.h) return false;
            return Objects.equals(path, iconKey.path);
        }

        @Override
        public String toString() {
            return "[path=" + path + ", w=" + w + ", h=" + h + "]";
        }
    }
}
