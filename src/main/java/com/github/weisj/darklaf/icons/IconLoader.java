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
package com.github.weisj.darklaf.icons;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 */
public final class IconLoader {
    private static final Logger LOGGER = Logger.getLogger(IconLoader.class.getName());
    private static final Map<Class<?>, IconLoader> iconLoaderMap = new HashMap<>();
    private static final IconLoader instance = new IconLoader(IconLoader.class);
    private final Class<?> parentClass;
    private final Map<IconKey, DarkUIAwareIcon> awareIconMap = new HashMap<>();
    private final Map<IconKey, Icon> iconMap = new HashMap<>();

    @Contract(pure = true)
    private IconLoader(final Class<?> parentClass) {
        this.parentClass = parentClass;
        iconLoaderMap.put(parentClass, this);
    }

    @Contract(pure = true)
    public static IconLoader get() {
        return instance;
    }

    public static IconLoader get(final Class<?> parentClass) {
        if (iconLoaderMap.containsKey(parentClass)) {
            return iconLoaderMap.get(parentClass);
        } else {
            var loader = new IconLoader(parentClass);
            iconLoaderMap.put(parentClass, loader);
            return loader;
        }
    }

    public DarkUIAwareIcon getUIAwareIcon(final String path) {
        return getUIAwareIcon(path, 16, 16);
    }

    public DarkUIAwareIcon getUIAwareIcon(final String path, final int w, final int h) {
        IconKey key = new IconKey(path, w, h);
        if (awareIconMap.containsKey(key)) {
            return awareIconMap.get(key);
        } else {
            DarkUIAwareIcon icon = create(path, w, h);
            awareIconMap.put(key, icon);
            return icon;
        }
    }

    /*
     * Helper method to create the icons.
     */
    @Contract(value = "_, _, _ -> new", pure = true)
    @NotNull
    public DarkUIAwareIcon create(@NotNull final String name, final int w, final int h) {
        return new DarkUIAwareIcon("dark/" + name, "light/" + name, w, h, parentClass);
    }

    public Icon getIcon(final String path) {
        return getIcon(path, 16, 16);
    }

    public Icon getIcon(final String path, final int w, final int h) {
        return getIcon(path, w, h, false);
    }

    public Icon getIcon(final String path, final int w, final int h, final boolean themed) {
        IconKey key = new IconKey(path, w, h);
        if (iconMap.containsKey(key)) {
            return iconMap.get(key);
        } else if (awareIconMap.containsKey(key)) {
            return awareIconMap.get(key);
        } else {
            key.w = -1; //Enable wild card search. Find any icon that matches path.
            if (iconMap.containsKey(key)) {
                var icon = iconMap.get(key);
                if (icon instanceof DarkSVGIcon) {
                    //If the desired icon is an DarkSVGIcon we can create a view that shares the underlying svg with
                    //the existing icon.
                    Icon derived = ((DarkSVGIcon) icon).derive(w, h);
                    key.w = w;
                    iconMap.put(key, derived);
                    return derived;
                }
            }
            key.w = w; //Restore key.
            if (path.endsWith(".svg")) {
                Icon icon = loadSVGIcon(path, w, h, themed);
                iconMap.put(key, icon);
                return icon;
            } else {
                Icon icon = new LazyImageIcon(path, key, IconLoader.class);
                iconMap.put(key, icon);
                return icon;
            }
        }
    }

    @NotNull
    public Icon loadSVGIcon(@NotNull final String name, final int w, final int h, final boolean themed) {
        try {
            LOGGER.info("Loading icon '" + name + "'. Resolving from " + parentClass);
            if (themed) {
                return new ThemedSVGIcon(Objects.requireNonNull(parentClass.getResource(name).toURI()), w, h);
            } else {
                return new DarkSVGIcon(Objects.requireNonNull(parentClass.getResource(name).toURI()), w, h);
            }
        } catch (NullPointerException | URISyntaxException e) {
            LOGGER.log(Level.SEVERE, "Exception while loading '" + name + "'" + ". Resolving from " + parentClass,
                       e.getStackTrace());
        }
        return EmptyIcon.create(0);
    }

    @NotNull
    public Icon loadSVGIcon(@NotNull final String name, final boolean themed) {
        return loadSVGIcon(name, 16, 16, themed);
    }

    @Nullable
    public ImageIcon createImageIcon(final String path,
                                     final String description) {
        java.net.URL imgURL = parentClass.getResource(path);
        if (imgURL != null) {
            return new ImageIcon(imgURL, description);
        } else {
            LOGGER.severe("Could not find icon file: '" + path + "'");
            return null;
        }
    }

    public static final class IconKey {
        final String path;
        int w;
        int h;

        @Contract(pure = true)
        private IconKey(final String path, final int w, final int h) {
            this.path = path;
            this.w = w;
            this.h = h;
        }

        @Override
        public int hashCode() {
            return Objects.hash(path, w, h);
        }

        @Contract(value = "null -> false", pure = true)
        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            IconKey iconKey = (IconKey) o;

            if (iconKey.w == -1 || iconKey.h == -1) {
                //Math any size.
                return Objects.equals(path, iconKey.path);
            }
            if (w != iconKey.w) return false;
            if (h != iconKey.h) return false;
            return Objects.equals(path, iconKey.path);
        }

        @NotNull
        @Contract(pure = true)
        @Override
        public String toString() {
            return "[path=" + path + ", w=" + w + ", h=" + h + "]";
        }
    }
}
