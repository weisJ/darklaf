package com.weis.darklaf.icons;

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

public final class IconLoader {
    private static final Logger LOGGER = Logger.getLogger(IconLoader.class.getName());

    public UIAwareIcon getUIAwareIcon(final String path, final int w, final int h) {
        IconKey key = new IconKey(path, w, h);
        if (awareIconMap.containsKey(key)) {
            return awareIconMap.get(key);
        } else {
            UIAwareIcon icon = create(path, w, h);
            awareIconMap.put(key, icon);
            return icon;
        }
    }

    private static final Map<Class<?>, IconLoader> iconLoaderMap = new HashMap<>();
    private static final IconLoader instance = new IconLoader(IconLoader.class);

    private final Class<?> parentClass;
    private final Map<IconKey, UIAwareIcon> awareIconMap = new HashMap<>();
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
            return new IconLoader(parentClass);
        }
    }

    public UIAwareIcon getUIAwareIcon(final String path) {
        return getUIAwareIcon(path, 16, 16);
    }

    public Icon getIcon(final String path, final int w, final int h) {
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
                Icon icon = loadIcon(path, w, h);
                iconMap.put(key, icon);
                return icon;
            } else {
                Icon icon = new LazyImageIcon(path, key, IconLoader.class);
                iconMap.put(key, icon);
                return icon;
            }
        }
    }

    public Icon getIcon(final String path) {
        return getIcon(path, 16, 16);
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

    /*
     * Helper method to create the icons.
     */
    @Contract(value = "_, _, _ -> new", pure = true)
    @NotNull
    public UIAwareIcon create(@NotNull final String name, final int w, final int h) {
        return new UIAwareIcon("dark/" + name, "light/" + name, w, h, IconLoader.class);
    }

    @NotNull
    public Icon loadIcon(@NotNull final String name) {
        return loadIcon(name, 16, 16);
    }

    @NotNull
    @Contract("_, _, _ -> new")
    public Icon loadIcon(@NotNull final String name, final int w, final int h) {
        try {
            LOGGER.info("Loading icon '" + name + "'. Resolving from " + parentClass);
            return new DarkSVGIcon(Objects.requireNonNull(parentClass.getResource(name).toURI()), w, h);
        } catch (NullPointerException | URISyntaxException e) {
            LOGGER.log(Level.SEVERE, "Exception while loading '" + name + "'" + ". Resolving from " + parentClass,
                       e.getStackTrace());
        }
        return EmptyIcon.create(0);
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
}
