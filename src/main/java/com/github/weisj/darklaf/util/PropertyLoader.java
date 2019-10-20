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
package com.github.weisj.darklaf.util;

import com.github.weisj.darklaf.icons.DarkUIAwareIcon;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.icons.IconLoader;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.InsetsUIResource;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class PropertyLoader {
    private static final Logger LOGGER = Logger.getLogger(PropertyLoader.class.getName());
    private static final IconLoader ICON_LOADER = IconLoader.get();
    private static final String DUAL_KEY = "[dual]";
    private static final String AWARE_KEY = "[aware]";
    private static final String PATCH_KEY = "[patch]";
    private static final String REFERENCE_PREFIX = "%";

    private static final Collection<ObjectRequest> objectsToLoad = new HashSet<>();

    public static void finish() {
        var cache = new HashMap<String, Object>();
        for (var request : objectsToLoad) {
            try {
                request.resolve(cache);
            } catch (RuntimeException e) {
                LOGGER.log(Level.SEVERE, "Could not load" + request, e.getMessage());
            }
        }
        cache.clear();
        reset();
    }

    public static void reset() {
        objectsToLoad.clear();
    }

    @NotNull
    public static Properties loadProperties(@NotNull final Class<?> clazz, final String name, final String path) {
        final Properties properties = new Properties();
        try (InputStream stream = clazz.getResourceAsStream(path + name + ".properties")) {
            properties.load(stream);
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Could not load" + name + ".properties", e.getMessage());
        }
        return properties;
    }

    public static void putProperties(@NotNull final Properties properties, final Properties accumulator,
                                     final UIDefaults currentDefaults) {
        for (final String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            var parsed = parseValue(key, value, accumulator);
            if (parsed instanceof ObjectRequest) {
                objectsToLoad.add((ObjectRequest) parsed);
            } else if (parsed != null) {
                accumulator.put(parseKey(key), parsed);
            } else {
                currentDefaults.remove(parseKey(key));
            }
        }
    }

    private static Object parseValue(@NotNull final String key, @NotNull final String value,
                                     final Map<Object, Object> defaults) {
        return parseValue(key, value, false, defaults);
    }

    private static String parseKey(@NotNull final String key) {
        return key.startsWith(REFERENCE_PREFIX) ? key.substring(REFERENCE_PREFIX.length()) : key;
    }

    @Nullable
    private static Object parseValue(@NotNull final String propertyKey, @NotNull final String value,
                                     final boolean ignoreRequest, final Map<Object, Object> defaults) {
        if ("null".equals(value)) {
            return null;
        }
        var key = propertyKey;
        boolean skipObjects = ignoreRequest;
        if (key.startsWith(REFERENCE_PREFIX)) {
            key = parseKey(key);
            skipObjects = true;
        }

        Object returnVal = new LoadError();
        if (key.endsWith("Insets")) {
            returnVal = parseInsets(value);
        } else if (!skipObjects
                && (key.endsWith("Border")
                || key.endsWith(".border")
                || key.endsWith(".component")
                || key.endsWith("Component")
                || key.endsWith("Renderer"))) {
            return new ObjectRequest(key, value);
        } else if (key.endsWith(".font")) {
            returnVal = parseFont(value);
        } else if (key.endsWith(".icon") || key.endsWith("Icon")) {
            returnVal = parseIcon(value);
        } else if (key.endsWith("Size") || key.endsWith(".size")) {
            returnVal = parseSize(value);
        } else if ("null".equalsIgnoreCase(value)) {
            returnVal = null;
        } else if (value.startsWith("%")) {
            var val = value.substring(1);
            if (!defaults.containsKey(val)) {
                LOGGER.warning("Could not reference value '" + val + "'while loading '" + key + "'. " +
                                       "May be a forward reference");
            }
            returnVal = defaults.get(val);
        }
        if (returnVal instanceof LoadError) {
            final Color color = ColorUtil.fromHex(value, null);
            final Integer invVal = getInteger(value);
            final Boolean boolVal = "true".equalsIgnoreCase(value)
                                    ? Boolean.TRUE
                                    : "false".equalsIgnoreCase(value) ? Boolean.FALSE : null;
            if (color != null && (value.length() == 6 || value.length() == 8)) {
                return new ColorUIResource(color);
            } else if (invVal != null) {
                return invVal;
            } else if (boolVal != null) {
                return boolVal;
            }
        } else {
            return returnVal;
        }
        return value;
    }

    @NotNull
    private static Object parseInsets(final String value) {
        final List<String> numbers = StringUtil.split(value, ",");
        return new InsetsUIResource(
                Integer.parseInt(numbers.get(0)),
                Integer.parseInt(numbers.get(1)),
                Integer.parseInt(numbers.get(2)),
                Integer.parseInt(numbers.get(3)));
    }

    @NotNull
    @Contract("_ -> new")
    private static Object parseFont(final String value) {
        try {
            final String[] decode = value.split("-");
            return new FontUIResource(decode[0], Integer.parseInt(decode[1]), Integer.parseInt(decode[2]));
        } catch (@NotNull final Exception e) {
            return new FontUIResource("Dialog", Font.PLAIN, 12);
        }
    }

    private static Icon parseIcon(@NotNull final String value) {
        String path = value;
        Dimension dim = new Dimension(16, 16);
        if (value.charAt(value.length() - 1) == ')') {
            int i = path.lastIndexOf('(');
            String dimVal = path.substring(i + 1, path.length() - 1);
            int[] values = Arrays.stream(dimVal.split(",", 2)).mapToInt(Integer::parseInt).toArray();
            dim.width = values[0];
            dim.height = values[1];
            path = path.substring(0, i);
        }
        if (path.charAt(path.length() - 1) == ']') {
            String tag = null;
            if (path.endsWith(DUAL_KEY)) {
                tag = DUAL_KEY;
            } else if (path.endsWith(AWARE_KEY)) {
                tag = AWARE_KEY;
            } else if (path.endsWith(PATCH_KEY)) {
                tag = PATCH_KEY;
            }
            if (tag == null) {
                throw new IllegalArgumentException("Invalid tag on icon path: '" + value + "'");
            }
            var iconPath = path.substring(0, path.length() - tag.length());
            if (tag.equals(PATCH_KEY)) {
                return ICON_LOADER.getIcon(iconPath, dim.width, dim.height, true);
            } else {
                DarkUIAwareIcon icon = ICON_LOADER.getUIAwareIcon(iconPath, dim.width, dim.height);
                if (tag.equals(DUAL_KEY)) {
                    return icon.getDual();
                } else {
                    return icon;
                }
            }
        }
        if (path.equals("empty")) {
            return EmptyIcon.create(dim.width, dim.height);
        }
        return ICON_LOADER.getIcon(path, dim.width, dim.height);
    }

    @NotNull
    private static DimensionUIResource parseSize(@NotNull final String value) {
        int[] dim = Arrays.stream(value.split(",", 2)).mapToInt(Integer::parseInt).toArray();
        return new DimensionUIResource(dim[0], dim[1]);
    }

    @Nullable
    private static Integer getInteger(@NotNull final String value) {
        try {
            return Integer.parseInt(value);
        } catch (@NotNull final NumberFormatException ignored) {
            return null;
        }
    }

    @Nullable
    private static Object parseObject(final String value) {
        try {
            return Class.forName(value).getDeclaredConstructor().newInstance();
        } catch (@NotNull final Exception ignored) { }
        return null;
    }

    private static final class LoadError {
    }

    private static final class ObjectRequest {

        final String value;
        final String key;

        @Contract(pure = true)
        private ObjectRequest(final String key, final String value) {
            this.key = key;
            this.value = value;
        }

        private void resolve(@NotNull final Map<String, Object> cache) {
            var defaults = UIManager.getLookAndFeelDefaults();
            if (cache.containsKey(value)) {
                defaults.put(key, cache.get(value));
            } else {
                Object obj = parseObject(value);
                if (obj == null) {
                    obj = parseValue(key, value, true, defaults);
                    if (obj instanceof ObjectRequest) {
                        LOGGER.severe("Failed to resolve object. " + this);
                        return;
                    }
                } else {
                    cache.put(value, obj);
                }
                if (obj == null) {
                    defaults.remove(key);
                } else {
                    defaults.put(key, obj);
                }
            }
        }

        @NotNull
        @Contract(pure = true)
        @Override
        public String toString() {
            return "[" + key + "," + value + "]";
        }
    }
}
