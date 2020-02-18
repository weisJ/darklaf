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

import com.github.weisj.darklaf.icons.DarkUIAwareIcon;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.StringUtil;

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.InsetsUIResource;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;
import java.util.*;
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
    private static final String THEMED_KEY = "[themed]";
    private static final String REFERENCE_PREFIX = "%";

    private static final Collection<ObjectRequest> objectsToLoad = new HashSet<>();

    public static void finish() {
        Map<String, Object> cache = new HashMap<>();
        for (ObjectRequest request : objectsToLoad) {
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


    public static Properties loadProperties(final Class<?> clazz, final String name, final String path) {
        final Properties properties = new Properties();
        String p = path + name + ".properties";
        try (InputStream stream = clazz.getResourceAsStream(p)) {
            properties.load(stream);
        } catch (IOException | NullPointerException e) {
            LOGGER.log(Level.SEVERE, "Could not load " + p + " " + e.getMessage(), e.getStackTrace());
        }
        return properties;
    }

    public static void putProperties(final Properties properties, final Properties accumulator,
                                     final UIDefaults currentDefaults) {
        putProperties(properties, accumulator, currentDefaults, ICON_LOADER);
    }

    public static void putProperties(final Properties properties, final Properties accumulator,
                                     final UIDefaults currentDefaults, final IconLoader iconLoader) {
        for (final String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            Object parsed = parseValue(key, value, accumulator, iconLoader);
            if (parsed instanceof ObjectRequest) {
                objectsToLoad.add((ObjectRequest) parsed);
            } else if (parsed != null) {
                accumulator.put(parseKey(key), parsed);
            } else {
                currentDefaults.remove(parseKey(key));
            }
        }
    }

    private static Object parseValue(final String key, final String value,
                                     final Map<Object, Object> defaults, final IconLoader iconLoader) {
        return parseValue(key, value, false, defaults, iconLoader);
    }

    private static String parseKey(final String key) {
        return key.startsWith(REFERENCE_PREFIX) ? key.substring(REFERENCE_PREFIX.length()) : key;
    }


    private static Object parseValue(final String propertyKey, final String value,
                                     final boolean ignoreRequest, final Map<Object, Object> defaults,
                                     final IconLoader iconLoader) {
        if ("null".equals(value)) {
            return null;
        }
        String key = propertyKey;
        boolean skipObjects = ignoreRequest;
        if (key.startsWith(REFERENCE_PREFIX)) {
            key = parseKey(key);
            skipObjects = true;
        }

        Object returnVal = new LoadError();
        if (key.endsWith("Insets") || key.endsWith(".insets")) {
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
            returnVal = parseIcon(value, iconLoader);
        } else if (key.endsWith("Size") || key.endsWith(".size")) {
            returnVal = parseSize(value);
        } else if ("null".equalsIgnoreCase(value)) {
            returnVal = null;
        } else if (value.startsWith("%")) {
            String val = value.substring(1);
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


    private static Object parseInsets(final String value) {
        final List<String> numbers = StringUtil.split(value, ",");
        return new InsetsUIResource(
                Integer.parseInt(numbers.get(0)),
                Integer.parseInt(numbers.get(1)),
                Integer.parseInt(numbers.get(2)),
                Integer.parseInt(numbers.get(3)));
    }

    private static Object parseFont(final String value) {
        try {
            final String[] decode = value.split("-");
            return new FontUIResource(decode[0], Integer.parseInt(decode[1]), Integer.parseInt(decode[2]));
        } catch (final Exception e) {
            return new FontUIResource("Dialog", Font.PLAIN, 12);
        }
    }

    private static Icon parseIcon(final String value, final IconLoader iconLoader) {
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
            } else if (path.endsWith(THEMED_KEY)) {
                tag = THEMED_KEY;
            }
            if (tag == null) {
                throw new IllegalArgumentException("Invalid tag on icon path: '" + value + "'");
            }
            String iconPath = path.substring(0, path.length() - tag.length());
            if (tag.equals(THEMED_KEY)) {
                return iconLoader.getIcon(iconPath, dim.width, dim.height, true);
            } else {
                DarkUIAwareIcon icon = iconLoader.getUIAwareIcon(iconPath, dim.width, dim.height);
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
        return iconLoader.getIcon(path, dim.width, dim.height);
    }


    private static DimensionUIResource parseSize(final String value) {
        int[] dim = Arrays.stream(value.split(",", 2)).mapToInt(Integer::parseInt).toArray();
        return new DimensionUIResource(dim[0], dim[1]);
    }


    private static Integer getInteger(final String value) {
        try {
            return Integer.parseInt(value);
        } catch (final NumberFormatException ignored) {
            return null;
        }
    }


    private static Object parseObject(final String value) {
        try {
            return Class.forName(value).getDeclaredConstructor().newInstance();
        } catch (final Exception ignored) { }
        return null;
    }

    private static final class LoadError {
    }

    private static final class ObjectRequest {

        final String value;
        final String key;


        private ObjectRequest(final String key, final String value) {
            this.key = key;
            this.value = value;
        }

        private void resolve(final Map<String, Object> cache) {
            UIDefaults defaults = UIManager.getLookAndFeelDefaults();
            if (cache.containsKey(value)) {
                defaults.put(key, cache.get(value));
            } else {
                Object obj = parseObject(value);
                if (obj == null) {
                    obj = parseValue(key, value, true, defaults, ICON_LOADER);
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


        @Override
        public String toString() {
            return "[" + key + "," + value + "]";
        }
    }
}
