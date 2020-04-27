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
package com.github.weisj.darklaf;

import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.text.AttributedCharacterIterator;
import java.util.*;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.swing.*;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.InsetsUIResource;

import com.github.weisj.darklaf.icons.DarkUIAwareIcon;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.icons.StateIcon;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.uiresource.DarkFontUIResource;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.Pair;
import com.github.weisj.darklaf.util.PropertyValue;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class PropertyLoader {
    private static final Logger LOGGER = Logger.getLogger(PropertyLoader.class.getName());
    private static final IconLoader ICON_LOADER = IconLoader.get(IconLoader.class);
    private static final char INT_LIST_START = '[';
    private static final char INT_LIST_END = ']';
    private static final String DUAL_KEY = "[dual]";
    private static final String AWARE_KEY = "[aware]";
    private static final String THEMED_KEY = "[themed]";
    private static final String ICON_EMPTY = "empty";

    private static final char REFERENCE_PREFIX = '%';

    private static final String FONT_FROM = "from";
    private static final String FONT_SIZE = "withSize";
    private static final String FONT_STYLE = "withStyle";
    private static final char FONT_DELIMITER = '-';

    private static final char LIST_START = '{';
    private static final char LIST_END = '}';
    private static final char ARG_START = '(';
    private static final char ARG_END = ')';
    private static final char SEPARATOR = ',';
    private static final char LIST_SEPARATOR = ';';
    private static final char PAIR_SEPARATOR = ':';

    private static boolean addReferenceInfo;

    private static final Map<AttributedCharacterIterator.Attribute, Integer> attributes = Collections.emptyMap();

    public static void setAddReferenceInfo(final boolean addReferenceInfo) {
        PropertyLoader.addReferenceInfo = addReferenceInfo;
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
        putProperties(properties, properties.stringPropertyNames(), accumulator, currentDefaults, iconLoader);
    }

    public static void putProperties(final Properties properties, final UIDefaults defaults) {
        putProperties(properties, defaults, ICON_LOADER);
    }

    public static void putProperties(final Properties properties, final UIDefaults defaults,
                                     final IconLoader iconLoader) {
        putProperties(properties, properties.stringPropertyNames(), defaults, defaults, iconLoader);
    }

    public static void putProperties(final Map<Object, Object> properties, final Set<String> keys,
                                     final Map<Object, Object> accumulator,
                                     final UIDefaults currentDefaults, final IconLoader iconLoader) {
        for (final String key : keys) {
            final String value = properties.get(key).toString();
            Object parsed = parseValue(key, value, accumulator, currentDefaults, iconLoader);
            if (parsed != null) {
                accumulator.put(parseKey(key), parsed);
            } else {
                currentDefaults.remove(parseKey(key));
            }
        }
    }

    public static void replaceProperties(final Map<Object, Object> properties,
                                         final Predicate<Map.Entry<Object, Object>> predicate,
                                         final Function<Map.Entry<Object, Object>, Object> mapper) {
        replacePropertiesOfType(Object.class, properties, predicate, mapper);
    }

    public static <T> void replacePropertiesOfType(final Class<T> type,
                                                   final Map<Object, Object> properties,
                                                   final Function<T, T> mapper) {
        replacePropertiesOfType(type, properties, e -> true, e -> mapper.apply(e.getValue()));
    }

    @SuppressWarnings("unchecked")
    public static <T> void replacePropertiesOfType(final Class<T> type,
                                                   final Map<Object, Object> properties,
                                                   final Predicate<Map.Entry<Object, T>> predicate,
                                                   final Function<Map.Entry<Object, T>, T> mapper) {
        for (Map.Entry<Object, Object> entry : properties.entrySet()) {
            if (type == Object.class || type.isInstance(entry.getValue())) {
                if (predicate.test((Map.Entry<Object, T>) entry)) {
                    T newValue = mapper.apply((Map.Entry<Object, T>) entry);
                    if (newValue != null) {
                        entry.setValue(newValue);
                    }
                }
            }
        }
    }

    private static String parseKey(final String key) {
        if (addReferenceInfo) return key;
        return key.startsWith(String.valueOf(REFERENCE_PREFIX)) ? key.substring(1) : key;
    }

    public static Object parseValue(final String propertyKey, final String value,
                                    final Map<Object, Object> accumulator,
                                    final UIDefaults currentDefaults, final IconLoader iconLoader) {
        if (value == null || PropertyValue.NULL.equals(value)) {
            return null;
        }
        String key = propertyKey;
        boolean skipObjects = false;
        if (key.startsWith(String.valueOf(REFERENCE_PREFIX))) {
            key = parseKey(key);
            skipObjects = true;
        }

        Object returnVal = new LoadError();
        if (key.endsWith("Insets") || key.endsWith(".insets")) {
            returnVal = parseInsets(value, accumulator, currentDefaults, iconLoader);
        } else if (!skipObjects
                   && (key.endsWith("Border")
                       || key.endsWith(".border")
                       || key.endsWith("Renderer"))) {
            return (UIDefaults.LazyValue) def -> parseObject(value);
        } else if (key.endsWith(".component") || key.endsWith("Component")) {
            return (UIDefaults.ActiveValue) (def) -> parseObject(value);
        } else if (key.toLowerCase().endsWith("font")) {
            returnVal = parseFont(key, value, accumulator, currentDefaults);
        } else if (key.endsWith(".icon") || key.endsWith("Icon") || key.endsWith("Image")) {
            returnVal = parseIcon(value, accumulator, currentDefaults, iconLoader);
        } else if (key.endsWith("Size") || key.endsWith(".size")) {
            returnVal = parseSize(value);
        } else if (value.startsWith(String.valueOf(LIST_START))
                   && value.endsWith(String.valueOf(LIST_END))) {
            returnVal = parseList((v, acc, defs, iconL) -> PropertyLoader.parseValue("", v, acc, defs, iconL),
                                  value, accumulator, currentDefaults, iconLoader);
        } else if (value.startsWith(String.valueOf(INT_LIST_START))
                   && value.endsWith(String.valueOf(INT_LIST_END))) {
            returnVal = parseList((SimpleValueMapper<Integer>) Integer::parseInt, value, accumulator, currentDefaults,
                                  iconLoader, INT_LIST_START, INT_LIST_END, SEPARATOR);
        } else if (value.contains(String.valueOf(PAIR_SEPARATOR))) {
            returnVal = parsePair((v, acc, defs, iconL) -> PropertyLoader.parseValue("", v, acc, defs, iconL),
                                  value, accumulator, currentDefaults, iconLoader);
        } else if (PropertyValue.NULL.equalsIgnoreCase(value)) {
            returnVal = null;
        } else if (value.startsWith(String.valueOf(REFERENCE_PREFIX))) {
            returnVal = parseReference(key, value, accumulator, currentDefaults);
        }
        if (returnVal instanceof LoadError) {
            final Color color = ColorUtil.fromHex(value, null);
            final Integer invVal = getInteger(value);
            final Boolean boolVal = PropertyValue.TRUE.equalsIgnoreCase(value)
                    ? Boolean.TRUE
                    : PropertyValue.FALSE.equalsIgnoreCase(value) ? Boolean.FALSE : null;
            if (color != null && (value.length() == 6 || value.length() == 8)) {
                return new DarkColorUIResource(color);
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

    private static <T> Pair<T, T> parsePair(final ParseFunction<T> mapper,
                                            final String value, final Map<Object, Object> accumulator,
                                            final UIDefaults currentDefaults, final IconLoader iconLoader) {
        return parsePair(mapper, mapper, value, accumulator, currentDefaults, iconLoader);
    }

    private static <T, K> Pair<T, K> parsePair(final ParseFunction<T> firstMapper, final ParseFunction<K> secondMapper,
                                               final String value, final Map<Object, Object> accumulator,
                                               final UIDefaults currentDefaults, final IconLoader iconLoader) {
        String[] pairVals = value.split(String.valueOf(PAIR_SEPARATOR), 2);
        return new Pair<>(firstMapper.parseValue(pairVals[0], accumulator, currentDefaults, iconLoader),
                          secondMapper.parseValue(pairVals[1], accumulator, currentDefaults, iconLoader));
    }

    private static Object parseReference(final String key, final String value,
                                         final Map<Object, Object> accumulator,
                                         final UIDefaults currentDefault) {
        String val = parseKey(value);
        String referenceFreeKey = val.substring(1);
        boolean accumulatorContainsKey = accumulator.containsKey(val)
                                         || (addReferenceInfo && accumulator.containsKey(referenceFreeKey));
        boolean defaultsContainKey = currentDefault.containsKey(val)
                                     || (addReferenceInfo && currentDefault.containsKey(referenceFreeKey));
        if (!defaultsContainKey && !accumulatorContainsKey) {
            LOGGER.warning("Could not reference value '" + val + "' while loading '" + key + "'. " +
                           "Maybe is a forward reference");
        }
        Object returnVal = accumulatorContainsKey ? accumulator.get(val)
                : currentDefault.get(val);
        if (addReferenceInfo) {
            if (returnVal == null) {
                returnVal = accumulatorContainsKey ? accumulator.get(referenceFreeKey)
                        : currentDefault.get(val);
            }
            returnVal = new Pair<>(value, returnVal);
        }
        return returnVal;
    }

    private static Object parseInsets(final String value,
                                      final Map<Object, Object> accumulator,
                                      final UIDefaults currentDefaults, final IconLoader iconLoader) {
        List<Integer> insets = parseList((SimpleValueMapper<Integer>) Integer::parseInt, value, accumulator,
                                         currentDefaults, iconLoader, Character.MIN_VALUE, Character.MIN_VALUE,
                                         SEPARATOR);
        return new InsetsUIResource(insets.get(0), insets.get(1), insets.get(2), insets.get(3));
    }

    @SuppressWarnings("MagicConstant")
    private static Object parseFont(final String key, final String value,
                                    final Map<Object, Object> accumulator,
                                    final UIDefaults currentDefaults) {
        String val = value;
        Font base = null;
        int size = -1;
        int style = -1;
        while (true) {
            if (val.startsWith(FONT_FROM)) {
                Pair<Font, String> result = parseFrom(val, accumulator, currentDefaults);
                base = result.getFirst();
                val = result.getSecond();
            } else if (val.startsWith(FONT_SIZE)) {
                Pair<Integer, String> result = parseFontAttribute(FONT_SIZE, val, accumulator, currentDefaults);
                size = result.getFirst();
                val = result.getSecond();
            } else if (val.startsWith(FONT_STYLE)) {
                Pair<Integer, String> result = parseFontAttribute(FONT_STYLE, val, accumulator, currentDefaults);
                style = result.getFirst();
                val = result.getSecond();
            } else {
                break;
            }
        }
        if (base == null) base = parseExplicitFont(value);
        if (base == null && accumulator.get(key) instanceof Font) base = (Font) accumulator.get(key);
        if (base == null) base = currentDefaults.getFont(key);
        if (base == null) base = new Font("Dialog", Font.PLAIN, 12);
        if (size > 0) base = base.deriveFont((float) size);
        if (style >= 0) base = base.deriveFont(style);
        return new DarkFontUIResource(base.deriveFont(attributes));
    }

    private static Font parseExplicitFont(final String value) {
        try {
            final String[] decode = value.split(String.valueOf(FONT_DELIMITER));
            return new DarkFontUIResource(decode[0], Integer.parseInt(decode[1]), Integer.parseInt(decode[2]));
        } catch (final Exception e) {
            return null;
        }
    }

    private static Pair<Integer, String> parseFontAttribute(final String identifier, final String val,
                                                            final Map<Object, Object> accumulator,
                                                            final UIDefaults currrentDefault) {
        String key = val.substring(identifier.length() + 1);
        int lastIndex = key.indexOf(ARG_END);
        String rest = key.substring(lastIndex + 1);
        key = key.substring(0, lastIndex);
        String[] subKeys = key.split(String.valueOf(SEPARATOR));
        int[] values = new int[subKeys.length];
        for (int i = 0; i < values.length; i++) {
            if (subKeys[i].startsWith(String.valueOf(REFERENCE_PREFIX))) {
                Object ref = parseReference(identifier, subKeys[i], accumulator, currrentDefault);
                values[i] = ref instanceof Integer ? (Integer) ref : 0;
            } else {
                try {
                    values[i] = Integer.parseInt(subKeys[i]);
                } catch (NumberFormatException ignored) {
                    // In this case the value will be 0.
                }
            }
        }
        int result = 0;
        for (int i : values) {
            result += i;
        }
        return new Pair<>(result, rest);
    }

    private static Pair<Font, String> parseFrom(final String val,
                                                final Map<Object, Object> accumulator,
                                                final UIDefaults currentDefaults) {
        String key = val.substring(FONT_FROM.length() + 1);
        int index = key.indexOf(ARG_END);
        String rest = key.substring(index + 1);
        key = key.substring(0, index);
        Font font = null;
        if (accumulator.get(key) instanceof Font) font = (Font) accumulator.get(key);
        if (font == null) font = currentDefaults.getFont(key);
        return new Pair<>(font, rest);
    }

    private static <T> List<T> parseList(final ParseFunction<T> mapper,
                                         final String value,
                                         final Map<Object, Object> accumulator,
                                         final UIDefaults currentDefaults, final IconLoader iconLoader) {
        return parseList(mapper, value, accumulator, currentDefaults, iconLoader, LIST_START, LIST_END,
                         LIST_SEPARATOR);
    }

    private static <T> List<T> parseList(final ParseFunction<T> mapper,
                                         final String value,
                                         final Map<Object, Object> accumulator,
                                         final UIDefaults currentDefaults, final IconLoader iconLoader,
                                         final char start, final char end,
                                         final char delimiter) {
        if (value == null || value.isEmpty()) return new ArrayList<>();
        String val = value;
        if (val.charAt(0) == start) {
            val = value.substring(1, value.length() - 1);
        }
        String[] values = val.split(String.valueOf(delimiter));
        return Arrays.stream(values)
                     .map(k -> mapper.parseValue(k, accumulator, currentDefaults, iconLoader))
                     .collect(Collectors.toList());
    }

    private static Icon parseIcon(final String value,
                                  final Map<Object, Object> accumulator,
                                  final UIDefaults currentDefaults,
                                  final IconLoader iconLoader) {
        if (value.startsWith(String.valueOf(LIST_START))) {
            return parseStateIcon(value, accumulator, currentDefaults, iconLoader);
        }
        String path = value;
        Dimension dim = new Dimension(16, 16);
        if (value.charAt(value.length() - 1) == ARG_END) {
            int i = path.lastIndexOf(ARG_START);
            String dimVal = path.substring(i + 1, path.length() - 1);
            int[] values = Arrays.stream(dimVal.split(String.valueOf(SEPARATOR), 2)).mapToInt(Integer::parseInt)
                                 .toArray();
            dim.width = values[0];
            dim.height = values[1];
            path = path.substring(0, i);
        }
        if (path.charAt(path.length() - 1) == INT_LIST_END) {
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
        if (path.equals(ICON_EMPTY)) {
            return EmptyIcon.create(dim.width, dim.height);
        }
        return iconLoader.getIcon(path, dim.width, dim.height);
    }

    private static Icon parseStateIcon(final String value,
                                       final Map<Object, Object> accumulator,
                                       final UIDefaults currentDefaults,
                                       final IconLoader iconLoader) {
        return new StateIcon(parseList(PropertyLoader::parseIcon, value, accumulator, currentDefaults, iconLoader));
    }

    private static Object parseSize(final String value) {
        try {
            int[] dim = Arrays.stream(value.split(String.valueOf(SEPARATOR), 2)).mapToInt(Integer::parseInt).toArray();
            return new DimensionUIResource(dim[0], dim[1]);
        } catch (IndexOutOfBoundsException | NumberFormatException e) {
            return new LoadError();
        }
    }

    private static Integer getInteger(final String value) {
        try {
            return Integer.parseInt(value);
        } catch (final NumberFormatException ignored) {
            return null;
        }
    }

    private static Double getDouble(final String value) {
        try {
            return Double.parseDouble(value);
        } catch (final NumberFormatException ignored) {
            return null;
        }
    }

    private static Object parseObject(final String value) {
        try {
            return Class.forName(value).getDeclaredConstructor().newInstance();
        } catch (final Exception ignored) {}
        return null;
    }

    private static final class LoadError {}

    private interface ParseFunction<T> {

        T parseValue(final String value,
                     final Map<Object, Object> accumulator,
                     final UIDefaults currentDefaults, final IconLoader iconLoader);
    }

    private interface SimpleValueMapper<T> extends ParseFunction<T> {

        T map(final String value);

        @Override
        default T parseValue(final String value,
                             final Map<Object, Object> accumulator,
                             final UIDefaults currentDefaults, final IconLoader iconLoader) {
            return map(value);
        }
    }
}
