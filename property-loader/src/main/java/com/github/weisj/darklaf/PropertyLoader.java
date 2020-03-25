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
import com.github.weisj.darklaf.util.Pair;
import com.github.weisj.darklaf.util.PropertyValue;
import com.github.weisj.darklaf.util.StringUtil;

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.DimensionUIResource;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.InsetsUIResource;
import java.awt.*;
import java.awt.font.TextAttribute;
import java.io.IOException;
import java.io.InputStream;
import java.text.AttributedCharacterIterator;
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
    private static final String FONT_FROM = "from";
    private static final String FONT_SIZE = "withSize";
    private static final String FONT_STYLE = "withStyle";

    private static boolean addReferenceInfo;

    private static final Map<AttributedCharacterIterator.Attribute, Integer> attributes = Collections.singletonMap(
        TextAttribute.KERNING,
        TextAttribute.KERNING_ON);

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
        for (final String key : properties.stringPropertyNames()) {
            final String value = properties.getProperty(key);
            Object parsed = parseValue(key, value, accumulator, currentDefaults, iconLoader);
            if (parsed != null) {
                accumulator.put(parseKey(key), parsed);
            } else {
                currentDefaults.remove(parseKey(key));
            }
        }
    }

    private static Object parseValue(final String key, final String value, final Properties accumulator,
                                     final UIDefaults currentDefaults, final IconLoader iconLoader) {
        return parseValue(key, value, false, accumulator, currentDefaults, iconLoader);
    }

    private static String parseKey(final String key) {
        if (addReferenceInfo) return key;
        return key.startsWith(REFERENCE_PREFIX) ? key.substring(REFERENCE_PREFIX.length()) : key;
    }


    private static Object parseValue(final String propertyKey, final String value,
                                     final boolean ignoreRequest, final Map<Object, Object> accumulator,
                                     final UIDefaults currentDefaults, final IconLoader iconLoader) {
        if (PropertyValue.NULL.equals(value)) {
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
                       || key.endsWith("Renderer"))) {
            return (UIDefaults.LazyValue) def -> parseObject(value);
        } else if (key.endsWith(".component") || key.endsWith("Component")) {
            return (UIDefaults.ActiveValue) (def) -> parseObject(value);
        } else if (key.toLowerCase().endsWith("font")) {
            returnVal = parseFont(key, value, accumulator, currentDefaults);
        } else if (key.endsWith(".icon") || key.endsWith("Icon")) {
            returnVal = parseIcon(value, iconLoader);
        } else if (key.endsWith("Size") || key.endsWith(".size")) {
            returnVal = parseSize(value);
        } else if (PropertyValue.NULL.equalsIgnoreCase(value)) {
            returnVal = null;
        } else if (value.startsWith(REFERENCE_PREFIX)) {
            String val = parseKey(value);
            String referenceFreeKey = val.substring(REFERENCE_PREFIX.length());
            boolean containsKey = accumulator.containsKey(val)
                                  || (addReferenceInfo && accumulator.containsKey(referenceFreeKey));
            if (!containsKey) {
                LOGGER.warning("Could not reference value '" + val + "' while loading '" + key + "'. " +
                               "May be a forward reference");
            }
            returnVal = accumulator.get(val);
            if (addReferenceInfo) {
                if (returnVal == null) returnVal = accumulator.get(referenceFreeKey);
                returnVal = new Pair<>(value, returnVal);
            }
        }
        if (returnVal instanceof LoadError) {
            final Color color = ColorUtil.fromHex(value, null);
            final Integer invVal = getInteger(value);
            final Boolean boolVal = PropertyValue.TRUE.equalsIgnoreCase(value)
                                    ? Boolean.TRUE
                                    : PropertyValue.FALSE.equalsIgnoreCase(value) ? Boolean.FALSE : null;
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

    @SuppressWarnings("MagicConstant")
    private static Object parseFont(final String key, final String value, final Map<Object, Object> accumulator,
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
                Pair<Integer, String> result = parseFontAttribute(FONT_SIZE, val);
                size = result.getFirst();
                val = result.getSecond();
            } else if (val.startsWith(FONT_STYLE)) {
                Pair<Integer, String> result = parseFontAttribute(FONT_STYLE, val);
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
        return new FontUIResource(base.deriveFont(attributes));
    }

    private static Font parseExplicitFont(final String value) {
        try {
            final String[] decode = value.split("-");
            return new FontUIResource(decode[0], Integer.parseInt(decode[1]), Integer.parseInt(decode[2]));
        } catch (final Exception e) {
            return null;
        }
    }

    private static Pair<Integer, String> parseFontAttribute(final String identifier, final String val) {
        String key = val.substring(identifier.length() + 1);
        int lastIndex = key.indexOf(')');
        String rest = key.substring(lastIndex + 1);
        key = key.substring(0, lastIndex);
        try {
            return new Pair<>(Integer.parseInt(key), rest);
        } catch (NumberFormatException e) {
            return new Pair<>(-1, rest);
        }
    }

    private static Pair<Font, String> parseFrom(final String val,
                                                final Map<Object, Object> accumulator,
                                                final UIDefaults currentDefaults) {
        String key = val.substring(FONT_FROM.length() + 1);
        int index = key.indexOf(')');
        String rest = key.substring(index + 1);
        key = key.substring(0, index);
        Font font = null;
        if (accumulator.get(key) instanceof Font) font = (Font) accumulator.get(key);
        if (font == null) font = currentDefaults.getFont(key);
        return new Pair<>(font, rest);
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


    private static Object parseSize(final String value) {
        try {
            int[] dim = Arrays.stream(value.split(",", 2)).mapToInt(Integer::parseInt).toArray();
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


    private static Object parseObject(final String value) {
        try {
            return Class.forName(value).getDeclaredConstructor().newInstance();
        } catch (final Exception ignored) {
        }
        return null;
    }

    private static final class LoadError {
    }

}
