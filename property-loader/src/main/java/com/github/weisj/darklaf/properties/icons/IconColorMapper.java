/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
 */
package com.github.weisj.darklaf.properties.icons;

import java.awt.*;
import java.util.*;
import java.util.logging.Logger;

import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.properties.parser.ParseResult;
import com.github.weisj.darklaf.properties.parser.Parser;
import com.github.weisj.darklaf.properties.parser.ParserContext;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Pair;
import com.github.weisj.darklaf.util.Types;

/**
 * Utility class responsible for patching color definitions in svg icons.
 *
 * @author Jannis Weis
 */
public final class IconColorMapper {
    private static final Logger LOGGER = LogUtil.getLogger(IconLoader.class);
    private static final String INLINE_VALUE_PREFIX = "%";
    private static final Color FALLBACK_COLOR = Color.RED;

    public static Color resolveColor(final String key, final String[] fallbacks,
            final Map<Object, Object> propertyMap, final Map<Object, Object> contextDefaults) {
        Color color = get(propertyMap, contextDefaults, key, fallbacks, Color.class);

        if (color == null) {
            color = FALLBACK_COLOR;
            LOGGER.warning("Could not load color with id '" + key + "' fallbacks" + Arrays.toString(fallbacks)
                    + ". Using color '" + color + "' instead.");
        }
        return color;
    }

    public static float getOpacity(final String key, final String[] fallbacks, final Map<Object, Object> propertyMap,
            final Map<Object, Object> contextDefaults) {
        if ((key == null || key.isEmpty()) && (fallbacks == null || fallbacks.length == 0)) return -1;
        // UIManager defaults to 0, if the value isn't an integer (or null).
        Number obj = get(propertyMap, contextDefaults, key, fallbacks, Number.class);
        if (obj instanceof Integer) {
            return obj.intValue() / 100.0f;
        } else if (obj instanceof Long) {
            return obj.intValue() / 100.0f;
        } else if (obj instanceof Float) {
            return obj.floatValue();
        } else if (obj instanceof Double) {
            return obj.floatValue();
        }
        LOGGER.warning(obj + " is an invalid opacity value. Key = '" + key + "'");
        // In this case we default to -1.
        return -1;
    }

    public static <T> Pair<Object, T> getEntry(final Map<Object, Object> map, final Map<Object, Object> contextDefaults,
            final Object key, final Object[] fallbacks, final Class<T> type) {
        Object obj = null;
        String refPrefix = PropertyLoader.getReferencePrefix();
        Set<Object> seen = new HashSet<>();
        Object currentKey = key;
        int max = fallbacks != null ? fallbacks.length : 0;
        outer: for (int i = -1; i < max; i++) {
            currentKey = i < 0 ? key : fallbacks[i];
            int retryCount = 5;
            if (i >= 0 && currentKey instanceof String && ((String) currentKey).startsWith(INLINE_VALUE_PREFIX)) {
                ParseResult p = Parser.parse(
                        Parser.createParseResult(Objects.toString(key),
                                ((String) currentKey).substring(INLINE_VALUE_PREFIX.length())),
                        new ParserContext(map, contextDefaults, IconLoader.get()));
                obj = Types.safeCast(p.result, type);
            }
            do {
                if (obj == null) {
                    obj = map.get(currentKey);
                }
                if (contextDefaults != null && (obj == null || seen.contains(obj))) {
                    obj = contextDefaults.get(currentKey);
                }
                seen.add(obj);
                if (obj instanceof String && obj.toString().startsWith(refPrefix)) {
                    currentKey = obj.toString().substring(refPrefix.length());
                    obj = null;
                } else {
                    if (type.isInstance(obj)) {
                        // We found the value
                        break outer;
                    } else {
                        // Any further searches won't find anything.
                        // The value doesn't explicitly reference other keys.
                        continue outer;
                    }
                }
                retryCount--;
            } while (retryCount > 0);
        }
        return new Pair<>(currentKey, type.cast(obj));
    }

    public static <T> T get(final Map<Object, Object> map, final Map<Object, Object> contextDefaults, final Object key,
            final Object[] fallbacks, final Class<T> type) {
        return getEntry(map, contextDefaults, key, fallbacks, type).getSecond();
    }
}
