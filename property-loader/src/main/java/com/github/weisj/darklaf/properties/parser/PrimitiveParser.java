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
package com.github.weisj.darklaf.properties.parser;

import java.awt.Color;

import com.github.weisj.darklaf.properties.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.PropertyValue;

public class PrimitiveParser implements PropertyParser {

    public static final String COLOR_PREFIX = "#";
    public static final char STRING_DELIMITER = '\'';

    @Override
    public ParseResult doParse(final ParseResult parseResult, final ParserContext context) {
        if (parseResult.value.startsWith(COLOR_PREFIX)) {
            Color c = ColorUtil.fromHex(parseResult.value, null, true);
            ParserUtil.setNonNull(parseResult, c != null ? new DarkColorUIResource(c) : null);
        }
        if (parseResult.finished) return parseResult;
        if (ParserUtil.startsWith(parseResult, STRING_DELIMITER)) {
            ParserUtil.setNonNull(parseResult,
                    ParserUtil.parseBetween(STRING_DELIMITER, STRING_DELIMITER,
                            PropertyParser.of(String::valueOf), String.class,
                            parseResult, context).orElse(null));
        }
        if (parseResult.finished) return parseResult;
        ParserUtil.setNonNull(parseResult, getInteger(parseResult.value));
        if (parseResult.finished) return parseResult;
        ParserUtil.setNonNull(parseResult, getBoolean(parseResult.value));
        return parseResult;
    }

    private Boolean getBoolean(final String value) {
        return PropertyValue.TRUE.equalsIgnoreCase(value)
                ? Boolean.TRUE
                : PropertyValue.FALSE.equalsIgnoreCase(value)
                        ? Boolean.FALSE
                        : null;
    }

    private static Integer getInteger(final String value) {
        try {
            return Integer.parseInt(value);
        } catch (final NumberFormatException ignored) {
            return null;
        }
    }
}
