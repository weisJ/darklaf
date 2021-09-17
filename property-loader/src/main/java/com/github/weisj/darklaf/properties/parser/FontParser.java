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

import java.awt.Font;
import java.text.AttributedCharacterIterator;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import com.github.weisj.darklaf.properties.uiresource.DarkFontUIResource;
import com.github.weisj.darklaf.util.FontUtil;

public class FontParser extends KeyFilteredParser implements Delimiters {

    private static final Map<AttributedCharacterIterator.Attribute, Integer> attributes = Collections.emptyMap();

    private static final String BASE_FONT = "font";
    private static final String FONT_FROM = "from";
    private static final String FONT_SIZE = "withSize";
    private static final String FONT_STYLE = "withStyle";
    private static final char FONT_DELIMITER = '-';

    public FontParser() {
        super("font");
    }

    @Override
    public ParseResult doParse(final ParseResult parseResult, final ParserContext context) {
        Font base = null;
        int size = -1;
        int style = -1;
        while (!parseResult.value.isEmpty()) {
            int length = parseResult.value.length();
            if (ParserUtil.stripPrefixFromValue(parseResult, BASE_FONT)) {
                if (base != null) ParserUtil.warning("Duplicate Base font declared: '" + parseResult.value + "'");
                base = ParserUtil
                        .parseBetween(ARG_START, ARG_END, this::parseBaseFont, Font.class, parseResult, context)
                        .orElse(base);
            } else if (ParserUtil.stripPrefixFromValue(parseResult, FONT_FROM)) {
                if (base != null) ParserUtil.warning("Duplicate Base font declared: '" + parseResult.value + "'");
                base = ParserUtil.parseBetween(ARG_START, ARG_END, Font.class, parseResult, context)
                        .orElse(base);
            } else if (ParserUtil.stripPrefixFromValue(parseResult, FONT_STYLE)) {
                if (style >= 0) ParserUtil.warning("Duplicate font style declared: '" + parseResult.value + "'");
                style = ParserUtil.parseBetween(ARG_START, ARG_END, Integer.class, parseResult, context)
                        .orElse(style);
            } else if (ParserUtil.stripPrefixFromValue(parseResult, FONT_SIZE)) {
                if (size >= 0) ParserUtil.warning("Duplicate font size declared: '" + parseResult.value + "'");
                size = ParserUtil.parseDelimited(ARG_START, ARG_END, ARG_SEPARATOR, Integer.class, parseResult, context)
                        .stream().reduce(Integer::sum).orElse(size);
            }
            if (parseResult.value.length() == length) {
                // Did not make any progress. Bail
                return ParserUtil.error(parseResult, "Unexpected token while parsing font");
            }
        }
        if (base == null) base = FontUtil.createFont(null, Font.PLAIN, 12);
        if (size <= 0) size = base.getSize();
        if (style < 0) style = base.getStyle();

        // noinspection MagicConstant
        Font font = base.deriveFont(style, size);
        return ParserUtil.setNonNull(parseResult, new DarkFontUIResource(font.deriveFont(attributes)));
    }

    private ParseResult parseBaseFont(final ParseResult parseResult, final ParserContext context) {
        List<Object> parts = ParserUtil.parseDelimited(FONT_DELIMITER, Object.class,
                Parser.createParseResult("", parseResult.value), context);
        if (parts.size() != 3) return fontStructureError(parseResult);
        if (!(parts.get(0) instanceof String)) return fontStructureError(parseResult);
        if (!(parts.get(1) instanceof Integer)) return fontStructureError(parseResult);
        if (!(parts.get(2) instanceof Integer)) return fontStructureError(parseResult);
        return ParserUtil.setNonNull(parseResult,
                FontUtil.createFont((String) parts.get(0), (Integer) parts.get(1), (Integer) parts.get(2)));
    }

    private ParseResult fontStructureError(final ParseResult parseResult) {
        return ParserUtil.error(parseResult,
                "Expected structure FontName-FontStyle-FontStyle but got '" + parseResult.value + "'");
    }
}
