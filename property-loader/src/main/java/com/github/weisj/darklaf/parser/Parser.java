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
package com.github.weisj.darklaf.parser;

import java.util.Arrays;
import java.util.List;


public final class Parser {

    public static final Object EMPTY_VALUE = new Object();

    private static final List<PropertyParser> steps = Arrays.asList(
            new NullParser(),
            new FallbackParser(),
            new ReferenceParser(),
            new PrimitiveParser(),
            new InsetParser(),
            new LazyObjectParser(),
            new ActiveObjectParser(),
            new FontParser(),
            new IconParser(),
            new DimensionParser(),
            new ListParser(),
            new MapParser());

    private static boolean debugMode;

    public static void setDebugMode(final boolean debugMode) {
        Parser.debugMode = debugMode;
    }

    public static boolean isDebugMode() {
        return debugMode;
    }

    public static ParseResult parse(final ParseResult parseResult, final ParserContext context) {
        ParseResult p = parseResult;
        String savedValue = parseResult.value;
        for (PropertyParser step : steps) {
            if (p.finished) return p;
            p = step.parse(p, context);
        }
        if (!p.finished) {
            for (String warning : p.warnings) {
                ParserUtil.warning(warning);
            }
            ParserUtil.setNonNull(p, savedValue);
        }
        return p;
    }

    public static ParseResult createParseResult(final String key, final String value) {
        if (isDebugMode()) {
            return new DebugParseResult(key, value);
        } else {
            return new ParseResult(key, value);
        }
    }

    public static class DebugParseResult extends ParseResult {

        public String originalKey;
        public String originalValue;
        public String referenceKey;

        public DebugParseResult(final String key, final String value) {
            super(key, value);
            this.originalKey = key;
            this.originalValue = value;
        }
    }
}
