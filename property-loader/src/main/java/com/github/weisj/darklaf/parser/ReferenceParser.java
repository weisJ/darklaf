/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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

public class ReferenceParser implements PropertyParser {

    private static final String REFERENCE_PREFIX = "%";

    @Override
    public ParseResult doParse(final ParseResult parseResult, final ParserContext context) {
        ParserUtil.stripPrefixFromKey(parseResult, REFERENCE_PREFIX);
        parseResult.save();
        if (ParserUtil.stripPrefixFromValue(parseResult, REFERENCE_PREFIX)) {
            ParserUtil.replaceIfNull(parseResult, parseResult.value, context.accumulator);
            ParserUtil.replaceIfNull(parseResult, parseResult.value, context.defaults);
            if (!parseResult.finished) {
                parseResult.warnings.add("Could not reference value '"
                        + parseResult.value + "' while loading '" + parseResult.key + "'. "
                        + "Maybe it's a forward reference.");
                parseResult.restore();
            } else if (parseResult instanceof Parser.DebugParseResult) {
                ((Parser.DebugParseResult) parseResult).referenceKey = REFERENCE_PREFIX + parseResult.value;
            }
        }
        return parseResult;
    }
}
