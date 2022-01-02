/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.properties.parser;

import com.github.weisj.darklaf.util.Lambdas;

@FunctionalInterface
public interface PropertyParser {
    ParseResult doParse(final ParseResult parseResult, final ParserContext context);

    default ParseResult parse(final ParseResult parseResult, final ParserContext context) {
        if (parseResult.finished) return parseResult;
        if (!filter(parseResult, context)) return parseResult;
        parseResult.save();
        ParseResult p = doParse(parseResult, context);
        parseResult.save();
        return p;
    }

    default boolean filter(final ParseResult parseResult, final ParserContext context) {
        return true;
    }

    default PropertyParser andThen(final PropertyParser parser) {
        return new JoinedParser(this, parser);
    }

    static <E extends Exception> PropertyParser of(final Lambdas.CheckedFunction<String, Object, E> function) {
        return (p, c) -> {
            try {
                return ParserUtil.setNonNull(p, function.apply(p.value));
            } catch (final Exception e) {
                p.result = null;
                p.finished = true;
            }
            return p;
        };
    }
}
