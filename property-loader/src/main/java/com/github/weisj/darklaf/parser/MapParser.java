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

import java.util.List;
import java.util.stream.Collectors;

import com.github.weisj.darklaf.util.Pair;

public class MapParser implements PropertyParser, Delimiters {

    private final PropertyParser pairParser = (parsable, context) -> {
        if (parsable.value.contains(String.valueOf(PAIR_SEPARATOR))) {
            List<Object> components = ParserUtil.parseDelimited(PAIR_SEPARATOR, Object.class, parsable, context);
            if (components.size() != 2) return ParserUtil.error(parsable, "Expected 2 components");
            return ParserUtil.setNonNull(parsable, new Pair<>(components.get(0), components.get(1)));
        } else {
            return ParserUtil.setNonNull(parsable,
                    new Pair<>(Parser.parse(parsable, context).result, Parser.EMPTY_VALUE));
        }
    };

    @Override
    public ParseResult doParse(final ParseResult parseResult, final ParserContext context) {
        if (parseResult.value.startsWith(String.valueOf(MAP_START))
                && parseResult.value.endsWith(String.valueOf(MAP_END))) {
            return ParserUtil.setNonNull(parseResult,
                    ParserUtil.parseDelimited(MAP_START, MAP_END, MAP_SEPARATOR, pairParser,
                            Pair.class, parseResult, context).stream()
                            .collect(Collectors.toMap(Pair::getFirst, Pair::getSecond)));
        }
        return parseResult;
    }

}
