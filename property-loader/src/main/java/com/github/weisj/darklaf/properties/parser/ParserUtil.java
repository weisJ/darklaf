/*
 * MIT License
 *
 * Copyright (c) 2021-2025 Jannis Weis
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Types;

final class ParserUtil implements Delimiters {

    private static final char EMPTY_CHAR = Character.MIN_VALUE;
    private static final Logger LOGGER = LogUtil.getLogger(PropertyParser.class);

    static ParseResult error(final ParseResult parseResult, final String message) {
        LOGGER.severe("Error while parsing " + parseResult + ". " + message);
        parseResult.finished = true;
        parseResult.result = null;
        return parseResult;
    }

    static void warning(final String message) {
        LOGGER.log(Level.WARNING, message, new RuntimeException(message));
    }

    static ParseResult apply(final PropertyParser parser, final ParseResult p, final ParserContext c) {
        return parser != null ? parser.parse(p, c) : p;
    }

    static boolean startsWith(final ParseResult parseResult, final String prefix) {
        return parseResult.value.startsWith(prefix);
    }

    static boolean startsWith(final ParseResult parseResult, final char prefix) {
        return !parseResult.value.isEmpty() && parseResult.value.charAt(0) == prefix;
    }

    static void stripPrefixFromKey(final ParseResult parseResult, final String prefix) {
        if (parseResult.key.startsWith(prefix)) {
            parseResult.key = parseResult.key.substring(prefix.length());
        }
    }

    static boolean stripPrefixFromValue(final ParseResult parseResult, final String prefix) {
        if (startsWith(parseResult, prefix)) {
            parseResult.value = parseResult.value.substring(prefix.length());
            return true;
        }
        return false;
    }

    static void replaceIfNull(final ParseResult parseResult, final String key, final Map<Object, Object> map) {
        if (parseResult.finished) return;
        Object obj = map.get(key);
        while (obj instanceof ParseResult) {
            obj = ((ParseResult) obj).result;
        }
        setNonNull(parseResult, obj);
    }

    static ParseResult setNonNull(final ParseResult parseResult, final Object result) {
        if (result != null) {
            parseResult.result = result;
            parseResult.finished = true;
        }
        return parseResult;
    }

    static <T> Optional<T> parseBetween(final char start, final char end, final Class<T> type,
            final ParseResult parseResult,
            final ParserContext context) {
        return parseBetween(start, end, Parser::parse, type, parseResult, context);
    }

    static <T> Optional<T> parseBetween(final char start, final char end, final PropertyParser parser,
            final Class<T> type, final ParseResult parseResult,
            final ParserContext context) {
        List<T> parsed = parseDelimited(start, end, EMPTY_CHAR, parser, type, parseResult, context);
        return parsed.stream().findFirst();
    }


    static <T> List<T> parseDelimited(final char delimiter, final PropertyParser parser, final Class<T> type,
            final ParseResult parseResult, final ParserContext context) {
        return parseDelimited(EMPTY_CHAR, EMPTY_CHAR, delimiter, parser, type, parseResult, context);
    }

    static <T> List<T> parseDelimited(final char delimiter, final Class<T> type,
            final ParseResult parseResult, final ParserContext context) {
        return parseDelimited(delimiter, Parser::parse, type, parseResult, context);
    }

    static <T> List<T> parseDelimited(final char start, final char end, final char delimiter, final Class<T> type,
            final ParseResult parseResult, final ParserContext context) {
        return parseDelimited(start, end, delimiter, Parser::parse, type, parseResult, context);
    }

    static <T> List<T> parseDelimited(final char start, final char end, final char delimiter,
            final PropertyParser parser, final Class<T> type, final ParseResult parseResult,
            final ParserContext context) {
        return parseDelimited(start, end, delimiter, true, parser, type, parseResult, context);
    }

    /*
     * Parses the value in a delimited fashion. The consumed part of the value will be replaced in-place
     * in the Parsable object
     */
    static <T> List<T> parseDelimited(final char start, final char end, final char delimiter, final boolean forward,
            final PropertyParser parser, final Class<T> type, final ParseResult parseResult,
            final ParserContext context) {
        if (forward) {
            if (start != EMPTY_CHAR) {
                if (parseResult.value.charAt(0) == start) {
                    parseResult.value = parseResult.value.substring(1);
                } else {
                    LOGGER.warning("Expected '" + start + "' while parsing " + parseResult.value);
                }
            }
        } else {
            if (end != EMPTY_CHAR) {
                if (parseResult.value.charAt(parseResult.value.length() - 1) == end) {
                    parseResult.value = parseResult.value.substring(0, parseResult.value.length() - 1);
                } else {
                    LOGGER.warning("Expected '" + start + "' while parsing " + parseResult.value);
                }
            }
        }
        List<String> values = delimitedSplit(delimiter, forward ? end : start, parseResult, forward);
        if (values.isEmpty()) return Collections.emptyList();
        List<T> parsed = values.stream()
                .map(v -> Parser.createParseResult(parseResult.key, v))
                .map(p -> parser.parse(p, context))
                .map(p -> {
                    T casted = Types.safeCast(p.result, type);
                    if (casted == null) LOGGER.warning("Value " + p.result + " is not of type " + type
                            + ". Encountered while parsing '" + p + "' for '" + parseResult + "'");
                    return casted;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        if (!forward) Collections.reverse(parsed);
        return parsed;
    }

    static List<String> delimitedSplit(final char delimiter, final char closingDelimiter,
            final ParseResult parseResult, final boolean forward) {
        String value = parseResult.value;
        int openDelimiters = 0;
        List<String> values = new ArrayList<>();
        boolean done = false;

        int i = forward ? 0 : value.length() - 1;
        int step = forward ? 1 : -1;
        int end = forward ? value.length() : -1;
        int lastPos = forward ? 0 : value.length();
        for (; i != end; i += step) {
            char c = value.charAt(i);
            if (openDelimiters == 0) {
                if (closingDelimiter != EMPTY_CHAR && closingDelimiter == c) {
                    done = true;
                    break;
                } else if (delimiter == c) {
                    if (forward) {
                        values.add(value.substring(lastPos, i));
                        lastPos = i + 1;
                    } else {
                        values.add(value.substring(i + 1, lastPos));
                        lastPos = i;
                    }
                }
            }
            if (isOpenDelimiter(c)) openDelimiters++;
            if (isClosingDelimiter(c)) openDelimiters--;
        }
        if (!done && lastPos != end) {
            if (forward) {
                values.add(value.substring(lastPos));
            } else {
                values.add(value.substring(0, lastPos));
            }
            parseResult.value = "";
        } else {
            if (forward) {
                values.add(value.substring(lastPos, i));
                int endIndex = closingDelimiter != EMPTY_CHAR ? i + 1 : i;
                parseResult.value = parseResult.value.substring(endIndex);
            } else {
                values.add(value.substring(i + 1, lastPos));
                parseResult.value = parseResult.value.substring(0, i);
            }
        }
        return values;
    }

    static Object createObject(final String value) {
        try {
            return Class.forName(value).getDeclaredConstructor().newInstance();
        } catch (final Exception ignored) {
            return null;
        }
    }

    private static boolean isOpenDelimiter(final char c) {
        return c == LIST_START || c == MAP_START || c == ARG_START;
    }

    private static boolean isClosingDelimiter(final char c) {
        return c == LIST_END || c == MAP_END || c == ARG_END;
    }
}
