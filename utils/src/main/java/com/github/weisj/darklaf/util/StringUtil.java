/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * Provides some additional utility functions for handling {@link String}s.
 *
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class StringUtil {

    private StringUtil() {}

    public static String toHtml(final String text) {
        if (text == null) return "";
        if (!text.startsWith("<html>") && text.contains("\n")) {
            return "<html>" + text.replaceAll("\n", "<\\br>") + "</html>";
        }
        return text;
    }

    public static String toUpperCase(final String a) {
        return a == null ? null : toUpperCase((CharSequence) a).toString();
    }

    public static CharSequence toUpperCase(final CharSequence s) {
        if (s == null) {
            throw new NullPointerException("CharSequence can't be null");
        }
        if (s.length() == 0) {
            return s;
        }

        StringBuilder answer = new StringBuilder();

        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);
            char upCased = toUpperCase(c);
            answer.append(upCased);
        }
        return answer;
    }

    public static char toUpperCase(final char a) {
        if (a < 'a') {
            return a;
        } else {
            return a <= 'z' ? (char) (a + 32) : Character.toUpperCase(a);
        }
    }

    public static Iterable<String> split(final String s, final String separator) {
        return split(s, separator, true);
    }

    public static Iterable<String> split(final String s, final String separator, final boolean excludeSeparator) {
        return split(s, separator, excludeSeparator, true);
    }

    public static Iterable<String> split(final String s, final String separator, final boolean excludeSeparator,
            final boolean excludeEmptyStrings) {
        if (separator.isEmpty()) {
            return Collections.singletonList(s);
        } else {
            List<String> result = new ArrayList<>();
            int pos = 0;

            while (true) {
                int index = s.indexOf(separator, pos);
                if (index == -1) {
                    if (pos < s.length() || (!excludeEmptyStrings && pos == s.length())) {
                        result.add(s.substring(pos));
                    }

                    return result;
                }

                int nextPos = index + separator.length();
                String token = s.substring(pos, excludeSeparator ? index : nextPos);
                if (!token.isEmpty() || !excludeEmptyStrings) {
                    result.add(token);
                }

                pos = nextPos;
            }
        }
    }

    public static boolean charsEqualIgnoreCase(final char a, final char b) {
        return a == b || toUpperCase(a) == toUpperCase(b) || toLowerCase(a) == toLowerCase(b);
    }

    public static char toLowerCase(final char a) {
        if (a >= 'A' && (a < 'a' || a > 'z')) {
            return a <= 'Z' ? (char) (a + 32) : Character.toLowerCase(a);
        } else {
            return a;
        }
    }

    public static boolean containsIgnoreCase(final String where, final String what) {
        return indexOfIgnoreCase(where, what, 0) >= 0;
    }

    public static int indexOfIgnoreCase(final String where, final String what, int fromIndex) {
        int targetCount = what.length();
        int sourceCount = where.length();
        if (fromIndex >= sourceCount) {
            return targetCount == 0 ? sourceCount : -1;
        } else {
            if (fromIndex < 0) {
                fromIndex = 0;
            }

            if (targetCount == 0) {
                return fromIndex;
            } else {
                char first = what.charAt(0);
                int max = sourceCount - targetCount;

                for (int i = fromIndex; i <= max; ++i) {
                    if (!charsEqualIgnoreCase(where.charAt(i), first)) {
                        do {
                            ++i;
                        } while (i <= max && !charsEqualIgnoreCase(where.charAt(i), first));
                    }

                    if (i <= max) {
                        int j = i + 1;
                        int end = j + targetCount - 1;

                        for (int k = 1; j < end && charsEqualIgnoreCase(where.charAt(j), what.charAt(k)); ++k) {
                            ++j;
                        }

                        if (j == end) {
                            return i;
                        }
                    }
                }

                return -1;
            }
        }
    }

    public static boolean isBlank(final String s) {
        return s.trim().length() == 0;
    }

    public static String repeat(final String s, final int count) {
        if (count <= 0) return "";
        if (count == 1) return s;
        StringBuilder builder = new StringBuilder(s.length() * count);
        builder.append(s.repeat(count));
        return builder.toString();
    }

    public static String orEmpty(final String str) {
        return str != null ? str : "";
    }

    public static String orEmpty(final Object obj) {
        return obj != null ? orEmpty(obj.toString()) : "";
    }
}
