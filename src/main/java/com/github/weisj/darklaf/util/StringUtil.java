/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class StringUtil {

    @Contract(pure = true)
    private StringUtil() {
    }

    @Contract("null -> null; !null -> !null")
    public static String toUpperCase(final String a) {
        return a == null ? null : toUpperCase((CharSequence) a).toString();
    }

    @NotNull
    @Contract("null -> fail")
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

    @Contract(pure = true)
    public static char toUpperCase(final char a) {
        if (a < 'a') {
            return a;
        } else {
            return a <= 'z' ? (char) (a + -32) : Character.toUpperCase(a);
        }
    }

    public static List<String> split(final String s, final String separator) {
        return split(s, separator, true);
    }

    public static List<String> split(final String s, final String separator, final boolean excludeSeparator) {
        return split(s, separator, excludeSeparator, true);
    }

    public static List<String> split(final String s, @NotNull final String separator,
                                     final boolean excludeSeparator, final boolean excludeEmptyStrings) {
        if (separator.isEmpty()) {
            return Collections.singletonList(s);
        } else {
            List<String> result = new ArrayList<>();
            int pos = 0;

            while (true) {
                int index = s.indexOf(separator, pos);
                if (index == -1) {
                    if (pos < s.length() || !excludeEmptyStrings && pos == s.length()) {
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

    public static int indexOfIgnoreCase(@NotNull final String where, final char what, int fromIndex) {
        int sourceCount = where.length();
        if (fromIndex >= sourceCount) {
            return -1;
        } else {
            if (fromIndex < 0) {
                fromIndex = 0;
            }

            for (int i = fromIndex; i < sourceCount; ++i) {
                if (charsEqualIgnoreCase(where.charAt(i), what)) {
                    return i;
                }
            }

            return -1;
        }
    }

    public static boolean charsEqualIgnoreCase(final char a, final char b) {
        return a == b || toUpperCase(a) == toUpperCase(b) || toLowerCase(a) == toLowerCase(b);
    }

    @Contract(pure = true)
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

    public static int compareVersionNumbers(final String v1, final String v2) {
        if (v1 == null && v2 == null) {
            return 0;
        } else if (v1 == null) {
            return -1;
        } else if (v2 == null) {
            return 1;
        } else {
            String[] part1 = v1.split("[._\\-]");
            String[] part2 = v2.split("[._\\-]");

            int idx;
            for (idx = 0; idx < part1.length && idx < part2.length; ++idx) {
                String p1 = part1[idx];
                String p2 = part2[idx];
                int cmp;
                if (p1.matches("\\d+") && p2.matches("\\d+")) {
                    cmp = (Integer.valueOf(p1)).compareTo(Integer.valueOf(p2));
                } else {
                    cmp = part1[idx].compareTo(part2[idx]);
                }

                if (cmp != 0) {
                    return cmp;
                }
            }

            if (part1.length == part2.length) {
                return 0;
            } else {
                boolean left = part1.length > idx;

                for (String[] parts = left ? part1 : part2; idx < parts.length; ++idx) {
                    String p = parts[idx];
                    int cmp;
                    if (p.matches("\\d+")) {
                        cmp = (Integer.valueOf(p)).compareTo(0);
                    } else {
                        cmp = 1;
                    }

                    if (cmp != 0) {
                        return left ? cmp : -cmp;
                    }
                }

                return 0;
            }
        }
    }

    public static boolean isBlank(final String s) {
        return s.trim().length() == 0;
    }

    @NotNull
    public static String repeat(@NotNull final String s, final int count) {
        if (count <= 0) return s;
        if (count == 1) return s;
        StringBuilder builder = new StringBuilder(s.length() * count);
        for (int i = 0; i < count; i++) {
            builder.append(s);
        }
        return builder.toString();
    }
}
