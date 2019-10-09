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
package com.weis.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
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

    @Contract(pure = true)
    public static char toLowerCase(final char a) {
        if (a >= 'A' && (a < 'a' || a > 'z')) {
            return a <= 'Z' ? (char) (a + 32) : Character.toLowerCase(a);
        } else {
            return a;
        }
    }

    public static List<String> split(final String s, final String separator) {
        return split(s, separator, true);
    }

    public static List<String> split(final String s, final String separator, final boolean excludeSeparator) {
        return split(s, separator, excludeSeparator, true);
    }

    public static List<String> split(final String s, @NotNull final String separator, final boolean excludeSeparator, final boolean excludeEmptyStrings) {
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
}
