package com.weis.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public final class StringUtil {

    @Contract(pure = true)
    private StringUtil() {
    }

    @Contract("null -> null; !null -> !null")
    public static String toUpperCase(final String a) {
        return a == null ? null : toUpperCase((CharSequence) a).toString();
    }

    @Contract("null -> fail")
    public static CharSequence toUpperCase(final CharSequence s) {
        if (s == null) {
            throw new NullPointerException();
        }

        StringBuilder answer = null;

        for (int i = 0; i < s.length(); ++i) {
            char c = s.charAt(i);
            char upCased = toUpperCase(c);
            if (answer == null && upCased != c) {
                answer = new StringBuilder(s.length());
                answer.append(s.subSequence(0, i));
            }

            if (answer != null) {
                answer.append(upCased);
            }
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

    public static List<String> split(String s, String separator) {
        return split(s, separator, true);
    }

    public static List<String> split(String s, String separator, boolean excludeSeparator) {
        return split(s, separator, excludeSeparator, true);
    }

    public static List<String> split(String s, @NotNull String separator, boolean excludeSeparator, boolean excludeEmptyStrings) {
        if (separator.isEmpty()) {
            return Collections.singletonList(s);
        } else {
            List<String> result = new ArrayList<>();
            int pos = 0;

            while(true) {
                int index = s.indexOf(separator, pos);
                if (index == -1) {
                    if (pos < s.length() || !excludeEmptyStrings && pos == s.length()) {
                        result.add(s.substring(pos, s.length()));
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
