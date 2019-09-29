package com.weis.darklaf.util;

import com.intellij.openapi.util.text.StringUtilRt;
import org.jetbrains.annotations.Contract;

public final class StringUtil {

    @Contract(pure = true)
    private StringUtil() {
    }

    @Contract("null -> null; !null -> !null")
    public static String toUpperCase(final String a) {
        return a == null ? null : StringUtilRt.toUpperCase(a).toString();
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
}
