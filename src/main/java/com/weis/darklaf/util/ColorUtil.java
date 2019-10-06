package com.weis.darklaf.util;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

public final class ColorUtil {

    @Contract(pure = true)
    private ColorUtil() {

    }

    @NotNull
    @Contract("_, _ -> new")
    public static Color shift(@NotNull final Color c, final double d) {
        return new Color(shift(c.getRed(), d), shift(c.getGreen(), d), shift(c.getBlue(), d), c.getAlpha());
    }

    @Contract(pure = true)
    private static int shift(final int colorComponent, final double d) {
        int n = (int) ((double) colorComponent * d);
        return n > 255 ? 255 : (Math.max(n, 0));
    }

    @NotNull
    @Contract("_, _ -> new")
    public static Color toAlpha(final Color color, final int a) {
        Color c = color != null ? color : Color.black;
        return new Color(c.getRed(), c.getGreen(), c.getBlue(), a);
    }

    public static Color fromHex(final String str, final Color defaultValue) {
        try {
            return fromHex(str);
        } catch (Exception var3) {
            return defaultValue;
        }
    }

    @NotNull
    public static Color fromHex(@NotNull String str) {
        if (str.startsWith("#")) {
            str = str.substring(1);
        }

        if (str.length() == 3) {
            return new Color(17 * Integer.valueOf(String.valueOf(str.charAt(0)), 16),
                             17 * Integer.valueOf(String.valueOf(str.charAt(1)), 16),
                             17 * Integer.valueOf(String.valueOf(str.charAt(2)), 16));
        } else if (str.length() == 6) {
            return Color.decode("0x" + str);
        } else {
            throw new IllegalArgumentException("Should be String of 3 or 6 chars length.");
        }
    }

}
