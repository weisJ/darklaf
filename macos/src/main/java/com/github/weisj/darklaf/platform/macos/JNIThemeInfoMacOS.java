/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.platform.macos;

import com.github.weisj.darklaf.platform.macos.theme.MacOSColors;

import java.awt.*;

public class JNIThemeInfoMacOS {

    public static native boolean isDarkThemeEnabled();

    public static native boolean isHighContrastEnabled();

    private static native int nativeGetAccentColor();

    public static Color getAccentColor() {
        int index = nativeGetAccentColor();
        switch (index) {
            case -1:
                return MacOSColors.GRAY;
            case 0:
                return MacOSColors.RED;
            case 1:
                return MacOSColors.ORANGE;
            case 2:
                return MacOSColors.YELLOW;
            case 3:
                return MacOSColors.GREEN;
            case 5:
                return MacOSColors.LILAC;
            case 6:
                return MacOSColors.ROSE;
            default:
                return MacOSColors.BLUE;
        }
    }

    public static native int nativeGetSelectionColor();

    public static Color getSelectionColor() {
        int rgba = nativeGetSelectionColor();
        // If rgba == 0 then it has an alpha channel != 255, so it is invalid.
        if (rgba == 0) return null;
        return new Color(rgba);
    }

    public static native long createPreferenceChangeListener();

    public static native void deletePreferenceChangeListener(final long listenerPtr);
}
