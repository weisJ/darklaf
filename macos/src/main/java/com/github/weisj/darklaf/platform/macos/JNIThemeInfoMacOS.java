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
 *
 */
package com.github.weisj.darklaf.platform.macos;

import java.awt.*;

import com.github.weisj.darklaf.platform.macos.theme.MacOSColors;

public class JNIThemeInfoMacOS {

    /**
     * Returns whether high contrast mode is enabled.
     *
     * @return true if high contrast mode is enabled.
     */
    public static native boolean isHighContrastEnabled();

    /*
     * Get the index of the accent color.
     */
    private static native int nativeGetAccentColor();

    /**
     * Returns the current accent color.
     *
     * @return the accent color.
     */
    public static Color getAccentColor() {
        int index = nativeGetAccentColor();
        switch (index) {
            case -2 :
                return MacOSColors.BLUE;
            case -1 :
                return MacOSColors.GRAY;
            case 0 :
                return MacOSColors.RED;
            case 1 :
                return MacOSColors.ORANGE;
            case 2 :
                return MacOSColors.YELLOW;
            case 3 :
                return MacOSColors.GREEN;
            case 5 :
                return MacOSColors.LILAC;
            case 6 :
                return MacOSColors.ROSE;
            default :
                return null;
        }
    }

    /*
     * Returns the selection color as an AARRGGBB integer.
     */
    private static native int nativeGetSelectionColor();

    /**
     * Returns the current selection color.
     *
     * @return the current selection color.
     */
    public static Color getSelectionColor() {
        int rgba = nativeGetSelectionColor();
        // If rgba == 0 then it has an alpha channel != 255, so it is invalid.
        if (rgba == 0) return null;
        return new Color(rgba);
    }

    /**
     * Create an preference change listener.
     *
     * @param  callback the event callback.
     * @return          the pointer to the listener.
     */
    public static native long createPreferenceChangeListener(final Runnable callback);

    /**
     * Delete the preference change listener.
     *
     * @param listenerPtr pointer to the listener.
     */
    public static native void deletePreferenceChangeListener(final long listenerPtr);
}
