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
package com.github.weisj.darklaf.theme.info;

import java.util.function.Consumer;

public interface ThemePreferenceProvider {

    /**
     * Get the preferred theme style.
     *
     * @return the preferred theme style.
     */
    PreferredThemeStyle getPreference();

    /** Initialize all necessary resources. */
    void initialize();

    /**
     * Set the callback for changes is the preferred theme style.
     *
     * @param callback the callback.
     */
    void setCallback(final Consumer<PreferredThemeStyle> callback);

    /**
     * Sets whether changes in theme preference should be signaled to the callback.
     *
     * @see #setCallback(Consumer)
     */
    void setReporting(final boolean reporting);

    /**
     * Returns whether changes in theme preference are signaled to the callback.
     *
     * @return true if changes are reported.
     * @see #setCallback(Consumer)
     */
    boolean isReporting();

    /**
     * Returns whether the provider can report changes in system preferences.
     *
     * @return true if reporting is supported.
     */
    default boolean canReport() {
        return false;
    }

    /**
     * Returns whether this provider can provide the native accent color value.
     *
     * @return true if supported.
     */
    default boolean supportsNativeAccentColor() {
        return false;
    }

    /**
     * Returns whether this provider can provide the native selection color value.
     *
     * @return true if supported.
     */
    default boolean supportsNativeSelectionColor() {
        return false;
    }

    /**
     * Returns whether this provider can provide the native font size.
     *
     * @return true if supported.
     */
    default boolean supportsNativeFontSize() {
        return false;
    }

    /**
     * Returns whether this provider can provide the native theme preferences.
     *
     * @return true if supported.
     */
    default boolean supportsNativeTheme() {
        return false;
    }
}
