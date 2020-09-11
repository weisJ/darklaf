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
package com.github.weisj.darklaf.platform.windows;

public class JNIThemeInfoWindows {

    /**
     * Returns whether dark mode is enabled.
     *
     * @return true if enabled.
     */
    public static native boolean isDarkThemeEnabled();

    /**
     * Returns whether high contrast mode is enabled.
     *
     * @return true if enabled.
     */
    public static native boolean isHighContrastEnabled();

    /**
     * Returns the font scaling factor in percent i.e. 100 is original size and 200 is twice as big.
     *
     * @return the font scale factor.
     */
    public static native long getFontScaleFactor();

    /**
     * Returns the currently used accent color.
     *
     * @return the accent color in the format 0xAARRGGBB.
     */
    public static native int getAccentColor();

    /**
     * Create a monitor event handler.
     *
     * @param  callback the event callback.
     * @return          the event handler pointer.
     */
    public static native long createEventHandler(final Runnable callback);

    /**
     * Deletes the event handler.
     *
     * @param handle the event handler pointer.
     */
    public static native void deleteEventHandler(final long handle);
}
