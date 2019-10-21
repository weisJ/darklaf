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
package com.github.weisj.darklaf.platform.windows;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.platform.NativeUtil;
import com.github.weisj.darklaf.util.SystemInfo;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import org.jetbrains.annotations.Contract;

import java.awt.*;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 */
public class JNIDecorations {

    private static final Logger LOGGER = Logger.getLogger(JNIDecorations.class.getName());
    private static boolean supported;
    private static boolean loaded;

    static {
        updateLibrary();
    }

    public static native void updateValues(final long hwnd, final int left, final int right, final int height);

    public static native void setResizable(final long hwnd, final boolean resizable);

    public static native void setBackground(final long hwnd, final int r, final int g, final int b);

    public static native void minimize(final long hwnd);

    public static native void maximize(final long hwnd);

    public static native void restore(final long hwnd);

    public static native void installDecorations(final long hwnd);

    public static native void uninstallDecorations(final long hwnd);

    public static long getHWND(final Component component) {
        return Pointer.nativeValue(Native.getComponentPointer(component));
    }

    /**
     * Load the decorations library if necessary.
     *
     * @return true if successful and library wasn't already loaded.
     */
    public static boolean updateLibrary() {
        boolean oldLoaded = loaded;
        if (!supported) {
            supported = loadLibrary();
        }
        return supported && !oldLoaded;
    }

    private static boolean loadLibrary() {
        if (!SystemInfo.isWindows || !DarkLaf.isDecorationsEnabled()) {
            return false;
        }
        if (loaded) return true;
        try {
            if (SystemInfo.isX86) {
                NativeUtil.loadLibraryFromJar("/com/github/weisj/darklaf/platform/windows/x86/jniplatform.dll");
            } else if (SystemInfo.isX64) {
                NativeUtil.loadLibraryFromJar("/com/github/weisj/darklaf/platform/windows/x64/jniplatform.dll");
            } else {
                LOGGER.warning("Could not determine jre model '"
                                       + SystemInfo.jreArchitecture
                                       + "'. Decorations will be disabled");
                return false;
            }
            loaded = true;
            LOGGER.info("Loaded jniplatform.dll. Decorations are enabled.");
            return true;
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Could not load decorations library. Decorations will be disabled",
                       e.getMessage());
            return false;
        }
    }

    @Contract(pure = true)
    public static boolean isCustomDecorationSupported() {
        return supported;
    }
}
