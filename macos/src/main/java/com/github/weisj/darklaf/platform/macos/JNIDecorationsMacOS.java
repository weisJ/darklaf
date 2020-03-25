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

import com.github.weisj.darklaf.platform.NativeUtil;
import com.github.weisj.darklaf.util.SystemInfo;

import java.awt.*;
import java.util.logging.Level;
import java.util.logging.Logger;

public class JNIDecorationsMacOS {

    private static final Logger LOGGER = Logger.getLogger(JNIDecorationsMacOS.class.getName());
    private static boolean loaded;
    private static boolean attemptedLoad;

    public static native long getComponentPointer(final Window window);

    public static native void retainWindow(final long hwnd);

    public static native void releaseWindow(final long hwnd);

    public static native double getTitleBarHeight(final long hwnd);

    public static native void installDecorations(final long hwnd);

    public static native void uninstallDecorations(final long hwnd);

    public static native void setTitleEnabled(final long hwnd, final boolean enabled);

    public static native void setDarkTheme(final long hwnd, final boolean darkEnabled);

    public static native boolean isFullscreen(final long hwnd);

    public static native double getTitleFontSize(final long hwnd);


    /**
     * Load the decorations-library if necessary.
     */
    public static void updateLibrary() {
        if (!loaded && !attemptedLoad) {
            loadLibrary();
        }
    }

    private static void loadLibrary() {
        attemptedLoad = true;
        if (!SystemInfo.isMac || loaded) {
            return;
        }
        try {
            if (SystemInfo.isX64) {
                NativeUtil.loadLibraryFromJar(
                    "/com/github/weisj/darklaf/platform/darklaf-macos/macos-x86-64/libdarklaf-macos.dylib");
                loaded = true;
                LOGGER.info("Loaded libdarklaf-macos.dylib. Decorations are enabled.");
            } else {
                LOGGER.warning("JRE model '"
                               + SystemInfo.jreArchitecture
                               + "' not supported. Decorations will be disabled");
            }
        } catch (Throwable e) {
            //Library not found, SecurityManager prevents library loading etc.
            LOGGER.log(Level.SEVERE, "Could not load decorations library libdarklaf-macos.dylib." +
                                     " Decorations will be disabled", e);
        }
    }

    public static boolean isCustomDecorationSupported() {
        return loaded;
    }
}
