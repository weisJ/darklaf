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
package com.github.weisj.darklaf.platform.windows;

import java.util.logging.Level;
import java.util.logging.Logger;

import com.github.weisj.darklaf.platform.NativeUtil;
import com.github.weisj.darklaf.util.SystemInfo;

public class WindowsLibrary {

    private static final Logger LOGGER = Logger.getLogger(WindowsLibrary.class.getName());

    private static boolean loaded;
    private static boolean attemptedLoad;

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
        if (!SystemInfo.isWindows || loaded) {
            return;
        }
        try {
            if (SystemInfo.isX86) {
                NativeUtil.loadLibraryFromJar("/com/github/weisj/darklaf/platform/darklaf-windows/windows-x86/darklaf-windows.dll");
            } else if (SystemInfo.isX64) {
                NativeUtil.loadLibraryFromJar("/com/github/weisj/darklaf/platform/darklaf-windows/windows-x86-64/darklaf-windows.dll");
            } else {
                LOGGER.warning("Could not determine jre model '"
                               + SystemInfo.jreArchitecture
                               + "'. Native features will be disabled");
                return;
            }
            loaded = true;
            LOGGER.info("Loaded darklaf-windows.dll. Native features are enabled.");
        } catch (Throwable e) {
            // Library not found, SecurityManager prevents library loading etc.
            LOGGER.log(Level.SEVERE,
                       "Could not load decorations library darklaf-windows.dll." +
                                     " Native features will be disabled",
                       e);
        }
    }

    public static boolean isLoaded() {
        return loaded;
    }
}
