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
package com.github.weisj.darklaf.platform.macos.darkmode;

import com.github.weisj.darklaf.platform.AbstractLibrary;
import com.github.weisj.darklaf.util.SystemInfo;

public class MacOSDarkMode extends AbstractLibrary {

    private static final String PATH = "/com/github/weisj/darklaf/platform/darklaf-macos-dark-mode/macos-x86-64/";
    private static final String DLL_NAME = "libdarklaf-macos-dark-mode.dylib";
    private static final MacOSDarkMode instance = new MacOSDarkMode();

    public static MacOSDarkMode get() {
        return instance;
    }

    private static native boolean nativeIsDarkThemeEnabled();

    /**
     * Returns whether dark mode is enabled.
     *
     * @return true if dark mode is enabled.
     */
    public static boolean isDarkThemeEnabled() {
        return get().isLoaded() && nativeIsDarkThemeEnabled();
    }

    protected MacOSDarkMode() {
        super(PATH, DLL_NAME);
    }

    @Override
    protected String getLibraryPath() {
        if (!SystemInfo.isX64) {
            logger.warning("JRE model '"
                           + SystemInfo.jreArchitecture
                           + "' not supported. Native features will be disabled");
            return null;
        }
        return super.getLibraryPath();
    }

    @Override
    protected boolean canLoad() {
        return SystemInfo.isMacOSMojave;
    }
}
