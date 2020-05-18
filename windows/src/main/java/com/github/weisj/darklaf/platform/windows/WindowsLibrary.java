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

import com.github.weisj.darklaf.platform.AbstractLibrary;
import com.github.weisj.darklaf.util.SystemInfo;

public class WindowsLibrary extends AbstractLibrary {

    private static final String PATH = "/com/github/weisj/darklaf/platform/darklaf-windows/";
    private static final String DLL_NAME = "darklaf-windows.dll";
    private static final String x86_PATH = "windows-x86/";
    private static final String x86_64_PATH = "windows-x86-64/";
    private static final WindowsLibrary library = new WindowsLibrary();

    public static WindowsLibrary get() {
        return library;
    }

    protected WindowsLibrary() {
        super(PATH, DLL_NAME);
    }

    @Override
    protected String getLibraryPath() {
        if (!(SystemInfo.isX64 || SystemInfo.isX86)) {
            logger.warning("Could not determine jre model '"
                           + SystemInfo.jreArchitecture
                           + "'. Native features will be disabled");
            return null;
        }
        return super.getLibraryPath();
    }

    @Override
    protected String getPath() {
        if (SystemInfo.isX86) {
            return super.getPath() + x86_PATH;
        } else if (SystemInfo.isX64) {
            return super.getPath() + x86_64_PATH;
        } else {
            return super.getPath();
        }
    }

    @Override
    protected boolean canLoad() {
        return SystemInfo.isWindows10;
    }
}
