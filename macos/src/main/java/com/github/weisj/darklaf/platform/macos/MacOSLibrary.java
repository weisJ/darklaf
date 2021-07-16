/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.platform.macos;

import java.util.Collections;
import java.util.List;

import com.github.weisj.darklaf.nativeutil.AbstractLibrary;
import com.github.weisj.darklaf.nativeutil.NativeUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.SystemInfo;

public class MacOSLibrary extends AbstractLibrary {

    private static final String PATH = "/com/github/weisj/darklaf/platform/darklaf-macos/";
    private static final String x86_64_PATH = "macos-x86-64/";
    private static final String arm64_PATH = "macos-arm64/";
    private static final String DLL_NAME = "libdarklaf-macos.dylib";

    private static final String FRAMEWORK_TARGET_PATH = "JavaNativeFoundation.framework/";
    private static final String FRAMEWORK_PATH = PATH + FRAMEWORK_TARGET_PATH + "JavaNativeFoundation";
    private static final MacOSLibrary instance = new MacOSLibrary();

    public static MacOSLibrary get() {
        return instance;
    }

    protected MacOSLibrary() {
        super(PATH, DLL_NAME, LogUtil.getLogger(MacOSLibrary.class));
    }

    private String getArm64Path() {
        return super.getPath() + arm64_PATH;
    }

    private String getX64Path() {
        return super.getPath() + x86_64_PATH;
    }


    @Override
    protected String getPath() {
        if (SystemInfo.isX86Compatible && SystemInfo.isX64) {
            return getX64Path();
        } else if (SystemInfo.isM1) {
            return getArm64Path();
        } else {
            throw new IllegalStateException("Unsupported arch");
        }
    }

    @Override
    protected List<NativeUtil.Resource> getResourcePaths() {
        return Collections.singletonList(
                new NativeUtil.Resource(FRAMEWORK_PATH, FRAMEWORK_TARGET_PATH));
    }

    @Override
    protected boolean canLoad() {
        return ((SystemInfo.isX86Compatible && SystemInfo.isX64) || SystemInfo.isM1) && SystemInfo.isMacOSYosemite;
    }
}
