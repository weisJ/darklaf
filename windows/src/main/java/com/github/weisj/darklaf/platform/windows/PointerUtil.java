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
package com.github.weisj.darklaf.platform.windows;

import java.awt.*;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.SystemInfo;

public final class PointerUtil {

    private static final Logger LOGGER = LogUtil.getLogger(PointerUtil.class);

    /**
     * Get the window handle for the window the given component descends from.
     *
     * @param component the component.
     * @return the handle.
     */
    public static long getHWND(final Component component) {
        Window window = component instanceof Window
                ? (Window) component
                : SwingUtilities.getWindowAncestor(component);
        String javaLibPath = SystemInfo.JAVA_HOME != null
                ? SystemInfo.JAVA_HOME + "/bin/jawt.dll"
                : "jawt.dll";
        long hwnd = JNIDecorationsWindows.getWindowHWND(window, javaLibPath);
        if (hwnd < 0) {
            LOGGER.severe("Couldn't get HWND of window (errorCode =" + hwnd + ")." + component);
        }
        return hwnd;
    }
}
