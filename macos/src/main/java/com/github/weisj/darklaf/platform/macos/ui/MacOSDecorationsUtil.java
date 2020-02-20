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
package com.github.weisj.darklaf.platform.macos.ui;

import com.github.weisj.darklaf.util.SystemInfo;

import javax.swing.*;
import java.awt.peer.WindowPeer;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public class MacOSDecorationsUtil {

    private static final String FULL_WINDOW_CONTENT_KEY = "apple.awt.fullWindowContent";
    private static final String TRANSPARENT_TITLE_BAR_KEY = "apple.awt.transparentTitleBar";

    private static final int FULL_WINDOW_CONTENT_MASK = 1 << 14;
    private static final int TRANSPARENT_TITLE_BAR_MASK = 1 << 18;

    protected static boolean isFullWindowContentEnabled(final JRootPane rootPane) {
        if (rootPane == null) return false;
        if (SystemInfo.isJavaVersionAtLeast("12")) {
            return Boolean.TRUE.equals(rootPane.getClientProperty(FULL_WINDOW_CONTENT_KEY));
        }
        return false;
    }

    protected static boolean isTransparentTitleBarEnabled(final JRootPane rootPane) {
        if (rootPane == null) return false;
        if (SystemInfo.isJavaVersionAtLeast("12")) {
            return Boolean.TRUE.equals(rootPane.getClientProperty(TRANSPARENT_TITLE_BAR_KEY));
        }
        return false;
    }

    protected static void setFullWindowContentEnabled(final JRootPane rootPane, final boolean enabled) {
        if (rootPane == null) return;
        if (SystemInfo.isJavaVersionAtLeast("12")) {
            rootPane.putClientProperty(FULL_WINDOW_CONTENT_KEY, enabled);
        } else {
            setStyleBits(rootPane, FULL_WINDOW_CONTENT_MASK, enabled);
        }
    }

    protected static void setTransparentTitleBarEnabled(final JRootPane rootPane, final boolean enabled) {
        if (rootPane == null) return;
        if (SystemInfo.isJavaVersionAtLeast("12")) {
            rootPane.putClientProperty(TRANSPARENT_TITLE_BAR_KEY, enabled);
        } else {
            setStyleBits(rootPane, TRANSPARENT_TITLE_BAR_MASK, enabled);
        }
    }

    private static Object getPlatformWindow(final JRootPane rootPane) {
        WindowPeer peer = (WindowPeer) SwingUtilities.getWindowAncestor(rootPane).getPeer();
        if (peer.getClass().getName().equals("sun.lwawt.LWWindowPeer")) {
            try {
                Method getPlatformWindow = peer.getClass().getDeclaredMethod("getPlatformWindow");
                getPlatformWindow.setAccessible(true);
                Object platformWindow = getPlatformWindow.invoke(peer);
                if (platformWindow.getClass().getName().equals("sun.lwawt.macosx.CPlatformWindow")) {
                    return platformWindow;
                }
            } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    private static void setStyleBits(final JRootPane rootPane, final int mask, final boolean value) {
        Object platformWindow = getPlatformWindow(rootPane);
        if (platformWindow != null) {
            try {
                Method setStyleBits = platformWindow.getClass().getDeclaredMethod("setStyleBits",
                                                                                  int.class, boolean.class);
                setStyleBits.setAccessible(true);
                setStyleBits.invoke(platformWindow, mask, value);
            } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
                e.printStackTrace();
            }
        }
    }
}
