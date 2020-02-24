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
package com.github.weisj.darklaf.platform;

import com.github.weisj.darklaf.util.ReflectiveWarningSuppressor;
import com.sun.jna.Native;
import sun.awt.AWTAccessor;

import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

public class PointerUtil {

    /**
     * Get the window handle for the window the given component descends from.
     *
     * @param component the component.
     * @return the handle.
     */
    public static long getHWND(final Component component) {
        if (System.getProperty("os.name").toLowerCase().startsWith("mac")) {
            return getHWNDMacOS(component);
        } else {
            return Native.getComponentID(component);
        }
    }

    private static long getHWNDMacOS(final Component component) {
        long handle = Native.getComponentID(component);
        if (handle != 0) return handle;

        Window window = component instanceof Window ? (Window) component
                                                    : SwingUtilities.getWindowAncestor(component);
        try (ReflectiveWarningSuppressor sup = new ReflectiveWarningSuppressor()) {
            Object peer = AWTAccessor.getComponentAccessor().getPeer(window);
            Method getPlatformWindow = peer.getClass().getMethod("getPlatformWindow");
            Object platformWindow = getPlatformWindow.invoke(peer);
            Class<?> CFRetainedResource = Class.forName("sun.lwawt.macosx.CFRetainedResource");
            Field ptr = CFRetainedResource.getDeclaredField("ptr");
            ptr.setAccessible(true);
            return (Long) ptr.get(platformWindow);
        } catch (Exception ignored) {
        }
        return 0;
    }

}
