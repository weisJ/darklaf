package com.weis.darklaf.platform.windows;

import com.bulenkov.iconloader.util.SystemInfo;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinDef;
import com.weis.darklaf.platform.NativeUtil;
import org.jetbrains.annotations.Contract;

import java.awt.*;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class JNIDecorations {

    public static native void updateValues(final long hwnd, final int left, final int right, final int height);

    public static native void setResizable(final long hwnd, final boolean resizable);

    public static native void setBackground(final long hwnd, final int r, final int g, final int b);

    public static native void minimize(final long hwnd);

    public static native void maximize(final long hwnd);

    public static native void restore(final long hwnd);

    public static native void installDecorations(final long hwnd);

    public static native void uninstallDecorations(final long hwnd);

    public static long getHWND(final Component component) {
        var hwnd = new WinDef.HWND();
        hwnd.setPointer(Native.getComponentPointer(component));
        return Pointer.nativeValue(hwnd.getPointer());
    }

    private static final Logger LOGGER = Logger.getLogger(JNIDecorations.class.getName());
    private static boolean supported = false;

    static {
        if (SystemInfo.isWindows) {
            try {
                NativeUtil.loadLibraryFromJar("/library/jniplatform.dll");
                supported = true;
                LOGGER.info("Loaded jniplatform.dll. Decorations are enabled.");
            } catch (IOException e) {
                supported = false;
                LOGGER.log(Level.SEVERE, "Could not load decorations library. Decorations will be disabled",
                           e.getMessage());
            }
        }
    }

    @Contract(pure = true)
    public static boolean isCustomDecorationSupported() {
        return supported;
    }
}
