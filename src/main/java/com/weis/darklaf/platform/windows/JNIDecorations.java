package com.weis.darklaf.platform.windows;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.WinDef;

import java.awt.*;

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

    static {
        System.loadLibrary("jniplatform");
    }
}
