package com.weis.darklaf.util;

import com.sun.jna.LastErrorException;
import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.sun.jna.platform.win32.User32;
import com.sun.jna.platform.win32.WinDef;
import com.sun.jna.platform.win32.WinDef.HWND;
import com.sun.jna.win32.StdCallLibrary;
import com.sun.jna.win32.W32APIOptions;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;

public final class WindowsFrameUtil {

    public static final WinDef.LRESULT PASS_TO_SUPER = new WinDef.LRESULT(-1);
    public static final WinDef.LRESULT PASS_TO_DEFAULT = new WinDef.LRESULT(-2);
    private static int prevWndProc;

    public static void enableTitleBar(final Window window, final boolean enabled) {
        if (window == null) return;

        final HWND hwnd = getHWND(window);
        int style = User32.INSTANCE.GetWindowLong(hwnd, User32dll.GWL_STYLE);
        if (enabled) {
            style |= 0xc00000;
        } else {
            style = 0x00040000;
        }
        //ResizeMode="CanResizeWithGrip" AllowsTransparency="True"
        User32.INSTANCE.SetWindowLong(hwnd, User32dll.GWL_STYLE, style);
        User32.INSTANCE.SetWindowPos(hwnd, new HWND(new Pointer(0L)),
                                     0, 0, 0, 0, 0x27);
    }

    public static void setWindowCallback(final Window window, final WindowProc callback) {
        if (window == null) return;

        final HWND hwnd = getHWND(window);
        callback.parent = User32dll.INSTANCE.GetWindowLongPtr(hwnd, (long) User32dll.GWL_WNDPROC);
        User32dll.INSTANCE.SetWindowLongPtr(hwnd, User32dll.GWL_WNDPROC, callback);
    }

    @NotNull
    @Contract("_ -> new")
    public static HWND getHWND(final Component component) {
        var hwnd = new HWND();
        hwnd.setPointer(Native.getComponentPointer(component));
        return hwnd;
    }

    public interface User32dll extends User32 {

        User32dll INSTANCE = Native.loadLibrary("user32", User32dll.class,
                                                W32APIOptions.UNICODE_OPTIONS);

        WNDPROC GetWindowLongPtr(HWND hWnd, long nIndex) throws LastErrorException;

        LONG_PTR SetWindowLongPtr(HWND hWnd, int nIndex, WNDPROC wndProc) throws LastErrorException;

        int ShowCursor(boolean bShow);

        void DisableProcessWindowsGhosting() throws LastErrorException;

        INT_PTR GetWindowDC(HWND hwnd);

        int ReleaseDC(HWND hwnd, INT_PTR hdc);
    }

    public interface WNDPROC extends StdCallLibrary.StdCallCallback {
        WinDef.LRESULT callback(HWND hWnd, int uMsg, WinDef.WPARAM uParam, WinDef.LPARAM lParam) throws LastErrorException;
    }

    public abstract static class WindowProc implements WNDPROC {

        protected WNDPROC parent;

    }
}
