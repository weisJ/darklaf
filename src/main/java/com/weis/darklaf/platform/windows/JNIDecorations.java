package com.weis.darklaf.platform.windows;

import com.sun.jna.Native;
import com.sun.jna.Pointer;
import com.weis.darklaf.DarkLaf;
import com.weis.darklaf.platform.NativeUtil;
import com.weis.darklaf.util.SystemInfo;
import org.jetbrains.annotations.Contract;

import java.awt.*;
import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class JNIDecorations {

    private static final Logger LOGGER = Logger.getLogger(JNIDecorations.class.getName());
    private static boolean supported;
    private static boolean loaded;

    static {
        updateLibrary();
    }

    public static native void updateValues(final long hwnd, final int left, final int right, final int height);

    public static native void setResizable(final long hwnd, final boolean resizable);

    public static native void setBackground(final long hwnd, final int r, final int g, final int b);

    public static native void minimize(final long hwnd);

    public static native void maximize(final long hwnd);

    public static native void restore(final long hwnd);

    public static native void installDecorations(final long hwnd);

    public static native void uninstallDecorations(final long hwnd);

    public static long getHWND(final Component component) {
        return Pointer.nativeValue(Native.getComponentPointer(component));
    }

    /**
     * Load the decorations library if necessary.
     *
     * @return true if successful and library wasn't already loaded.
     */
    public static boolean updateLibrary() {
        boolean oldLoaded = loaded;
        if (!supported) {
            supported = loadLibrary();
        }
        return supported && !oldLoaded;
    }

    private static boolean loadLibrary() {
        if (!SystemInfo.isWindows || !DarkLaf.isDecorationsEnabled()) {
            return false;
        }
        if (loaded) return true;
        try {
            if (SystemInfo.isX86) {
                NativeUtil.loadLibraryFromJar("/library/x86/jniplatform.dll");
            } else if (SystemInfo.isX64) {
                NativeUtil.loadLibraryFromJar("/library/x64/jniplatform.dll");
            } else {
                LOGGER.warning("Could not determine jre model '"
                                       + SystemInfo.jreArchitecture
                                       + "'. Decorations will be disabled");
                return false;
            }
            loaded = true;
            LOGGER.info("Loaded jniplatform.dll. Decorations are enabled.");
            return true;
        } catch (IOException e) {
            LOGGER.log(Level.SEVERE, "Could not load decorations library. Decorations will be disabled",
                       e.getMessage());
            return false;
        }
    }

    @Contract(pure = true)
    public static boolean isCustomDecorationSupported() {
        return supported;
    }
}
