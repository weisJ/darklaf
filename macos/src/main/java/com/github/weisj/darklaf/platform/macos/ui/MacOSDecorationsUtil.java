/*
 * MIT License
 *
 * Copyright (c) 2020-2022 Jannis Weis
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
 */
package com.github.weisj.darklaf.platform.macos.ui;

import java.awt.*;
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.platform.macos.JNIDecorationsMacOS;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public final class MacOSDecorationsUtil {

    private static final Logger LOGGER = LogUtil.getLogger(MacOSDecorationsUtil.class);
    private static final String FULL_WINDOW_CONTENT_KEY = "apple.awt.fullWindowContent";
    private static final String TRANSPARENT_TITLE_BAR_KEY = "apple.awt.transparentTitleBar";

    static DecorationInformation installDecorations(final JRootPane rootPane,
            final boolean useColoredTitleBar) {
        if (rootPane == null) {
            return null;
        }
        Window window = SwingUtilities.getWindowAncestor(rootPane);
        long windowHandle = JNIDecorationsMacOS.getComponentPointer(window);
        if (windowHandle == 0) {
            return new DecorationInformation(0, false, false, false, rootPane, false, 0, 0);
        }
        // Removing the decorations may cause the window to be resized.
        // Hence, we save the current bounds to restore them later.
        Rectangle windowBounds = window.getBounds();
        LOGGER.fine("Installing decorations for window " + windowHandle
                + "(coloredTitleBar = " + useColoredTitleBar + ")");
        JNIDecorationsMacOS.retainWindow(windowHandle);
        boolean fullWindowContent = isFullWindowContentEnabled(rootPane);
        boolean transparentTitleBar = isTransparentTitleBarEnabled(rootPane);
        float titleFontSize = (float) JNIDecorationsMacOS.getTitleFontSize(windowHandle);
        int titleBarHeight = (int) JNIDecorationsMacOS.getTitleBarHeight(windowHandle);

        setFullSizeContent(windowHandle, useColoredTitleBar);

        boolean titleVisible = SystemInfo.isMacOSMojave && !UIManager.getBoolean("Theme.macos.useSwingTitleLabel");
        JNIDecorationsMacOS.setTitleEnabled(windowHandle, titleVisible);
        if (titleVisible) {
            boolean isDarkTheme = UIManager.getBoolean("Theme.dark");
            JNIDecorationsMacOS.setDarkTheme(windowHandle, isDarkTheme);
        }
        window.setBounds(windowBounds);
        return new DecorationInformation(
                windowHandle, fullWindowContent,
                transparentTitleBar, useColoredTitleBar,
                rootPane, titleVisible, titleBarHeight, titleFontSize);
    }

    private static void setFullSizeContent(final long windowHandle, final boolean enabled) {
        if (enabled) {
            JNIDecorationsMacOS.installDecorations(windowHandle);
        }
    }

    static void uninstallDecorations(final Window window, final DecorationInformation information) {
        if (information == null || information.windowHandle == 0) {
            return;
        }
        if (information.useColoredTitleBar) {
            // As for the case when removing decorations, adding them back will cause the window to
            // change it's size.
            Rectangle bounds = window.getBounds();
            JNIDecorationsMacOS.uninstallDecorations(information.windowHandle,
                    information.fullWindowContentEnabled,
                    information.transparentTitleBarEnabled);
            window.setBounds(bounds);
        }
        JNIDecorationsMacOS.setTitleEnabled(information.windowHandle, true);
        JNIDecorationsMacOS.releaseWindow(information.windowHandle);
    }

    private static boolean isFullWindowContentEnabled(final JRootPane rootPane) {
        return PropertyUtil.getBooleanProperty(rootPane, FULL_WINDOW_CONTENT_KEY);
    }

    private static boolean isTransparentTitleBarEnabled(final JRootPane rootPane) {
        return PropertyUtil.getBooleanProperty(rootPane, TRANSPARENT_TITLE_BAR_KEY);
    }
}
