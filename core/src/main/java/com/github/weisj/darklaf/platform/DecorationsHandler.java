/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.platform;

import java.awt.*;
import java.util.Properties;

import javax.swing.*;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.platform.decorations.CustomTitlePane;
import com.github.weisj.darklaf.platform.decorations.DecorationsProvider;
import com.github.weisj.darklaf.platform.macos.MacOSDecorationsProvider;
import com.github.weisj.darklaf.platform.windows.WindowsDecorationsProvider;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.SystemInfo;

public class DecorationsHandler {

    public static final String DECORATIONS_FLAG = DarkLaf.SYSTEM_PROPERTY_PREFIX + "decorations";
    private static DecorationsHandler sharedInstance;
    private DecorationsProvider decorationsProvider;
    private boolean decorationsEnabled = true;

    public static DecorationsHandler getSharedInstance() {
        if (sharedInstance == null) setSharedInstance(new DecorationsHandler());
        return sharedInstance;
    }

    public static void setSharedInstance(final DecorationsHandler handler) {
        DecorationsHandler.sharedInstance = handler;
    }

    protected DecorationsHandler() {
        try {
            // Extend for different platforms.
            boolean enableDecorations = isNativeDecorationsEnabled();
            if (SystemInfo.isWindows && enableDecorations) {
                // Decorations are in the Windows10 visuals. Disable for older version.
                decorationsProvider = new WindowsDecorationsProvider();
            } else if (SystemInfo.isMac && enableDecorations) {
                // Compiled binary needs at least macOS 10.10 (Yosemite).
                decorationsProvider = new MacOSDecorationsProvider();
            } else {
                decorationsProvider = new DefaultDecorationsProvider();
            }
        } catch (final Throwable e) {
            // If decorations modules are not available disable them.
            decorationsProvider = new DefaultDecorationsProvider();
        }
    }

    public CustomTitlePane createTitlePane(final JRootPane rootPane, final int decorationStyle, final Window window) {
        if (!decorationsProvider.isCustomDecorationSupported()) return DefaultDecorationsProvider.createNoOPTitlePane();
        return decorationsProvider.createTitlePane(rootPane, decorationStyle, window);
    }

    public void installPopupWindow(final Window window) {
        if (!decorationsProvider.isCustomDecorationSupported()) return;
        decorationsProvider.installPopupWindow(window);
    }

    public void uninstallPopupWindow(final Window window) {
        if (!decorationsProvider.isCustomDecorationSupported()) return;
        decorationsProvider.uninstallPopupWindow(window);
    }

    public boolean isCustomDecorationSupported() {
        return decorationsProvider.isCustomDecorationSupported() && decorationsEnabled
                && LafManager.getInstalledTheme().useCustomDecorations();
    }

    private boolean isNativeDecorationsEnabled() {
        return PropertyUtil.getSystemFlag(DECORATIONS_FLAG)
                && PropertyUtil.getSystemFlag(DarkLaf.ALLOW_NATIVE_CODE_FLAG);
    }

    public void initialize() {
        decorationsProvider.initialize();
    }

    public void loadDecorationProperties(final Properties uiProps, final UIDefaults defaults) {
        decorationsProvider.loadDecorationProperties(uiProps, defaults);
    }

    public void setDecorationsEnabled(final boolean enabled) {
        decorationsEnabled = enabled;
    }

    public void adjustContentArea(final JRootPane root, final Rectangle rect) {
        decorationsProvider.adjustContentArea(root, rect);
    }
}
