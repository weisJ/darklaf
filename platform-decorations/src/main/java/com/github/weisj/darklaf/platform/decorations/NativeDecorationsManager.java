/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.decorations;

import java.awt.*;
import java.util.Properties;

import javax.swing.*;
import javax.swing.plaf.RootPaneUI;

import com.github.weisj.darklaf.platform.CustomTitlePane;
import com.github.weisj.darklaf.platform.DecorationsProvider;
import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.platform.TitlePaneLayoutInfo;
import com.github.weisj.darklaf.platform.macos.MacOSDecorationsProvider;
import com.github.weisj.darklaf.platform.windows.WindowsDecorationsProvider;
import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.properties.icons.IconLoader;

public class NativeDecorationsManager {

    private static NativeDecorationsManager sharedInstance;
    private DecorationsProvider decorationsProvider;
    private boolean decorationsEnabled = true;

    public static NativeDecorationsManager getSharedInstance() {
        if (sharedInstance == null) setSharedInstance(new NativeDecorationsManager());
        return sharedInstance;
    }

    public static void setSharedInstance(final NativeDecorationsManager handler) {
        NativeDecorationsManager.sharedInstance = handler;
    }

    public NativeDecorationsManager() {
        this(true);
    }


    public NativeDecorationsManager(final boolean allowNativeDecorations) {
        try {
            // Extend for different platforms.
            if (SystemInfo.isWindows && allowNativeDecorations) {
                // Decorations are in the Windows10 visuals. Disable for older version.
                decorationsProvider = new WindowsDecorationsProvider();
            } else if (SystemInfo.isMac && allowNativeDecorations) {
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
        return decorationsProvider.isCustomDecorationSupported() && decorationsEnabled;
    }

    public void initialize() {
        decorationsProvider.initialize();
    }

    public void loadDecorationProperties(final Properties uiProps, final UIDefaults defaults) {
        IconLoader iconLoader = IconLoader.get(decorationsProvider.getClass());
        for (String path : decorationsProvider.getPropertyResourcePaths()) {
            PropertyLoader.putProperties(
                    PropertyLoader.loadProperties(decorationsProvider.getClass(), path, ""),
                    uiProps, defaults, iconLoader);
        }
    }

    public void setDecorationsEnabled(final boolean enabled) {
        decorationsEnabled = enabled;
    }

    public void adjustContentArea(final JRootPane root, final Rectangle rect) {
        decorationsProvider.adjustContentArea(root, rect);
    }

    public boolean supportsNativeTitleText() {
        return decorationsProvider.supportsNativeTitleLabel();
    }

    public TitlePaneLayoutInfo titlePaneLayoutInfo(final RootPaneContainer frameOrDialog) {
        RootPaneUI ui = frameOrDialog.getRootPane().getUI();
        if (!(ui instanceof AbstractNativeDecorationsRootPaneUI)) throw new IllegalStateException();
        return decorationsProvider.titlePaneLayoutInfo(((AbstractNativeDecorationsRootPaneUI) ui).titlePane());
    }
}
