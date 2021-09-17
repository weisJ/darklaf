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
import java.util.Properties;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.platform.decorations.CustomTitlePane;
import com.github.weisj.darklaf.platform.decorations.DecorationsProvider;
import com.github.weisj.darklaf.platform.decorations.UnsupportedProviderException;
import com.github.weisj.darklaf.platform.windows.ui.WindowsTitlePane;
import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.properties.icons.IconLoader;
import com.github.weisj.darklaf.util.SystemInfo;

public class WindowsDecorationsProvider implements DecorationsProvider {

    public WindowsDecorationsProvider() throws UnsupportedProviderException {
        if (!SystemInfo.isWindows10)
            throw new UnsupportedProviderException("Only supported on Windows 10 or later");
        if (!WindowsLibrary.get().canLoad())
            throw new UnsupportedProviderException("Native components aren't supported");
    }

    @Override
    public CustomTitlePane createTitlePane(final JRootPane rootPane, final int decorationStyle, final Window window) {
        return new WindowsTitlePane(rootPane, decorationStyle, window);
    }

    @Override
    public boolean isCustomDecorationSupported() {
        return WindowsLibrary.get().isLoaded();
    }

    @Override
    public void initialize() {
        WindowsLibrary.get().updateLibrary();
    }

    @Override
    public void installPopupWindow(final Window window) {
        if (!window.isDisplayable()) {
            window.addNotify();
        }
        long hwnd = PointerUtil.getHWND(window);
        if (hwnd > 0) {
            JNIDecorationsWindows.installPopupMenuDecorations(hwnd);
            if (window instanceof RootPaneContainer) {
                JRootPane rootPane = ((RootPaneContainer) window).getRootPane();
                Color bg = rootPane != null ? rootPane.getBackground() : null;
                if (bg != null) {
                    JNIDecorationsWindows.setBackground(hwnd, bg.getRed(), bg.getGreen(), bg.getBlue());
                }
            }
        }
    }

    @Override
    public void uninstallPopupWindow(final Window window) {
        if (window.isDisplayable()) {
            long hwnd = PointerUtil.getHWND(window);
            if (hwnd != 0) {
                JNIDecorationsWindows.uninstallDecorations(hwnd, false);
            }
            window.dispose();
        }
    }

    @Override
    public void adjustContentArea(final JRootPane root, final Rectangle rect) {
        Border border = root.getBorder();
        if (border != null) {
            Insets ins = border.getBorderInsets(root);
            rect.x -= ins.left;
            rect.y -= ins.top;
        }
    }

    @Override
    public void loadDecorationProperties(final Properties properties, final UIDefaults currentDefaults) {
        IconLoader iconLoader = IconLoader.get(WindowsDecorationsProvider.class);
        PropertyLoader.putProperties(
                PropertyLoader.loadProperties(WindowsDecorationsProvider.class, "windows_decorations", ""), properties,
                currentDefaults, iconLoader);
        PropertyLoader.putProperties(
                PropertyLoader.loadProperties(WindowsDecorationsProvider.class, "windows_icons", ""), properties,
                currentDefaults, iconLoader);
    }
}
