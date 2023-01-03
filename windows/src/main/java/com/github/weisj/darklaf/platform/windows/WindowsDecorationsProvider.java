/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.platform.windows;

import java.awt.*;
import java.util.ArrayList;
import java.util.List;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.platform.*;
import com.github.weisj.darklaf.platform.windows.ui.WindowsTitlePane;
import com.github.weisj.darklaf.util.PropertyUtil;

public class WindowsDecorationsProvider implements DecorationsProvider {

    public WindowsDecorationsProvider() throws UnsupportedProviderException {
        if (!SystemInfo.isWindows10OrGreater)
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
        PointerUtil.WindowPointer hwnd = PointerUtil.getHWND(window);
        if (hwnd.isValid()) {

            JNIDecorationsWindows.installPopupMenuDecorations(hwnd.value(),
                    useSmallCornerRadiusForWindow(window));
            if (window instanceof RootPaneContainer) {
                JRootPane rootPane = ((RootPaneContainer) window).getRootPane();
                Color bg = rootPane != null ? rootPane.getBackground() : null;
                if (bg != null) {
                    JNIDecorationsWindows.setBackground(hwnd.value(), bg.getRed(), bg.getGreen(), bg.getBlue());
                }
            }
        }
    }

    private boolean useSmallCornerRadiusForWindow(final Window w) {
        if (!(w instanceof RootPaneContainer)) return false;
        return PropertyUtil.getBooleanProperty(((RootPaneContainer) w).getRootPane(),
                DecorationsConstants.KEY_WINDOWS_SMALL_CORNER_RADIUS, true);
    }

    @Override
    public void uninstallPopupWindow(final Window window) {
        if (window.isDisplayable()) {
            PointerUtil.WindowPointer hwnd = PointerUtil.getHWND(window);
            if (hwnd.isValid()) {
                JNIDecorationsWindows.uninstallDecorations(hwnd.value(), false);
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
    public TitlePaneLayoutInfo titlePaneLayoutInfo(final CustomTitlePane customTitlePane) {
        if (!(customTitlePane instanceof WindowsTitlePane)) throw new IllegalStateException();
        WindowsTitlePane titlePane = (WindowsTitlePane) customTitlePane;
        return new TitlePaneLayoutInfo(titlePane.windowButtonRect());
    }

    @Override
    public List<String> getPropertyResourcePaths() {
        ArrayList<String> properties = new ArrayList<>();
        properties.add("windows_icons");
        properties.add("windows_decorations");
        if (SystemInfo.isWindows11()) {
            properties.add("windows_11_decorations");
        }
        return properties;
    }
}
