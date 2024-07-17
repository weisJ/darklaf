/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
package com.github.weisj.darklaf.platform.macos;

import java.awt.*;
import java.util.Collections;
import java.util.List;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;

import com.github.weisj.darklaf.platform.*;
import com.github.weisj.darklaf.platform.macos.ui.MacOSDecorationsUtil;
import com.github.weisj.darklaf.platform.macos.ui.MacOSTitlePane;

public class MacOSDecorationsProvider implements DecorationsProvider {

    public MacOSDecorationsProvider() throws UnsupportedProviderException {
        if (!SystemInfo.isMacOSYosemite)
            throw new UnsupportedProviderException("Only macOS Yosemite or later is supported");
        if (!MacOSLibrary.get().canLoad())
            throw new UnsupportedProviderException("Can't load native library");
    }

    @Override
    public CustomTitlePane createTitlePane(final JRootPane rootPane, final int decorationStyle, final Window window) {
        return new MacOSTitlePane(rootPane, window);
    }

    @Override
    public boolean isCustomDecorationSupported() {
        return MacOSLibrary.get().isLoaded();
    }

    @Override
    public void initialize() {
        MacOSLibrary.get().updateLibrary();
    }

    @Override
    public List<String> getPropertyResourcePaths() {
        return Collections.singletonList("macos_decorations");
    }

    @Override
    public void installPopupWindow(final Window window) {
        if (!window.isDisplayable()) {
            window.addNotify();
        }
        if (window instanceof RootPaneContainer) {
            JRootPane rootPane = ((RootPaneContainer) window).getRootPane();
            Border border = getOutermostBorder((JComponent) rootPane.getContentPane());
            while (border instanceof CompoundBorder) {
                border = ((CompoundBorder) border).getOutsideBorder();
            }
            if (border instanceof RoundedFrame) {
                MacOSDecorationsUtil.installPopupWindow(window,
                        ((RoundedFrame) border).getRadius(),
                        ((RoundedFrame) border).getThickness(),
                        ((RoundedFrame) border).getColor().getRGB());
                window.repaint();
            }
        }
    }

    private static Border getOutermostBorder(final JComponent component) {
        JComponent c = component;
        while (c.getComponentCount() == 1 && c.getBorder() == null) {
            JComponent child = (JComponent) c.getComponent(0);
            if (child.getWidth() == c.getWidth() && child.getHeight() == c.getHeight()) {
                c = child;
            } else {
                break;
            }
        }
        return c.getBorder();
    }

    @Override
    public boolean supportsNativeTitleLabel() {
        return SystemInfo.isMacOSMojave;
    }

    @Override
    public TitlePaneLayoutInfo titlePaneLayoutInfo(final CustomTitlePane customTitlePane) {
        if (!(customTitlePane instanceof MacOSTitlePane)) throw new IllegalStateException();
        long hwnd = ((MacOSTitlePane) customTitlePane).windowHandle();
        if (hwnd == 0) {
            throw new IllegalStateException(
                    "Window isn't displayable (but has to in order to compute the title pane layout."
                            + " If you need this information before the window is displayed call `frame.addNotify()` to make it displayable.");
        }
        float[] b = JNIDecorationsMacOS.windowButtonRect(((MacOSTitlePane) customTitlePane).windowHandle());
        return new TitlePaneLayoutInfo(new Rectangle((int) b[0], (int) b[1], (int) b[2], (int) b[3]));
    }
}
