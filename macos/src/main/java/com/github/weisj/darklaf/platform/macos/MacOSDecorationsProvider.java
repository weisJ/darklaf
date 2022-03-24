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
package com.github.weisj.darklaf.platform.macos;

import java.awt.*;
import java.util.Collections;
import java.util.List;

import javax.swing.*;

import com.github.weisj.darklaf.platform.CustomTitlePane;
import com.github.weisj.darklaf.platform.DecorationsProvider;
import com.github.weisj.darklaf.platform.SystemInfo;
import com.github.weisj.darklaf.platform.UnsupportedProviderException;
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
}
