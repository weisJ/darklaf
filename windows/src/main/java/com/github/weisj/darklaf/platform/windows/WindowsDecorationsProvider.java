/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.github.weisj.darklaf.platform.windows;

import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.platform.windows.ui.DarkTitlePaneWindows;
import com.github.weisj.decorations.CustomTitlePane;
import com.github.weisj.decorations.DecorationsProvider;

import javax.swing.*;
import java.util.Properties;

public class WindowsDecorationsProvider implements DecorationsProvider {

    @Override
    public CustomTitlePane createTitlePane(final JRootPane rootPane) {
        return new DarkTitlePaneWindows(rootPane);
    }

    @Override
    public boolean isCustomDecorationSupported() {
        return JNIDecorationsWindows.isCustomDecorationSupported();
    }

    @Override
    public void initialize() {
        JNIDecorationsWindows.updateLibrary();
    }

    @Override
    public void loadDecorationProperties(final Properties properties, final UIDefaults currentDefaults) {
        IconLoader iconLoader = IconLoader.get(WindowsDecorationsProvider.class);
        PropertyLoader.putProperties(PropertyLoader.loadProperties(WindowsDecorationsProvider.class,
                                                                   "windows_decorations", ""),
                                     properties, currentDefaults, iconLoader);
        PropertyLoader.putProperties(PropertyLoader.loadProperties(WindowsDecorationsProvider.class,
                                                                   "windows_icons", ""),
                                     properties, currentDefaults, iconLoader);
    }
}
