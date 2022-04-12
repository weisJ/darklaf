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
import java.util.Collections;
import java.util.List;

import javax.swing.*;

import com.github.weisj.darklaf.platform.CustomTitlePane;
import com.github.weisj.darklaf.platform.DecorationsProvider;
import com.github.weisj.darklaf.platform.TitlePaneLayoutInfo;

class DefaultDecorationsProvider implements DecorationsProvider {

    @Override
    public CustomTitlePane createTitlePane(final JRootPane rootPane, final int decorationStyle, final Window window) {
        return createNoOPTitlePane();
    }

    static CustomTitlePane createNoOPTitlePane() {
        return new CustomTitlePane() {
            @Override
            public void uninstall(final boolean removeDecorations) {}

            @Override
            public void install() {}
        };
    }

    @Override
    public boolean isCustomDecorationSupported() {
        return false;
    }

    @Override
    public void initialize() {}

    @Override
    public List<String> getPropertyResourcePaths() {
        return Collections.emptyList();
    }

    @Override
    public TitlePaneLayoutInfo titlePaneLayoutInfo(final CustomTitlePane customTitlePane) {
        return new TitlePaneLayoutInfo(new Rectangle());
    }
}
