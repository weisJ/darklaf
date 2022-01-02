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
package com.github.weisj.darklaf.properties.icons;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;

/**
 * Responsible for resolving icons.
 */
public interface IconResolver {

    @NotNull
    Icon getIcon(final @NotNull String path);

    @NotNull
    Icon getIcon(final @NotNull String path, final boolean themed);

    @NotNull
    Icon getIcon(final @NotNull String path, final int width, final int height);

    @NotNull
    Icon getIcon(final @NotNull String path, final int width, final int height, final boolean themed);

    @NotNull
    UIAwareIcon getUIAwareIcon(final @NotNull String path);

    @NotNull
    UIAwareIcon getUIAwareIcon(final @NotNull String path, final int width, final int height);
}
