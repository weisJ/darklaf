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
 *
 */
package com.github.weisj.darklaf.icons;

import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.util.LogUtil;

public abstract class AbstractLazyIconSupplier<T extends Icon> implements IconSupplier<T> {

    private static final Logger LOGGER = LogUtil.getLogger(AbstractLazyIconSupplier.class);
    protected final String path;
    protected final IconLoader.IconKey key;
    protected final Class<?> parentClass;
    private boolean loaded;
    private T icon;

    public AbstractLazyIconSupplier(final String path, final IconLoader.IconKey key, final Class<?> parentClass) {
        this.path = path;
        this.key = key;
        this.parentClass = parentClass;
    }

    private void ensureLoaded() {
        if (!loaded) {
            LOGGER.finer(() -> "Loading icon '" + path + "'. Resolving from " + parentClass);
            icon = loadIcon();
            loaded = true;
            if (icon == null) {
                throw new IllegalStateException("Could not load icon '" + path + "'");
            }
            key.w = icon.getIconWidth();
            key.h = icon.getIconHeight();
        }
    }

    protected abstract T loadIcon();

    @Override
    public T getIcon() {
        ensureLoaded();
        return icon;
    }
}
