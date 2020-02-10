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
package com.github.weisj.darklaf.icons;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.util.logging.Logger;

/**
 * @author Jannis Weis
 */
public abstract class LazyIcon implements Icon, UIResource {

    private static final Logger LOGGER = Logger.getLogger(LazyIcon.class.getName());
    protected final String path;
    protected final IconLoader.IconKey key;
    protected final Class<?> parentClass;
    private boolean loaded;
    private Icon icon;

    @Contract(pure = true)
    public LazyIcon(final String path, final IconLoader.IconKey key, final Class<?> parentClass) {
        this.path = path;
        this.key = key;
        this.parentClass = parentClass;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureLoaded();
        icon.paintIcon(c, g, x, y);
    }

    private void ensureLoaded() {
        if (!loaded) {
            LOGGER.info("Loading icon '" + path + "'. Resolving from " + parentClass);
            icon = loadIcon();
            loaded = true;
            if (icon == null) {
                throw new IllegalStateException("Could not load icon '" + path + "'");
            }
            key.w = icon.getIconWidth();
            key.h = icon.getIconHeight();
        }
    }

    protected abstract Icon loadIcon();

    @Override
    public int getIconWidth() {
        ensureLoaded();
        return icon.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        ensureLoaded();
        return icon.getIconHeight();
    }
}
