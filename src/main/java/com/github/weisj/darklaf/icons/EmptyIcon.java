/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class EmptyIcon implements Icon, UIResource {
    private static final Map<Integer, Icon> cache = new HashMap<>();
    private final int width;
    private final int height;

    @Contract(pure = true)
    private EmptyIcon(final int width, final int height) {
        this.width = width;
        this.height = height;
    }

    @NotNull
    public static Icon create(@NotNull final Icon base) {
        return create(base.getIconWidth(), base.getIconHeight());
    }

    @NotNull
    public static Icon create(final int width, final int height) {
        return width == height ? create(width) : new EmptyIcon(width, height);
    }

    @NotNull
    public static Icon create(final int size) {
        Icon icon = cache.get(size);
        if (icon == null && size < 129) {
            cache.put(size, icon = new EmptyIcon(size, size));
        }
        return icon == null ? new EmptyIcon(size, size) : icon;
    }

    public void paintIcon(final Component component, final Graphics g, final int i, final int j) {
    }

    @Contract(pure = true)
    public int getIconWidth() {
        return this.width;
    }

    @Contract(pure = true)
    public int getIconHeight() {
        return this.height;
    }

    @Contract(pure = true)
    public int hashCode() {
        int sum = this.width + this.height;
        return sum * (sum + 1) / 2 + this.width;
    }

    @Contract(value = "null -> false", pure = true)
    public boolean equals(final Object o) {
        if (this == o) {
            return true;
        } else if (!(o instanceof EmptyIcon)) {
            return false;
        } else {
            EmptyIcon icon = (EmptyIcon) o;
            return this.height == icon.height && this.width == icon.width;
        }
    }
}
