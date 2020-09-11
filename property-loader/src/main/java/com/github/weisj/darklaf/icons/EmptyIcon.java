/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;

import javax.swing.*;
import javax.swing.plaf.UIResource;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public final class EmptyIcon implements Icon, UIResource {
    private static final Map<Integer, Icon> cache = new HashMap<>();
    private final int width;
    private final int height;

    private EmptyIcon(final int width, final int height) {
        this.width = width;
        this.height = height;
    }

    public static Icon create(final Icon base) {
        return create(base.getIconWidth(), base.getIconHeight());
    }

    public static Icon create(final int width, final int height) {
        return width == height ? create(width) : new EmptyIcon(width, height);
    }

    public static Icon create(final int size) {
        Icon icon = cache.get(size);
        if (icon == null && size < 129) {
            cache.put(size, icon = new EmptyIcon(size, size));
        }
        return icon == null ? new EmptyIcon(size, size) : icon;
    }

    public void paintIcon(final Component component, final Graphics g, final int i, final int j) {}

    public int getIconWidth() {
        return this.width;
    }

    public int getIconHeight() {
        return this.height;
    }

    public int hashCode() {
        int sum = this.width + this.height;
        return sum * (sum + 1) / 2 + this.width;
    }

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
