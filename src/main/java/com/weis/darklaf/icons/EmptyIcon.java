package com.weis.darklaf.icons;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;

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
