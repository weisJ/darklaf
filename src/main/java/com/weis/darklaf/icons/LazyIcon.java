package com.weis.darklaf.icons;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public abstract class LazyIcon implements Icon, UIResource {

    private boolean loaded;
    private Icon icon;

    protected final String path;
    protected final IconLoader.IconKey key;
    protected final Class<?> parentClass;

    @Contract(pure = true)
    public LazyIcon(final String path, final IconLoader.IconKey key, final Class<?> parentClass) {
        this.path = path;
        this.key = key;
        this.parentClass = parentClass;
    }

    private void ensureLoaded() {
        if (!loaded) {
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
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureLoaded();
        icon.paintIcon(c, g, x, y);
    }

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
