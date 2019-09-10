package com.weis.darklaf.icons;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;

public class LazyIcon implements Icon, UIResource {

    private boolean loaded;
    private final String path;
    private final IconLoader.IconKey key;
    private final Class<?> parentClass;
    private Icon icon;

    @Contract(pure = true)
    public LazyIcon(final String path, final IconLoader.IconKey key, final Class<?> parentClass) {
        this.path = path;
        this.key = key;
        this.parentClass = parentClass;
    }

    private void ensureLoaded() {
        if (!loaded) {
            icon = IconLoader.get(parentClass).createImageIcon(path, path);
            loaded = true;
            if (icon == null) {
                throw new IllegalStateException("Could not load icon '" + path + "'");
            }
            key.w = icon.getIconWidth();
            key.h = icon.getIconHeight();
        }
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureLoaded();
        icon.paintIcon(c,g,x,y);
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
