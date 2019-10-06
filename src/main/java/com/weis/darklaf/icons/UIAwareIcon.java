package com.weis.darklaf.icons;

import com.weis.darklaf.LafManager;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.io.Serializable;

/**
 * Icon that is aware of the current ui theme and adjusts the icon accordingly. Icons are loaded
 * lazily at their point of usage.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class UIAwareIcon implements Icon, UIResource, Serializable {

    private final UIAwareIcon dual;
    private final String darkKey;
    private final String lightKey;
    private final int w;
    private final int h;
    private final Class<?> parentClass;
    private LafManager.Theme currentTheme;
    private transient boolean loaded;
    private transient Icon icon;

    /**
     * Create new ui aware icon.
     *
     * @param darkKey     key to load icon for dark mode.
     * @param lightKey    key to load icon for light mode.
     * @param w           width of icon.
     * @param h           height of icon.
     * @param parentClass the class to resolve the path while lazy loading.
     */
    @Contract(pure = true)
    public UIAwareIcon(final String darkKey, final String lightKey, final int w, final int h,
                       final Class<?> parentClass) {
        this.darkKey = darkKey;
        this.lightKey = lightKey;
        this.w = w;
        this.h = h;
        this.parentClass = parentClass;
        this.dual = new UIAwareIcon(this);
    }

    @Contract(pure = true)
    private UIAwareIcon(@NotNull final UIAwareIcon dual) {
        this.darkKey = dual.lightKey;
        this.lightKey = dual.darkKey;
        this.dual = dual;
        this.w = dual.w;
        this.h = dual.h;
        this.parentClass = dual.parentClass;
    }

    public void paintIcon(final Component c, @NotNull final Graphics g2,
                          final int x, final int y, final double scale) {
        ensureLoaded();
        Graphics2D g = (Graphics2D) g2.create();
        g.translate(x, y);
        g.scale(scale, scale);
        icon.paintIcon(c, g, 0, 0);
        g2.dispose();
    }

    private void ensureLoaded() {
        if (!isLoaded()) {
            loadIcon();
        }
    }

    @Contract(pure = true)
    private boolean isLoaded() {
        return loaded && LafManager.getCurrentLafTheme().equals(currentTheme);
    }

    private void loadIcon() {
        currentTheme = LafManager.getCurrentLafTheme();
        if (currentTheme.equals(LafManager.Theme.Dark)) {
            icon = IconLoader.get(parentClass).getIcon(darkKey, w, h);
        } else {
            icon = IconLoader.get(parentClass).getIcon(lightKey, w, h);
        }
        loaded = true;
    }

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

    public UIAwareIcon getDual() {
        return dual;
    }
}
