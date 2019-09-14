package com.weis.darklaf;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;
import java.awt.*;

/**
 * Manager for the Look and Feel.
 *
 * @author Jannis Weis
 * @since 2018
 */
public final class LafManager {

    private static Theme currentLaf = Theme.Dark;

    static void setCurrentLaf(final Theme currentLaf) {
        LafManager.currentLaf = currentLaf;
    }

    @Contract(pure = true)
    public static Theme getCurrentLafTheme() {
        return currentLaf;
    }

    /**
     * Set the LaF to one of the two defaults.
     *
     * @param theme The thume. See {@link Theme}.
     */
    public static void loadLaf(final Theme theme) {
        if (theme == Theme.Dark) {
            currentLaf = Theme.Dark;
            setTheme(DarkLaf.class.getCanonicalName());
        } else {
            throw new IllegalArgumentException(theme + " not supported!");
        }
    }

    private static void setTheme(final String loaf) {
        try {
            MetalLookAndFeel.setCurrentTheme(new DefaultMetalTheme());
            UIManager.setLookAndFeel(loaf);
            updateLaf();
        } catch (@NotNull final ClassNotFoundException
                                        | InstantiationException
                                        | IllegalAccessException
                                        | UnsupportedLookAndFeelException e) {
            e.printStackTrace();
        }
    }

    private static void updateLaf() {
        for (final Frame f : Frame.getFrames()) {
            updateLafRecursively(f);
        }
    }

    private static void updateLafRecursively(@NotNull final Window window) {
        for (final Window childWindow : window.getOwnedWindows()) {
            updateLafRecursively(childWindow);
        }
        SwingUtilities.updateComponentTreeUI(window);
    }

    public enum Theme {
        Dark
    }
}
