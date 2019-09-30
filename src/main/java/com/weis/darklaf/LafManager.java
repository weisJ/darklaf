package com.weis.darklaf;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.metal.DefaultMetalTheme;
import javax.swing.plaf.metal.MetalLookAndFeel;
import java.awt.*;
import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Level;
import java.util.logging.LogManager;
import java.util.logging.Logger;

/**
 * Manager for the Look and Feel.
 *
 * @author Jannis Weis
 * @since 2018
 */
public final class LafManager {

    static {
        setLogEnabled(true);
    }

    private static Theme currentLaf = Theme.Dark;

    static void setCurrentLaf(final Theme currentLaf) {
        LafManager.currentLaf = currentLaf;
    }

    @Contract(pure = true)
    public static Theme getCurrentLafTheme() {
        return currentLaf;
    }

    public static void setLogEnabled(final boolean logEnabled) {
        if (!logEnabled) {
            LogManager.getLogManager().reset();
        } else {
            try (InputStream inputStream = DarkLaf.class.getClassLoader()
                                                        .getResourceAsStream("logging.properties")) {
                if (inputStream != null) {
                    LogManager.getLogManager().readConfiguration(inputStream);
                }
            } catch (IOException e) {
                Logger.getGlobal().log(Level.SEVERE, "init logging system", e);
            }
        }
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
