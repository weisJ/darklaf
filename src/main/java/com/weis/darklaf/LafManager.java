package com.weis.darklaf;

import com.weis.darklaf.theme.DarculaTheme;
import com.weis.darklaf.theme.Theme;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
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

    private static Theme theme;

    static {
        enableLogging(true);
    }

    public static void enableLogging(final boolean logEnabled) {
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

    public static Theme getTheme() {
        if (theme == null) {
            theme = new DarculaTheme();
        }
        return theme;
    }

    public static void setTheme(final Theme theme) {
        LafManager.theme = theme;
    }


    /**
     * Set the LaF to one of the two defaults.
     *
     * @param theme The theme to install. See {@link Theme}.
     */
    public static void installTheme(final Theme theme) {
        setTheme(theme);
        install();
    }

    /**
     * Install the current theme. If no theme is installed, the default is
     * {@link DarculaTheme}.
     */
    public static void install() {
        try {
            UIManager.setLookAndFeel(DarkLaf.class.getCanonicalName());
            updateLaf();
        } catch (@NotNull final ClassNotFoundException
                | InstantiationException
                | IllegalAccessException
                | UnsupportedLookAndFeelException e) {
            e.printStackTrace();
        }
    }

    public static void updateLaf() {
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

}
