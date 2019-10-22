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
package com.github.weisj.darklaf;

import com.github.weisj.darklaf.theme.DarculaTheme;
import com.github.weisj.darklaf.theme.IntelliJTheme;
import com.github.weisj.darklaf.theme.Theme;
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
 */
public final class LafManager {

    private static Theme theme;

    static {
        /*
         * Disable for production.
         */
        enableLogging(false);
    }

    /**
     * Enable logging for the Look and Feel.
     *
     * @param logEnabled true if messages should be logged.
     */
    public static void enableLogging(final boolean logEnabled) {
        if (!logEnabled) {
            LogManager.getLogManager().reset();
        } else {
            try (InputStream inputStream = DarkLaf.class.getClassLoader()
                                                        .getResourceAsStream("com/github/weisj/darklaf/log/logging.properties")) {
                if (inputStream != null) {
                    LogManager.getLogManager().readConfiguration(inputStream);
                }
            } catch (IOException e) {
                Logger.getGlobal().log(Level.SEVERE, "init logging system", e);
            }
        }
    }

    /**
     * Get the current theme. This method will never return null even if the LaF isn#t currently installed.
     *
     * @return the current theme.
     */
    public static Theme getTheme() {
        if (theme == null) {
            theme = new IntelliJTheme();
        }
        return theme;
    }

    /**
     * Set the current theme.
     *
     * @param theme The theme to use.
     */
    public static void setTheme(final Theme theme) {
        LafManager.theme = theme;
    }


    /**
     * Sets the current theme and installs the LaF. If the LaF is already installed the theme is switched. This behaves
     * exactly like {@link #setTheme(Theme)} followed by {@link #install()}
     *
     * @param theme the theme to install.
     */
    public static void installTheme(final Theme theme) {
        setTheme(theme);
        install();
    }

    /**
     * Overload for {@link #installTheme(Theme)}.
     *
     * @param theme the theme to install.
     * @see #installTheme(Theme) installTheme().
     */
    public static void install(final Theme theme) {
        installTheme(theme);
    }

    /**
     * Install the current theme. If no theme is installed, the default is {@link DarculaTheme}. This sets the current
     * LaF and applies the given theme.
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

    /**
     * Update the component ui classes for all current windows.
     */
    public static void updateLaf() {
        for (final Window w : Window.getWindows()) {
            updateLafRecursively(w);
        }
    }

    private static void updateLafRecursively(@NotNull final Window window) {
        for (final Window childWindow : window.getOwnedWindows()) {
            updateLafRecursively(childWindow);
        }
        SwingUtilities.updateComponentTreeUI(window);
    }

}
