/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf;

import java.awt.Window;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.event.ThemeChangeEvent;
import com.github.weisj.darklaf.theme.event.ThemeChangeListener;
import com.github.weisj.darklaf.theme.event.ThemeEventSupport;
import com.github.weisj.darklaf.util.LogUtil;

final class LafInstaller {

    private static final Logger LOGGER = LogUtil.getLogger(LafManager.class);
    private static final AtomicBoolean isInstalling = new AtomicBoolean(false);
    private static final ThemeEventSupport<ThemeChangeEvent, ThemeChangeListener> eventSupport =
            new ThemeEventSupport<>();

    void install(final Theme theme) {
        if (!isInstalling.compareAndSet(false, true)) {
            throw new IllegalStateException("Can't install Laf while installation is in progress");
        }
        try {
            LOGGER.fine(() -> "Installing theme " + theme);
            LafTransition transition = LafTransition.showSnapshot();
            UIManager.setLookAndFeel(new DarkLaf(theme, false));
            updateLaf();
            SwingUtilities.invokeLater(transition::runTransition);
            notifyThemeInstalled(theme);
        } catch (final UnsupportedLookAndFeelException e) {
            LOGGER.log(Level.SEVERE, "Could not install LaF", e);
        } finally {
            isInstalling.set(false);
        }
    }

    void updateLaf() {
        for (final Window w : Window.getWindows()) {
            updateLafRecursively(w);
        }
    }

    private void updateLafRecursively(final Window window) {
        for (final Window childWindow : window.getOwnedWindows()) {
            updateLafRecursively(childWindow);
        }
        SwingUtilities.updateComponentTreeUI(window);
    }

    void notifyThemeInstalled(final Theme newTheme) {
        eventSupport.dispatchEvent(new ThemeChangeEvent(null, newTheme), ThemeChangeListener::themeInstalled);
    }

    void addThemeChangeListener(final ThemeChangeListener listener) {
        eventSupport.addListener(listener);
    }

    void removeThemeChangeListener(final ThemeChangeListener listener) {
        eventSupport.removeListener(listener);
    }


    void notifyThemeChanged(final Theme oldTheme, final Theme newTheme) {
        if (oldTheme != newTheme) {
            eventSupport.dispatchEvent(new ThemeChangeEvent(oldTheme, newTheme), ThemeChangeListener::themeChanged);
            LOGGER.fine(() -> "Setting theme to " + newTheme);
        }
    }
}
