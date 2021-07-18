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
package com.github.weisj.darklaf.core.test;

import java.awt.Window;
import java.awt.event.WindowEvent;
import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.SwingUtilities;

import com.github.weisj.darklaf.util.Lambdas;
import org.junit.jupiter.api.Assertions;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.IntelliJTheme;
import com.github.weisj.darklaf.theme.Theme;

final class TestUtils {

    private TestUtils() {}

    private static final Object lock = new Object();

    static void ensureLafInstalled() {
        ensureLafInstalled(new IntelliJTheme());
    }

    static void ensureLafInstalled(final Theme theme) {
        ensureLafInstalled(theme, false);
    }

    static void ensureLafInstalled(final Theme theme, final boolean alwaysInstall) {
        synchronized (lock) {
            if (alwaysInstall || !LafManager.isInstalled() || !LafManager.getInstalledTheme().equals(theme)) {
                runOnSwingThreadNotThrowing(() -> LafManager.install(theme));
            }
        }
    }

    static void runOnThreadNotThrowing(final Runnable action) {
        AtomicReference<Exception> exceptionRef = new AtomicReference<>();
        try {
            new Thread(() -> {
                try {
                    action.run();
                } catch (final Exception e) {
                    exceptionRef.set(e);
                }
            }).start();
        } catch (final Exception e) {
            e.printStackTrace();
            Assertions.fail(e.getMessage(), e);
        }
        if (exceptionRef.get() != null) {
            Assertions.fail(exceptionRef.get().getMessage(), exceptionRef.get());
        }
    }

    static void runOnSwingThreadNotThrowing(final Lambdas.CheckedRunnable<? extends Exception> action) {
        AtomicReference<Exception> exceptionRef = new AtomicReference<>();
        try {
            SwingUtilities.invokeAndWait(() -> {
                try {
                    action.run();
                } catch (final Exception e) {
                    exceptionRef.set(e);
                }
            });
        } catch (final InterruptedException e) {
            e.printStackTrace();
            Assertions.fail(e.getMessage(), e);
        } catch (final InvocationTargetException e) {
            e.getTargetException().printStackTrace();
            Throwable target = e.getTargetException();
            Assertions.fail(target.getMessage(), target);
        }
        if (exceptionRef.get() != null) {
            Assertions.fail(exceptionRef.get().getMessage(), exceptionRef.get());
        }
    }

    static void closeWindow(final Window window) {
        window.dispatchEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSING));
        window.dispose();
    }
}
