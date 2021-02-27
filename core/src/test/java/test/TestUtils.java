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
package test;

import java.lang.reflect.InvocationTargetException;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.*;

import org.junit.jupiter.api.Assertions;

final class TestUtils {

    private TestUtils() {}

    static void runOnSwingThreadNotThrowing(final Runnable action) {
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
}
