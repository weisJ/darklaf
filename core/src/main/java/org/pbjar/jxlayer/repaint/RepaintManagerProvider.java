/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package org.pbjar.jxlayer.repaint;

import javax.swing.*;

/**
 * To be implemented by classes that provide for a custom RepaintManager.
 *
 * @author Piet Blok
 * @see RepaintManagerUtils
 */
public interface RepaintManagerProvider {
    /**
     * Get the class of a {@link RepaintManager} that extends {@link WrappedRepaintManager}.
     * <p>
     * <b>Note:</b> the class must provide for a public constructor that takes a delegate
     * {@link RepaintManager} as its only argument.
     *
     * @return a class object
     */
    RepaintManager createWrappedRepaintManager(final RepaintManager delegate);

    /**
     * Checks whether or not the argument class is a {@link RepaintManager} class that will do the
     * required job.
     *
     * @param rpm a {@link RepaintManager}
     * @return {@code true} if the argument class will do the required job, {@code false} otherwise
     */
    boolean isAdequate(final RepaintManager rpm);
}
