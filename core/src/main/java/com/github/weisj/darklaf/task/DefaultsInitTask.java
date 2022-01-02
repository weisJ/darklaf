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
package com.github.weisj.darklaf.task;

import javax.swing.*;

import com.github.weisj.darklaf.theme.Theme;

public interface DefaultsInitTask {

    /**
     * Execute the task. All values in the properties should be of type
     * {@link javax.swing.plaf.UIResource}. This is specifically important for
     * {@link javax.swing.plaf.FontUIResource} and {@link javax.swing.plaf.ColorUIResource}.
     *
     * @param currentTheme the current theme being initialized.
     * @param defaults the current defaults to work with.
     */
    void run(final Theme currentTheme, final UIDefaults defaults);

    /**
     * Indicated that this task should only be run if the laf is actually installed.
     *
     * @return true if task should only be run during installation of the LaF.
     */
    default boolean onlyDuringInstallation() {
        return false;
    }
}
