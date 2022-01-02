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
package com.github.weisj.darklaf.theme.event;

import com.github.weisj.darklaf.theme.Theme;

public class ThemeChangeEvent implements ThemeEvent {

    private final Theme oldTheme;
    private final Theme newTheme;

    public ThemeChangeEvent(final Theme oldTheme, final Theme newTheme) {
        this.oldTheme = oldTheme;
        this.newTheme = newTheme;
    }

    /**
     * Gets the old theme.
     *
     * @return the old theme.
     */
    public Theme getOldTheme() {
        return oldTheme;
    }

    /**
     * Gets the new theme.
     *
     * @return the new theme.
     */
    public Theme getNewTheme() {
        return newTheme;
    }

    @Override
    public String toString() {
        return "ThemeChangeEvent{" + "oldTheme=" + oldTheme + ", newTheme=" + newTheme + '}';
    }
}
