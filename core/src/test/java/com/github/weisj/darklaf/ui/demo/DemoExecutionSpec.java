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
package com.github.weisj.darklaf.ui.demo;

import java.awt.Dimension;
import java.awt.Window;
import java.util.Collections;
import java.util.List;

import javax.swing.Icon;
import javax.swing.JMenu;

import com.github.weisj.darklaf.theme.Theme;

public interface DemoExecutionSpec {

    default Theme getTheme() {
        return DemoExecutor.getPreferredTheme();
    }

    default boolean useDarklaf() {
        return true;
    }

    default Dimension getWindowSize() {
        return null;
    }

    default Icon getFrameIcon() {
        return null;
    }

    default List<JMenu> createMenus() {
        return Collections.emptyList();
    }

    default void configureWindow(final Window window) {}

    default boolean hasMenuBar() {
        return true;
    }
}
