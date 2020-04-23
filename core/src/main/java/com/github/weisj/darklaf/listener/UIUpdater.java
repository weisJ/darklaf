/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.listener;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.event.ThemeChangeEvent;
import com.github.weisj.darklaf.theme.event.ThemeChangeListener;

/**
 * Listener that updates the component ui after a new theme has been installed.
 * This listener only needs to be used with the top most component.
 * This listener isn't needed for components that are part of a visible ui hierarchy.
 * The listener has to be added with {@link LafManager#addThemeChangeListener(ThemeChangeListener)}
 */
public class UIUpdater implements ThemeChangeListener {

    /**
     * Creates and registers a new {@link UIUpdater} with the given component.
     *
     * @param component the component.
     */
    public static void registerComponent(final Component component) {
        LafManager.addThemeChangeListener(new UIUpdater(component));
    }

    private final Component component;

    /**
     * Creates a new ui updater for the given component. After a new theme has been installed
     * This listener only needs to be used with the top most component.
     * This listener isn't needed for components that are part of a visible ui hierarchy.
     * The listener has to be added with {@link LafManager#addThemeChangeListener(ThemeChangeListener)}
     *
     * @param component the component to update.
     */
    public UIUpdater(final Component component) {
        this.component = component;
    }

    @Override
    public void themeChanged(final ThemeChangeEvent e) {}

    @Override
    public void themeInstalled(final ThemeChangeEvent e) {
        if (component != null) SwingUtilities.updateComponentTreeUI(component);
    }
}
