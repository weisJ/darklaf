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
package com.github.weisj.darklaf.properties.icons;

import java.awt.*;

import javax.swing.*;

/**
 * Default base implementation for {@link UIAwareIcon}. The specific loading mechanism needs to be
 * implemented in the subclass.
 */
public abstract class DefaultUIAwareIcon implements UIAwareIcon {

    private final UIAwareIcon dual;

    protected Icon light;
    protected Icon dark;

    public DefaultUIAwareIcon() {
        this.dual = createDual();
    }

    protected abstract UIAwareIcon createDual();

    protected DefaultUIAwareIcon(final DefaultUIAwareIcon dual) {
        this.dark = dual.light;
        this.light = dual.dark;
        this.dual = dual;
    }


    @Override
    public UIAwareIcon getDual() {
        return dual;
    }

    private boolean isDark() {
        return IconLoader.getAwareStyle() == AwareIconStyle.DARK;
    }

    private Icon getEffectiveIcon() {
        return isDark() ? getDarkIcon() : getLightIcon();
    }

    /**
     * Load the light version of the icon. This method should never return a null value.
     *
     * @return the light icon.
     */
    protected abstract Icon loadLightIcon();

    /**
     * Load the dark version of the icon. This method should never return a null value.
     *
     * @return the dark icon.
     */
    protected abstract Icon loadDarkIcon();

    private Icon getLightIcon() {
        if (light == null) {
            light = loadLightIcon();
        }
        return light;
    }

    private Icon getDarkIcon() {
        if (dark == null) {
            dark = loadDarkIcon();
        }
        return dark;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        getEffectiveIcon().paintIcon(c, g, x, y);
    }


    @Override
    public int getIconWidth() {
        return getEffectiveIcon().getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return getEffectiveIcon().getIconHeight();
    }
}
