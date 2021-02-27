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
package com.github.weisj.darklaf.icons;

import javax.swing.*;

public class LazyUIAwareIcon extends DefaultUIAwareIcon {

    private IconSupplier<Icon> lightSupplier;
    private IconSupplier<Icon> darkSupplier;

    public LazyUIAwareIcon(final IconSupplier<Icon> lightSupplier, final IconSupplier<Icon> darkSupplier) {
        this.lightSupplier = lightSupplier;
        this.darkSupplier = darkSupplier;
    }

    protected LazyUIAwareIcon(final LazyUIAwareIcon dual) {
        super(dual);
        this.lightSupplier = dual.darkSupplier;
        this.darkSupplier = dual.lightSupplier;
    }

    @Override
    protected UIAwareIcon createDual() {
        return new LazyUIAwareIcon(this);
    }

    protected Icon loadLightIcon() {
        Icon icon;
        if (lightSupplier != null) {
            icon = lightSupplier.getIcon();
            lightSupplier = null;
        } else {
            // Should never happen.
            icon = EmptyIcon.create(0);
        }
        return icon;
    }

    protected Icon loadDarkIcon() {
        Icon icon;
        if (darkSupplier != null) {
            icon = darkSupplier.getIcon();
            darkSupplier = null;
        } else {
            // Should never happen.
            icon = EmptyIcon.create(0);
        }
        return icon;
    }
}
