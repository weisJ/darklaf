/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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
package com.github.weisj.darklaf.components;


import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.ui.button.ButtonConstants;

public final class ComponentHelper {

    private ComponentHelper() {
        throw new IllegalStateException("Utility class");
    }

    public static JButton createIconOnlyButton(final Icon enabled) {
        return createIconOnlyButton(enabled, null);
    }

    public static JButton createIconOnlyButton(final Icon enabled, final Icon disabled) {
        JButton button = new JButton();
        setup(button, enabled, disabled);
        return button;
    }

    public static JSplitButton createIconOnlySplitButton(final Icon enabled) {
        return createIconOnlySplitButton(enabled, null);
    }

    public static JSplitButton createIconOnlySplitButton(final Icon enabled, final Icon disabled) {
        JSplitButton button = new JSplitButton();
        setup(button, enabled, disabled);
        return button;
    }

    public static <T extends AbstractButton> T createIconOnlyButton(final T button, final Icon enabled,
            final Icon disabled) {
        setup(button, enabled, disabled);
        return button;
    }

    private static void setup(final AbstractButton b, final Icon enabled, final Icon disabled) {
        b.setIcon(enabled);
        b.setDisabledIcon(disabled);
        b.putClientProperty(ButtonConstants.KEY_VARIANT, ButtonConstants.VARIANT_BORDERLESS);
        b.putClientProperty(ButtonConstants.KEY_SQUARE, true);
        b.putClientProperty(ButtonConstants.KEY_THIN, true);
        b.setFocusable(false);
        b.setFocusPainted(false);
        b.setBorderPainted(false);
    }
}
