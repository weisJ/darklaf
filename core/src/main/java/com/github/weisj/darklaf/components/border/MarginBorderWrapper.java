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
package com.github.weisj.darklaf.components.border;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicBorders;

import org.jetbrains.annotations.NotNull;

import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

public class MarginBorderWrapper extends CompoundBorder implements VisualPaddingProvider {

    public MarginBorderWrapper(final Border border) {
        super(border, new BasicBorders.MarginBorder());
    }

    public static void installBorder(final JComponent c) {
        Border border = c.getBorder();
        if (!(border instanceof MarginBorderWrapper)) {
            c.setBorder(wrapBorder(border));
        }
    }

    private static MarginBorderWrapper wrapBorder(final Border border) {
        if (border == null || border instanceof UIResource) {
            return new MarginBorderWrapper.UIBorder(border);
        } else {
            return new MarginBorderWrapper(border);
        }
    }

    public static void uninstallBorder(final JComponent c) {
        Border border = c.getBorder();
        if (border instanceof MarginBorderWrapper) {
            Border outside = ((MarginBorderWrapper) border).getOutsideBorder();
            c.setBorder(outside);
        }
    }

    public static Border getBorder(final JComponent c) {
        Border border = c.getBorder();
        if (border instanceof MarginBorderWrapper) {
            border = ((MarginBorderWrapper) border).getOutsideBorder();
        }
        return border;
    }

    @Override
    public @NotNull Insets getVisualPaddings(@NotNull Component component) {
        if (outsideBorder instanceof VisualPaddingProvider) {
            return ((VisualPaddingProvider) outsideBorder).getVisualPaddings(component);
        }
        return new Insets(0, 0, 0, 0);
    }

    public static class UIBorder extends MarginBorderWrapper implements UIResource {

        public UIBorder(final Border border) {
            super(border);
        }
    }
}
