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
package com.github.weisj.darklaf.ui.togglebutton.checkbox;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

/** @author Jannis Weis */
public class DarkCheckBoxBorder extends EmptyBorder implements UIResource, ToggleButtonConstants,
        VisualPaddingProvider {

    private final Insets visualPadding = UIManager.getInsets("CheckBox.visualInsets");

    public DarkCheckBoxBorder() {
        super(UIManager.getInsets("CheckBox.borderInsets"));
    }

    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        if (ToggleButtonConstants.isInCell(c)) {
            insets.set(0, 0, 0, 0);
            return insets;
        }
        return super.getBorderInsets(c, insets);
    }

    @Override
    public Insets getVisualPaddings(final Component c) {
        return DarkUIUtil.addInsets(getBorderInsets(), visualPadding);
    }
}
