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
package com.github.weisj.darklaf.swingdsl;

import java.awt.Insets;

import javax.swing.JComponent;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.swingdsl.laf.VisualPaddingProvider;

public final class VisualPaddingUtil {


    /**
     * Update the {@link VisualPaddingProvider#VISUAL_PADDING_PROP} property of the component based on
     * the components border.
     *
     * @param c the component.
     */
    public static void updateProperty(final JComponent c) {
        Border b = DarkUIUtil.getUnwrappedBorder(c);
        if (b instanceof VisualPaddingProvider) {
            updateProperty(c, ((VisualPaddingProvider) b).getVisualPaddings(c));
        } else {
            updateProperty(c, null);
        }
    }

    /**
     * Update the {@link VisualPaddingProvider#VISUAL_PADDING_PROP} property of the component based on
     * the given insets.
     *
     * @param c the component.
     * @param ins the padding insets. Null insets mean the property will be uninstalled.
     */
    public static void updateProperty(final JComponent c, final Insets ins) {
        if (ins != null) {
            PropertyUtil.installProperty(c, VisualPaddingProvider.VISUAL_PADDING_PROP,
                    new InsetsUIResource(ins.top, ins.left, ins.bottom, ins.right));
        } else {
            PropertyUtil.uninstallProperty(c, VisualPaddingProvider.VISUAL_PADDING_PROP);
        }
    }
}
