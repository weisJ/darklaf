/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.components.tooltip;

import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;

public enum ToolTipStyle implements ToolTipConstants {
    BALLOON(true, false),
    PLAIN_BALLOON(false, false),
    PLAIN(false, true);

    protected final boolean supportsPointer;
    protected final boolean opqaue;

    ToolTipStyle(final boolean supportsPointer, final boolean opqaue) {
        this.supportsPointer = supportsPointer;
        this.opqaue = opqaue;
    }

    public static ToolTipStyle parse(final Object style) {
        if (style instanceof ToolTipStyle) return (ToolTipStyle) style;
        if (style == null) return null;
        String name = style.toString();
        if (VARIANT_PLAIN_BALLOON.equalsIgnoreCase(name) || ToolTipStyle.PLAIN_BALLOON.name().equalsIgnoreCase(name)) {
            return ToolTipStyle.PLAIN_BALLOON;
        }
        if (VARIANT_BALLOON.equalsIgnoreCase(name) || ToolTipStyle.BALLOON.name().equalsIgnoreCase(name)) {
            return ToolTipStyle.BALLOON;
        }
        if (VARIANT_PLAIN.equalsIgnoreCase(name) || ToolTipStyle.PLAIN.name().equalsIgnoreCase(name)) {
            return ToolTipStyle.PLAIN;
        }
        return null;
    }

    public boolean supportsPointer() {
        return supportsPointer;
    }

    public boolean isOpqaue() {
        return opqaue;
    }
}
