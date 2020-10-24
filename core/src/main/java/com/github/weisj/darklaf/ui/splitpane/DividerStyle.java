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
package com.github.weisj.darklaf.ui.splitpane;

public enum DividerStyle {
    GRIP(SplitPaneConstants.STYLE_GRIP, false, true),
    GRIP_BORDERLESS(SplitPaneConstants.STYLE_GRIP_BORDERLESS, false, false),
    LINE(SplitPaneConstants.STYLE_LINE, true, true),
    INVISIBLE(SplitPaneConstants.STYLE_INVISIBLE, true, false);

    private final String name;
    private final boolean isThin;
    private final boolean paintBorder;

    DividerStyle(final String name, final boolean isThin, final boolean paintBorder) {
        this.name = name;
        this.isThin = isThin;
        this.paintBorder = paintBorder;
    }

    public static DividerStyle get(final Object style) {
        return get(style, GRIP);
    }

    public static DividerStyle get(final Object style, final DividerStyle fallback) {
        DividerStyle s = getNullableStyle(style);
        if (s == null) return fallback;
        return s;
    }

    public boolean isPaintBorder() {
        return paintBorder;
    }

    public boolean isThin() {
        return isThin;
    }

    public String getName() {
        return name;
    }

    @Override
    public String toString() {
        return getName();
    }

    static DividerStyle getNullableStyle(final Object obj) {
        if (obj == null) return null;
        if (obj instanceof DividerStyle) return (DividerStyle) obj;
        try {
            return valueOf(obj.toString());
        } catch (IllegalArgumentException ignored) {
        }
        for (DividerStyle s : values()) {
            if (s.name.equalsIgnoreCase(obj.toString())) return s;
        }
        return null;
    }
}
