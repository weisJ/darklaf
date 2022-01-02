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
package com.github.weisj.darklaf.components.border;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.layout.LayoutHelper;

public final class DarkBorders {

    private static DarkLineBorder sharedBorderEmpty;
    private static DarkLineBorder sharedBorderT;
    private static DarkLineBorder sharedBorderL;
    private static DarkLineBorder sharedBorderB;
    private static DarkLineBorder sharedBorderR;
    private static DarkLineBorder sharedBorderTLBR;

    public static Border createTopBorder() {
        return createLineBorder(1, 0, 0, 0);
    }

    public static Border createLeftBorder() {
        return createLineBorder(0, 1, 0, 0);
    }

    public static Border createRightBorder() {
        return createLineBorder(0, 0, 0, 1);
    }

    public static Border createBottomBorder() {
        return createLineBorder(0, 0, 1, 0);
    }

    public static Border createTopBorderWithSpacing() {
        return BorderFactory.createCompoundBorder(createTopBorder(), LayoutHelper.createEmptyContainerBorder());
    }

    public static Border createLeftBorderWithSpacing() {
        return BorderFactory.createCompoundBorder(createLeftBorder(), LayoutHelper.createEmptyContainerBorder());
    }

    public static Border createRightBorderWithSpacing() {
        return BorderFactory.createCompoundBorder(createRightBorder(), LayoutHelper.createEmptyContainerBorder());
    }

    public static Border createBottomBorderWithSpacing() {
        return BorderFactory.createCompoundBorder(createBottomBorder(), LayoutHelper.createEmptyContainerBorder());
    }

    public static Border createLineBorder(final int top, final int left, final int bottom, final int right) {
        if (top == 0 && left == 0 && bottom == 0 && right == 0) {
            if (sharedBorderEmpty == null) sharedBorderEmpty = createDarkLineBorder(0, 0, 0, 0);
            return sharedBorderEmpty;
        }
        if (top == 1 && left == 0 && bottom == 0 && right == 0) {
            if (sharedBorderT == null) sharedBorderT = createDarkLineBorder(1, 0, 0, 0);
            return sharedBorderT;
        }
        if (top == 0 && left == 1 && bottom == 0 && right == 0) {
            if (sharedBorderL == null) sharedBorderL = createDarkLineBorder(0, 1, 0, 0);
            return sharedBorderL;
        }
        if (top == 0 && left == 0 && bottom == 1 && right == 0) {
            if (sharedBorderB == null) sharedBorderB = createDarkLineBorder(0, 0, 1, 0);
            return sharedBorderB;
        }
        if (top == 0 && left == 0 && bottom == 0 && right == 1) {
            if (sharedBorderR == null) sharedBorderR = createDarkLineBorder(0, 0, 0, 1);
            return sharedBorderR;
        }
        if (top == 1 && left == 1 && bottom == 1 && right == 1) {
            if (sharedBorderTLBR == null) sharedBorderTLBR = createDarkLineBorder(1, 1, 1, 1);
            return sharedBorderTLBR;
        }
        return createDarkLineBorder(top, left, bottom, right);
    }

    private static DarkLineBorder createDarkLineBorder(final int top, final int left, final int bottom,
            final int right) {
        return new DarkLineBorder(top, left, bottom, right, "border", Color::darker);
    }

    public static Border createWidgetLineBorder(final int top, final int left, final int bottom, final int right) {
        return new DarkLineBorder(top, left, bottom, right, "borderSecondary", Color::brighter);
    }
}
