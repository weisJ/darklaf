/*
 * MIT License
 *
 * Copyright (c) 2020-2023 Jannis Weis
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
package com.github.weisj.darklaf.ui;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.util.DarkUIUtil;

public final class WidgetPopupHelper {

    public static Rectangle getPopupBounds(final JComponent parent, final Dimension popupSize,
            final Dimension parentSize) {
        return getPopupBounds(parent, popupSize, parentSize, true, false);
    }

    public static Rectangle getPopupBounds(final JComponent parent, final Dimension popupSize,
            final Dimension parentSize,
            final boolean useParentSizeAsMinimum, final boolean alignOpposite) {
        Insets parentIns = parent.getInsets();

        // reduce the width of the popup by the insets so that the popup
        // is the same width as the parent.
        if (useParentSizeAsMinimum) {
            popupSize.width = Math.max(popupSize.width, parentSize.width - parentIns.left - parentIns.right);
        }
        // popupSize.width -= popupIns.left + popupIns.right;

        boolean ltr = parent.getComponentOrientation().isLeftToRight();
        if (alignOpposite) ltr = !ltr;

        int x = ltr ? parentIns.left : parentSize.width - parentIns.right - popupSize.width;

        return computePopupBounds(parent,
                new Rectangle(x, parentSize.height - parentIns.bottom, popupSize.width, popupSize.height));
    }

    private static Rectangle computePopupBounds(final JComponent parent,
            final Rectangle popupTargetBounds) {
        Rectangle screenBounds = DarkUIUtil.getScreenBounds(parent, null);

        Point pos = parent.getLocationOnScreen();

        Insets ins = parent.getInsets();
        popupTargetBounds.translate(pos.x, pos.y);

        adjustYCoordinate(screenBounds, pos, ins, popupTargetBounds);
        adjustXCoordinate(screenBounds, popupTargetBounds);
        popupTargetBounds.translate(-pos.x, -pos.y);

        return popupTargetBounds;
    }

    private static void adjustYCoordinate(final Rectangle screenBounds,
            final Point parentPosition, final Insets parentInsets, final Rectangle targetRect) {
        if (targetRect.y + targetRect.height > screenBounds.y + screenBounds.height) {
            // Popup would be covered by bottom screen edge.
            if (parentPosition.y - targetRect.height - parentInsets.top >= screenBounds.y) {
                // popup goes above
                targetRect.y = parentPosition.y + parentInsets.top - targetRect.height;
            } else {
                // popup will be vertically centered on screen
                targetRect.y = screenBounds.y + Math.max(0, (screenBounds.height - targetRect.height) / 2);
                targetRect.height = Math.min(screenBounds.height, targetRect.height);
            }
        }
    }

    private static void adjustXCoordinate(final Rectangle screenBounds, final Rectangle rect) {
        if (rect.x < screenBounds.x) {
            rect.width = Math.min(screenBounds.width, rect.width);
            rect.x = screenBounds.x;
        } else if (rect.x + rect.width > screenBounds.width + screenBounds.x) {
            rect.width = Math.min(screenBounds.width, rect.width);
            rect.x = screenBounds.x + screenBounds.width - rect.width;
        }
    }

}
