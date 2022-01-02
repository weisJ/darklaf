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
package com.github.weisj.darklaf.ui.splitbutton;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.ui.button.ButtonConstants;
import com.github.weisj.darklaf.ui.button.DarkButtonBorder;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;

public class DarkSplitButtonBorder extends DarkButtonBorder {

    @Override
    protected void paintBorderlessBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        if (showSplit(c)) {
            paintDivider(c, g, x, y, width, height);
        }
        super.paintBorderlessBorder(c, g, x, y, width, height);
    }

    @Override
    protected void paintLineBorder(final Component c, final Graphics2D g2, final int arc, final boolean focus,
            final int bx, final int by, final int bw, final int bh) {
        if (showSplit(c)) {
            paintDivider(c, g2, bx, by, bw, bh);
        }
        super.paintLineBorder(c, g2, arc, focus, bx, by, bw, bh);
    }

    protected void paintDivider(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        if (!(c instanceof JSplitButton)) return;
        Component arrowButton = ((JSplitButton) c).getComponent(0);
        if (arrowButton == null) return;
        boolean ltr = c.getComponentOrientation().isLeftToRight();
        int splitPos = ltr ? arrowButton.getX() : arrowButton.getX() + arrowButton.getWidth() - 1;

        DarkSplitButtonUI ui = DarkUIUtil.getUIOfType(((JSplitButton) c).getUI(), DarkSplitButtonUI.class);
        if (ui != null && ui.getDrawOutline(c)) {
            boolean armed = ui.isArmedBorderless(ui.splitButton)
                    || (ui.useArrowButton() && ui.isArmedBorderless(ui.arrowButton));
            g.setColor(ui.getBorderlessOutline(armed));
        } else {
            g.setColor(getBorderColor(c, false));
        }

        g.fillRect(splitPos, y, 1, height);
    }

    protected boolean showSplit(final Component c) {
        boolean hasDefaultAction = c instanceof JSplitButton && ((JSplitButton) c).getActionCount() > 1;
        boolean borderless = ButtonConstants.isBorderlessVariant(c);
        if (!borderless) return hasDefaultAction;
        if (hasDefaultAction && ButtonConstants.isBorderlessVariant(c)) {
            DarkSplitButtonUI ui = DarkUIUtil.getUIOfType(((JSplitButton) c).getUI(), DarkSplitButtonUI.class);
            return ui != null && ui.isRolloverBorderless((AbstractButton) c);
        }
        return false;
    }
}
