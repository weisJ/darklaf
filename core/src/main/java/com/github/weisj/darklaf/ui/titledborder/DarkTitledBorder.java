/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.ui.titledborder;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.components.border.MutableLineBorder;

public class DarkTitledBorder extends MutableLineBorder implements UIResource {

    public DarkTitledBorder() {
        super(1, 0, 0, 0, null);
        setColor(UIManager.getColor("TitledBorder.borderColor"));
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        checkInsets(c);
        super.paintBorder(c, g, x, y, width, height);
    }

    @Override
    public Insets getBorderInsets(final Component c, final Insets insets) {
        checkInsets(c);
        return super.getBorderInsets(c, insets);
    }

    protected void checkInsets(final Component c) {
        setInsets(1, 0, 0, 0);
        if (c instanceof JComponent component) {
            Border border = component.getBorder();
            if (border instanceof TitledBorder) {
                String text = ((TitledBorder) border).getTitle();
                int pos = ((TitledBorder) border).getTitlePosition();
                boolean onTop = pos == TitledBorder.ABOVE_TOP || pos == TitledBorder.BELOW_TOP
                        || pos == TitledBorder.TOP || pos == TitledBorder.DEFAULT_POSITION;
                if (text == null || text.isEmpty() || !onTop) {
                    setInsets(1, 1, 1, 1);
                }
            }
        }
    }
}
