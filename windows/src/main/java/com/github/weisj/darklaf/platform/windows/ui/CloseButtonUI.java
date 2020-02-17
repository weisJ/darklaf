/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.github.weisj.darklaf.platform.windows.ui;

import javax.swing.*;
import javax.swing.plaf.basic.BasicButtonUI;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class CloseButtonUI extends BasicButtonUI {

    protected static final Rectangle viewRect = new Rectangle();
    protected static final Rectangle textRect = new Rectangle();
    protected static final Rectangle iconRect = new Rectangle();

    protected Color closeHover;
    protected Color closeClick;

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        closeHover = UIManager.getColor("TitlePane.close.rollOverColor");
        closeClick = UIManager.getColor("TitlePane.close.clickColor");
    }

    protected String layout(final AbstractButton b, final JComponent c, final FontMetrics fm,
                            final int width, final int height) {
        Insets i = b.getInsets();
        viewRect.x = i.left;
        viewRect.y = i.top;
        viewRect.width = width - (i.right + viewRect.x);
        viewRect.height = height - (i.bottom + viewRect.y);

        textRect.x = textRect.y = textRect.width = textRect.height = 0;
        iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;

        // layout the text and icon
        return SwingUtilities.layoutCompoundLabel(
            b, fm, b.getText(), b.getIcon(),
            b.getVerticalAlignment(), b.getHorizontalAlignment(),
            b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
            viewRect, iconRect, textRect,
            b.getText() == null ? 0 : b.getIconTextGap());
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        g.setColor(getBackground((AbstractButton) c));
        g.fillRect(0, 0, c.getWidth(), c.getHeight());
        paintIcon(g, c, iconRect);
    }

    protected Color getBackground(final AbstractButton c) {
        return c.getModel().isArmed() ? closeClick : closeHover;
    }
}
