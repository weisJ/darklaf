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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;

import javax.swing.*;

/** @author Jannis Weis */
public class ColorPreviewComponent extends JComponent {
    protected final Color borderColor;
    private Color color;

    public ColorPreviewComponent() {
        borderColor = UIManager.getColor("ColorChooser.previewBorderColor");
    }

    public void setColor(final Color c) {
        color = c;
        repaint();
    }

    @Override
    protected void paintComponent(final Graphics g) {
        final Insets i = getInsets();
        final Rectangle r = getBounds();

        final int width = r.width - i.left - i.right;
        final int height = r.height - i.top - i.bottom;

        g.setColor(Color.WHITE);
        g.fillRect(i.left, i.top, width, height);

        g.setColor(color);
        g.fillRect(i.left + 1, i.top + 1, width - 2, height - 2);

        g.setColor(borderColor);
        g.fillRect(i.left, i.top, width, 1);
        g.fillRect(i.left, i.top, 1, height);
        g.fillRect(i.left + width - 1, i.top, 1, height);
        g.fillRect(i.left, i.top + height - 1, width, 1);
    }
}
