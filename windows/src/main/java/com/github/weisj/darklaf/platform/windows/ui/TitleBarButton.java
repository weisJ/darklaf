/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
package com.github.weisj.darklaf.platform.windows.ui;

import java.awt.*;

import javax.swing.*;

class TitleBarButton extends JButton {

    private Color hoverColor;
    private Color clickColor;

    public TitleBarButton(final Action action) {
        super(action);
        setBorder(BorderFactory.createEmptyBorder());
        setBorderPainted(false);
    }

    @Override
    public Color getBackground() {
        if (getModel().isArmed()) {
            return clickColor;
        } else if (getModel().isRollover()) {
            return hoverColor;
        }
        return super.getBackground();
    }

    @Override
    public void paint(final Graphics g) {
        Icon icon = null;
        if (getModel().isArmed()) {
            g.setColor(clickColor);
            g.fillRect(0, 0, getWidth(), getHeight());
            icon = getPressedIcon();
        } else if (getModel().isRollover()) {
            g.setColor(hoverColor);
            g.fillRect(0, 0, getWidth(), getHeight());
            icon = getRolloverIcon();
        }
        if (icon == null) icon = getIcon();
        icon.paintIcon(this, g, (getWidth() - icon.getIconWidth()) / 2, (getHeight() - icon.getIconHeight()) / 2);
    }

    public void setHoverColor(Color hoverColor) {
        this.hoverColor = hoverColor;
        if (getModel().isRollover()) repaint();
    }

    public void setClickColor(Color clickColor) {
        this.clickColor = clickColor;
        if (getModel().isArmed()) repaint();
    }
}
