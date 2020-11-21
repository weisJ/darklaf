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
package com.github.weisj.darklaf.components;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.BorderUIResource;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class HoveringPanel extends JPanel {

    private Color color;
    private int arc;

    public HoveringPanel() {
        setLayout(new BorderLayout());
        setOpaque(false);
    }

    @Override
    protected void paintComponent(final Graphics g) {
        super.paintComponent(g);
        GraphicsContext context = new GraphicsContext(g);
        ((Graphics2D) g).setComposite(PaintUtil.getTransparentComposite());
        g.setColor(color != null ? color : getBackground());
        PaintUtil.fillRoundRect((Graphics2D) g, 0, 0, getWidth(), getHeight(), arc, false);
        context.restore();
    }

    @Override
    public void updateUI() {
        super.updateUI();
        arc = 2 * UIManager.getInt("arc");
        color = UIManager.getColor("hoverHighlight");
        int pad = 1 + UIManager.getInt("borderThickness");
        PropertyUtil.installBorder(this, new BorderUIResource.EmptyBorderUIResource(pad, pad, pad, pad));
    }
}
