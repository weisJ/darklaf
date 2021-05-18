/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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

import sun.swing.SwingUtilities2;

import com.github.weisj.darklaf.util.SwingUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/** @author Jannis Weis */
public class DarkPreviewPanel extends JPanel {

    private static final int SQUARE_SIZE = 28;
    private static final int SQUARE_GAP = 4;
    private static final int INNER_GAP = 4;
    private static final int SWATCH_WIDTH = 75;

    private static final int TEXT_GAP = 5;
    private String sampleText;

    private Color oldColor = null;

    public void paintComponent(final Graphics g) {
        if (oldColor == null) oldColor = getForeground();

        g.setColor(getBackground());
        g.fillRect(0, 0, getWidth(), getHeight());

        if (this.getComponentOrientation().isLeftToRight()) {
            int squareWidth = paintSquares(g, 0);
            int textWidth = paintText(g, squareWidth);
            paintSwatch(g, squareWidth + textWidth);
        } else {
            int swatchWidth = paintSwatch(g, 0);
            int textWidth = paintText(g, swatchWidth);
            paintSquares(g, swatchWidth + textWidth);
        }
    }

    public Dimension getPreferredSize() {
        JComponent host = getColorChooser();
        if (host == null) {
            host = this;
        }
        FontMetrics fm = host.getFontMetrics(getFont());

        int height = fm.getHeight();
        int width = SwingUtilities2.stringWidth(host, fm, getSampleText());

        int y = height * 3 + TEXT_GAP * 3;
        int x = SQUARE_SIZE * 3 + SQUARE_GAP * 2 + SWATCH_WIDTH + width + TEXT_GAP * 3;
        return new Dimension(x, y);
    }

    private int paintSquares(final Graphics g, final int offsetX) {
        Color color = getForeground();

        g.setColor(Color.white);
        g.fillRect(offsetX, 0, SQUARE_SIZE, SQUARE_SIZE);
        g.setColor(color);
        g.fillRect(offsetX + INNER_GAP, INNER_GAP, SQUARE_SIZE - (INNER_GAP * 2), SQUARE_SIZE - (INNER_GAP * 2));
        g.setColor(Color.white);
        g.fillRect(offsetX + INNER_GAP * 2, INNER_GAP * 2, SQUARE_SIZE - (INNER_GAP * 4),
                SQUARE_SIZE - (INNER_GAP * 4));

        g.setColor(color);
        g.fillRect(offsetX, SQUARE_SIZE + SQUARE_GAP, SQUARE_SIZE, SQUARE_SIZE);

        g.translate(SQUARE_SIZE + SQUARE_GAP, 0);
        g.setColor(Color.black);
        g.fillRect(offsetX, 0, SQUARE_SIZE, SQUARE_SIZE);
        g.setColor(color);
        g.fillRect(offsetX + INNER_GAP, INNER_GAP, SQUARE_SIZE - (INNER_GAP * 2), SQUARE_SIZE - (INNER_GAP * 2));
        g.setColor(Color.white);
        g.fillRect(offsetX + INNER_GAP * 2, INNER_GAP * 2, SQUARE_SIZE - (INNER_GAP * 4),
                SQUARE_SIZE - (INNER_GAP * 4));
        g.translate(-(SQUARE_SIZE + SQUARE_GAP), 0);

        g.translate(SQUARE_SIZE + SQUARE_GAP, SQUARE_SIZE + SQUARE_GAP);
        g.setColor(Color.white);
        g.fillRect(offsetX, 0, SQUARE_SIZE, SQUARE_SIZE);
        g.setColor(color);
        g.fillRect(offsetX + INNER_GAP, INNER_GAP, SQUARE_SIZE - (INNER_GAP * 2), SQUARE_SIZE - (INNER_GAP * 2));
        g.translate(-(SQUARE_SIZE + SQUARE_GAP), -(SQUARE_SIZE + SQUARE_GAP));

        g.translate((SQUARE_SIZE + SQUARE_GAP) * 2, 0);
        g.setColor(Color.white);
        g.fillRect(offsetX, 0, SQUARE_SIZE, SQUARE_SIZE);
        g.setColor(color);
        g.fillRect(offsetX + INNER_GAP, INNER_GAP, SQUARE_SIZE - (INNER_GAP * 2), SQUARE_SIZE - (INNER_GAP * 2));
        g.setColor(Color.black);
        g.fillRect(offsetX + INNER_GAP * 2, INNER_GAP * 2, SQUARE_SIZE - (INNER_GAP * 4),
                SQUARE_SIZE - (INNER_GAP * 4));
        g.translate(-((SQUARE_SIZE + SQUARE_GAP) * 2), 0);

        g.translate((SQUARE_SIZE + SQUARE_GAP) * 2, (SQUARE_SIZE + SQUARE_GAP));
        g.setColor(Color.black);
        g.fillRect(offsetX, 0, SQUARE_SIZE, SQUARE_SIZE);
        g.setColor(color);
        g.fillRect(offsetX + INNER_GAP, INNER_GAP, SQUARE_SIZE - (INNER_GAP * 2), SQUARE_SIZE - (INNER_GAP * 2));
        g.translate(-((SQUARE_SIZE + SQUARE_GAP) * 2), -(SQUARE_SIZE + SQUARE_GAP));

        return (SQUARE_SIZE * 3 + SQUARE_GAP * 2);
    }

    private int paintText(final Graphics g, final int offsetX) {
        GraphicsContext config = GraphicsUtil.setupAntialiasing(g);
        g.setFont(getFont());
        JComponent host = getColorChooser();
        if (host == null) {
            host = this;
        }
        FontMetrics fm = SwingUtil.getFontMetrics(host, g);

        int ascent = fm.getAscent();
        int height = fm.getHeight();
        int width = SwingUtilities2.stringWidth(host, fm, getSampleText());
        int textXOffset = offsetX + TEXT_GAP;

        Color color = getForeground();
        g.setColor(color);
        SwingUtil.drawString(host, g, getSampleText(), textXOffset + (TEXT_GAP / 2), ascent);

        g.fillRect(textXOffset, (height) + TEXT_GAP, width + (TEXT_GAP), height + 2);

        g.setColor(Color.black);
        SwingUtil.drawString(host, g, getSampleText(), textXOffset + (TEXT_GAP / 2),
                height + ascent + TEXT_GAP + 2);

        g.setColor(Color.white);

        g.fillRect(textXOffset, (height + TEXT_GAP) * 2, width + (TEXT_GAP), height + 2);

        g.setColor(color);
        SwingUtil.drawString(host, g, getSampleText(), textXOffset + (TEXT_GAP / 2),
                ((height + TEXT_GAP) * 2) + ascent + 2);

        config.restore();
        return width + TEXT_GAP * 3;
    }

    private int paintSwatch(final Graphics g, final int offsetX) {
        g.setColor(oldColor);
        g.fillRect(offsetX, 0, SWATCH_WIDTH, SQUARE_SIZE + SQUARE_GAP / 2);
        g.setColor(getForeground());
        g.fillRect(offsetX, SQUARE_SIZE + SQUARE_GAP / 2, SWATCH_WIDTH, SQUARE_SIZE + SQUARE_GAP / 2);
        return (offsetX + SWATCH_WIDTH);
    }

    private JColorChooser getColorChooser() {
        return (JColorChooser) SwingUtilities.getAncestorOfClass(JColorChooser.class, this);
    }

    private String getSampleText() {
        if (this.sampleText == null) {
            this.sampleText = UIManager.getString("ColorChooser.sampleText", getLocale());
        }
        return this.sampleText;
    }
}
