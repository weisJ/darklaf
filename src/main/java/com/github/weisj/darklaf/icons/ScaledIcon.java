/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.icons;

import com.github.weisj.darklaf.util.Scale;
import org.jetbrains.annotations.Contract;

import javax.swing.*;
import java.awt.*;

public class ScaledIcon implements Icon {

    private Image img;

    @Contract(pure = true)
    public ScaledIcon(final Image img) {
        this.img = img;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g2, final int x, final int y) {
        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);
        g.scale(1.0 / Scale.SCALE_X, 1.0 / Scale.SCALE_Y);
        g.drawImage(img, 0, 0, img.getWidth(null), img.getHeight(null), null);
    }

    @Override
    public int getIconWidth() {
        return (int) (img.getWidth(null) / Scale.SCALE_X);
    }

    @Override
    public int getIconHeight() {
        return (int) (img.getHeight(null) / Scale.SCALE_Y);
    }
}
