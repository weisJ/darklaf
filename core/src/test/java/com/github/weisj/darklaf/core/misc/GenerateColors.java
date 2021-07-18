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
package com.github.weisj.darklaf.core.misc;

import java.awt.*;
import java.util.logging.Logger;

import com.github.weisj.darklaf.properties.color.DarkColorModelHSL;

/** Used to generate the color swatches for ColorChooser. */
public final class GenerateColors {

    public static void main(final String[] args) {
        int cols = 30;
        int rows = 15;
        StringBuilder builder = new StringBuilder("{");
        int r;
        int g;
        int b;
        for (int i = 1; i < rows + 1; i++) {
            r = g = b = (int) ((i - 1) * 255.0 / (rows - 1));
            builder.append(r).append(",").append(g).append(",").append(b).append(",");
            for (int j = 0; j < cols - 1; j++) {
                Color c = colorFromPos(j, i, rows + 1, cols);
                builder.append(c.getRed()).append(",").append(c.getGreen()).append(",").append(c.getBlue()).append(",");
            }
        }
        builder.append("};");
        Logger.getGlobal().info(builder.toString());
    }

    private static Color colorFromPos(final int x, final int y, final int height, final int width) {
        int h, s, l;

        l = (int) (100 * y / (double) height);
        s = 100;
        h = (int) (360 * ((double) x / width));
        return new DarkColorModelHSL().getColorFromValues(new int[] {h, s, l});
    }
}
