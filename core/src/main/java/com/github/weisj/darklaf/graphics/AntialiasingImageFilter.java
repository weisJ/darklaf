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
package com.github.weisj.darklaf.graphics;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.RGBImageFilter;

public class AntialiasingImageFilter extends RGBImageFilter {

    private final BufferedImage destImg;
    private final int destPosX;
    private final int destPosY;
    private final int bgRgb;
    private final int bgRed;
    private final int bgGreen;
    private final int bgBlue;
    private final int fgRed;
    private final int fgGreen;
    private final int fgBlue;

    public AntialiasingImageFilter(final BufferedImage destImg, final int destPosX, final int destPosY, final Color fg,
            final Color bg) {
        this.destImg = destImg;
        this.destPosX = destPosX;
        this.destPosY = destPosY;
        this.bgRgb = bg.getRGB();
        this.bgRed = bg.getRed();
        this.bgBlue = bg.getBlue();
        this.bgGreen = bg.getGreen();
        this.fgRed = fg.getRed();
        this.fgBlue = fg.getBlue();
        this.fgGreen = fg.getGreen();
    }

    @Override
    public int filterRGB(final int x, final int y, final int rgb) {
        if (rgb == bgRgb) {
            return 0;
        }
        int red = (rgb & 0xff0000) >> 16;
        int green = (rgb & 0x00ff00) >> 8;
        int blue = rgb & 0x0000ff;
        float aa = (float) (red - bgRed) / (fgRed - bgRed);
        float ba = (float) (blue - bgBlue) / (fgBlue - bgBlue);
        float ga = (float) (green - bgGreen) / (fgGreen - bgGreen);

        if (destImg != null) {
            int destRgb = destImg.getRGB(destPosX + x, destPosY + y);
            int destRed = (destRgb & 0xff0000) >> 16;
            int destGreen = (destRgb & 0x00ff00) >> 8;
            int destBlue = destRgb & 0x0000ff;

            int outRed = Math.max(0, Math.min(255, (int) ((fgRed * aa) + (1 - aa) * destRed)));
            int outBlue = Math.max(0, Math.min(255, (int) ((fgBlue * ba) + (1 - ba) * destBlue)));
            int outGreen = Math.max(0, Math.min(255, (int) ((fgGreen * ga) + (1 - ga) * destGreen)));

            return (255 << 24) | (outRed << 16) | (outGreen << 8) | outBlue;
        } else {
            return ((int) (255 * ((aa + ga + ba) / 3)) << 24) | (fgRed << 16) | (fgGreen << 8) | fgBlue;
        }
    }
}
