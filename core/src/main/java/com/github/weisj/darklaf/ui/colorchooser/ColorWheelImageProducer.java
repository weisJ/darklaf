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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;
import java.awt.image.ColorModel;
import java.awt.image.MemoryImageSource;

/**
 * @author pegov
 * @author Konstantin Bulenkov
 */
public class ColorWheelImageProducer extends MemoryImageSource {

    private final int[] myPixels;
    private final int myWidth;
    private final int myHeight;
    private final float myBrightness;

    private float[] myHues;
    private float[] mySat;
    private int[] myAlphas;

    public ColorWheelImageProducer(final int w, final int h, final float brightness) {
        super(w, h, null, 0, w);
        myPixels = new int[w * h];
        myWidth = w;
        myHeight = h;
        myBrightness = brightness;
        generateLookupTables();
        newPixels(myPixels, ColorModel.getRGBdefault(), 0, w);
        setAnimated(true);
        generateColorWheel();
    }

    private void generateLookupTables() {
        mySat = new float[myWidth * myHeight];
        myHues = new float[myWidth * myHeight];
        myAlphas = new int[myWidth * myHeight];
        float radius = getRadius();

        // blend is used to create a linear alpha gradient of two extra pixels
        float blend = (radius + 2f) / radius - 1f;

        // Center of the color wheel circle
        int cx = myWidth / 2;
        int cy = myHeight / 2;

        for (int x = 0; x < myWidth; x++) {
            int kx = x - cx; // cartesian coordinates of x
            int squarekx = kx * kx; // Square of cartesian x

            for (int y = 0; y < myHeight; y++) {
                int ky = cy - y; // cartesian coordinates of y

                int index = x + y * myWidth;
                mySat[index] = (float) Math.sqrt(squarekx + ky
                                                            * ky)
                               / radius;
                if (mySat[index] <= 1f) {
                    myAlphas[index] = 0xff000000;
                } else {
                    myAlphas[index] = (int) ((blend - Math.min(blend,
                                                               mySat[index] - 1f)) * 255 / blend) << 24;
                    mySat[index] = 1f;
                }
                if (myAlphas[index] != 0) {
                    myHues[index] = (float) (Math.atan2(ky, kx) / Math.PI / 2d);
                }
            }
        }
    }

    public void generateColorWheel() {
        for (int index = 0; index < myPixels.length; index++) {
            if (myAlphas[index] != 0) {
                myPixels[index] = myAlphas[index] | 0xffffff & Color.HSBtoRGB(myHues[index],
                                                                              mySat[index],
                                                                              myBrightness);
            }
        }
        newPixels();
    }

    public int getRadius() {
        return Math.min(myWidth, myHeight) / 2 - 2;
    }
}
