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
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.awt.image.MultiResolutionImage;
import java.util.ArrayList;
import java.util.List;

public class IconUtil {

    public static BufferedImage getScaledIconImage(final List<Image> imageList, final int width, final int height) {
        if (width == 0 || height == 0) {
            return null;
        }
        java.util.List<Image> multiResAndNormalImages = new ArrayList<>(imageList.size());
        for (Image image : imageList) {
            if ((image instanceof MultiResolutionImage)) {
                Image im = ((MultiResolutionImage) image).getResolutionVariant(width, height);
                multiResAndNormalImages.add(im);
            } else {
                multiResAndNormalImages.add(image);
            }
        }
        Image bestImage = null;
        int bestWidth = 0;
        int bestHeight = 0;
        double bestSimilarity = 3; //Impossibly high value
        double bestScaleFactor = 0;
        for (Image im : multiResAndNormalImages) {
            //Iterate imageList looking for best matching image.
            //'Similarity' measure is defined as good scale factor and small insets.
            //The best possible similarity is 0 (no scale, no insets).
            //It's found while the experiments that good-looking result is achieved
            //with scale factors x1, x3/4, x2/3, xN, x1/N.
            if (im == null) {
                continue;
            }
            int iw;
            int ih;
            try {
                iw = im.getWidth(null);
                ih = im.getHeight(null);
            } catch (Exception e) {
                continue;
            }
            if (iw > 0 && ih > 0) {
                //Calc scale factor
                double scaleFactor = Math.min((double) width / (double) iw,
                                              (double) height / (double) ih);
                //Calculate scaled image dimensions
                //adjusting scale factor to nearest "good" value
                int adjw = 0;
                int adjh = 0;
                double scaleMeasure = 1; //0 - best (no) scale, 1 - impossibly bad
                if (scaleFactor >= 2) {
                    //Need to enlarge image more than twice
                    //Round down scale factor to multiply by integer value
                    scaleFactor = Math.floor(scaleFactor);
                    adjw = iw * (int) scaleFactor;
                    adjh = ih * (int) scaleFactor;
                    scaleMeasure = 1.0 - 0.5 / scaleFactor;
                } else if (scaleFactor >= 1) {
                    //Don't scale
                    scaleFactor = 1.0;
                    adjw = iw;
                    adjh = ih;
                    scaleMeasure = 0;
                } else if (scaleFactor >= 0.75) {
                    //Multiply by 3/4
                    scaleFactor = 0.75;
                    adjw = iw * 3 / 4;
                    adjh = ih * 3 / 4;
                    scaleMeasure = 0.3;
                } else if (scaleFactor >= 0.6666) {
                    //Multiply by 2/3
                    scaleFactor = 0.6666;
                    adjw = iw * 2 / 3;
                    adjh = ih * 2 / 3;
                    scaleMeasure = 0.33;
                } else {
                    //Multiply size by 1/scaleDivider
                    //where scaleDivider is minimum possible integer
                    //larger than 1/scaleFactor
                    double scaleDivider = Math.ceil(1.0 / scaleFactor);
                    scaleFactor = 1.0 / scaleDivider;
                    adjw = (int) Math.round((double) iw / scaleDivider);
                    adjh = (int) Math.round((double) ih / scaleDivider);
                    scaleMeasure = 1.0 - 1.0 / scaleDivider;
                }
                double similarity = ((double) width - (double) adjw) / (double) width +
                    ((double) height - (double) adjh) / (double) height + //Large padding is bad
                    scaleMeasure; //Large rescale is bad
                if (similarity < bestSimilarity) {
                    bestSimilarity = similarity;
                    bestScaleFactor = scaleFactor;
                    bestImage = im;
                    bestWidth = adjw;
                    bestHeight = adjh;
                }
                if (similarity == 0) break;
            }
        }
        if (bestImage == null) {
            //No images were found, possibly all are broken
            return null;
        }
        BufferedImage bimage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB);
        Graphics2D g = bimage.createGraphics();
        g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BILINEAR);
        try {
            int x = (width - bestWidth) / 2;
            int y = (height - bestHeight) / 2;
            g.drawImage(bestImage, x, y, bestWidth, bestHeight, null);
        } finally {
            g.dispose();
        }
        return bimage;
    }
}
