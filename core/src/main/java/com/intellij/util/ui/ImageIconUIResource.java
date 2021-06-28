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
package com.intellij.util.ui;

import java.awt.Image;

import javax.swing.ImageIcon;
import javax.swing.plaf.UIResource;

public class ImageIconUIResource extends ImageIcon implements UIResource {

    /**
     * Calls the superclass constructor with the same parameter.
     *
     * @param imageData an array of pixels
     * @see javax.swing.ImageIcon#ImageIcon(byte[])
     */
    public ImageIconUIResource(byte[] imageData) {
        super(imageData);
    }

    /**
     * Calls the superclass constructor with the same parameter.
     *
     * @param image an image
     * @see javax.swing.ImageIcon#ImageIcon(Image)
     */
    public ImageIconUIResource(Image image) {
        super(image);
    }
}
