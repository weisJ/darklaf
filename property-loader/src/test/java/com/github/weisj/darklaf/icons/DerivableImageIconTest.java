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
 *
 */
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class DerivableImageIconTest {

    @Test
    public void testCache() {
        IconLoader loader = IconLoader.get(DerivableImageIconTest.class);
        DerivableImageIcon icon = (DerivableImageIcon) loader.getIcon("image_icon.png");
        Set<Image> imageSet = new HashSet<>();
        for (int i = 0; i < 100; i++) {
            Image img = ((DerivableImageIcon) loader.getIcon("image_icon.png", 100, 100)).getImage();
            imageSet.add(img);
        }
        Assertions.assertEquals(1, imageSet.size());

        imageSet.clear();
        icon = icon.derive(50, 50);
        for (int i = 0; i < 100; i++) {
            icon = icon.derive(50, 50);
            imageSet.add(icon.getImage());
        }
        Assertions.assertEquals(1, imageSet.size());
    }
}
