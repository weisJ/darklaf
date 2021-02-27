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
package com.github.weisj.darklaf.icons;

import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.kitfox.svg.app.beans.SVGIcon;

class SVGImageTest {

    @Test
    void testDeriveWithSameSize() {
        IconLoader loader = IconLoader.get(SVGImageTest.class);

        DarkSVGIcon icon = (DarkSVGIcon) loader.getIcon("svg_icon.svg", 16, 16);
        Assertions.assertSame(icon, icon.derive(16, 16));

        // Force load the icon.
        icon.getSVGIcon();

        icon.setDisplaySize(-1, -1);
        Assertions.assertSame(icon, loader.getIcon("svg_icon.svg", 16, 16));

        Assertions.assertSame(icon, icon.derive(16, 16));
    }

    @Test
    void testChangingSizeAffectsCache() {
        IconLoader loader = IconLoader.get(SVGImageTest.class);

        DarkSVGIcon icon = (DarkSVGIcon) loader.getIcon("svg_icon.svg", 16, 16);
        Assertions.assertSame(icon, loader.getIcon("svg_icon.svg", 16, 16));

        // Force load
        icon.getSVGIcon();
        Assertions.assertSame(icon, loader.getIcon("svg_icon.svg", 16, 16));

        icon.setDisplaySize(50, 50);
        Assertions.assertSame(icon, loader.getIcon("svg_icon.svg", 50, 50));

        icon.setDisplaySize(-1, -1);
        Assertions.assertNotSame(icon, loader.getIcon("svg_icon.svg", 50, 50));
        Assertions.assertSame(icon.getSVGIcon(),
                ((DarkSVGIcon) loader.getIcon("svg_icon.svg", 50, 50)).getSVGIcon());


        Assertions.assertEquals(16, icon.getIconWidth());
        Assertions.assertEquals(16, icon.getIconHeight());
    }

    @Test
    void testCache() {
        IconLoader loader = IconLoader.get(SVGImageTest.class);

        DarkSVGIcon icon = (DarkSVGIcon) loader.getIcon("svg_icon.svg");
        Assertions.assertEquals(16, icon.getIconWidth());
        Assertions.assertEquals(16, icon.getIconHeight());

        Set<SVGIcon> svgSet = new HashSet<>();
        for (int i = 0; i < 100; i++) {
            DarkSVGIcon svgIcon = (DarkSVGIcon) loader.getIcon("svg_icon.svg");
            svgSet.add(svgIcon.getSVGIcon());
        }
        Assertions.assertEquals(1, svgSet.size());

        svgSet.clear();
        for (int i = 0; i < 100; i++) {
            DarkSVGIcon icon2 = icon.derive(i, i);
            svgSet.add(icon2.getSVGIcon());
        }
        Assertions.assertEquals(1, svgSet.size());
    }
}
