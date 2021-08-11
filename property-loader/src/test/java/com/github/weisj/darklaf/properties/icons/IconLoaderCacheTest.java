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
package com.github.weisj.darklaf.properties.icons;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Supplier;

import javax.swing.Icon;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;


@Timeout(value = 5)
class IconLoaderCacheTest {

    @Test
    void testClearCacheClearsCache() {
        IconLoader loader = IconLoader.get(IconLoaderCacheTest.class);
        Icon icon1 = loader.getIcon("image_icon.png");
        Assertions.assertSame(icon1, loader.getIcon("image_icon.png"));
        loader.clearCache();
        Icon icon2 = loader.getIcon("image_icon.png");
        Assertions.assertNotSame(icon1, icon2);
    }

    @Test
    void cacheReleasesImageIcon() {
        testCacheIsReleasedForIcon(500, "image_icon.png");
    }

    @Test
    void cacheReleasesSVGIcon() {
        testCacheIsReleasedForIcon(600, "svg_icon.svg");
    }

    private void testCacheIsReleasedForIcon(final int count, final String iconName) {
        IconLoader loader = IconLoader.get(IconLoaderCacheTest.class);
        Set<Icon> hardReferences = new HashSet<>();
        for (int i = 0; i < count; i++) {
            Icon icon = loader.getIcon(iconName, i, i);
            hardReferences.add(icon);
        }
        Assertions.assertEquals(count, hardReferences.size());
        Assertions.assertEquals(count, loader.cacheSize());

        hardReferences.clear();

        waitForGarbageCollection(() -> !loader.isCacheEmpty());
    }

    @SuppressWarnings({"unused"})
    private void waitForGarbageCollection(final Supplier<Boolean> checker) {
        while (checker.get()) {
            Object object = null;
            try {
                object = new int[10][10][10][10][10][10][10][10][10][10][10][10];
            } catch (OutOfMemoryError ignored) {
            }
            System.out.println(object);
            System.gc();
        }
    }
}
