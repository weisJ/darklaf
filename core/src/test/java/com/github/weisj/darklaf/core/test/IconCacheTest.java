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
 */
package com.github.weisj.darklaf.core.test;

import java.util.function.Supplier;

import javax.swing.UIDefaults;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.iconset.IconSet;
import com.github.weisj.darklaf.properties.icons.IconLoader;

@Timeout(value = 40)
@Disabled("Flaky")
class IconCacheTest implements NonThreadSafeTest {

    @Test
    void testIconCacheGetsReleased() {
        IconLoader iconLoader = (IconLoader) IconSet.iconLoader();
        iconLoader.clearCache();
        UIDefaults defaults = new DarkLaf().getDefaults();
        Assertions.assertFalse(iconLoader.isCacheEmpty());
        int size = iconLoader.cacheSize();
        defaults.clear();
        waitForGarbageCollection(() -> iconLoader.cacheSize() == size);
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
