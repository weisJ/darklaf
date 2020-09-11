/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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

import java.awt.*;
import java.lang.ref.WeakReference;

import javax.swing.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class IconUtilTest {

    @Test
    public void testWeakFrameCache() {
        if (GraphicsEnvironment.isHeadless()) return;
        Object obj = new Object();
        WeakReference<Object> ref = new WeakReference<>(obj);
        int count = 100;
        Icon icon = new TestIcon();
        JFrame[] frames = new JFrame[count];
        for (int i = 0; i < count; i++) {
            JFrame frame = new JFrame();
            frame.setIconImage(IconUtil.createFrameIcon(icon, frame));
            frames[i] = frame;
        }
        Assertions.assertEquals(count, IconUtil.getDynamicFrameIconCount());
        for (int i = 0; i < count; i++) {
            frames[i].dispose();
            frames[i] = null;
        }
        while (IconUtil.getDynamicFrameIconCount() != 0) {
            System.gc();
        }
        Assertions.assertEquals(0, IconUtil.getDynamicFrameIconCount());
    }

    private static class TestIcon implements DynamicIcon {

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {}

        @Override
        public int getIconWidth() {
            return 10;
        }

        @Override
        public int getIconHeight() {
            return 10;
        }
    }
}
