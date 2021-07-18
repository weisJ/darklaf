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
package com.github.weisj.darklaf.core.test;

import java.awt.Window;
import java.lang.reflect.InvocationTargetException;

import javax.swing.JWindow;
import javax.swing.SwingUtilities;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import com.github.weisj.darklaf.platform.windows.PointerUtil;
import com.github.weisj.darklaf.platform.windows.WindowsLibrary;
import com.sun.jna.Native;

class PointerUtilTest {

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void testCustomImplementationEqualsJNA() throws InterruptedException, InvocationTargetException {
        WindowsLibrary.get().updateLibrary();
        Assertions.assertTrue(WindowsLibrary.get().isLoaded());
        for (int i = 0; i < 100; i++) {
            Window frame = new JWindow();
            frame.setAutoRequestFocus(false);
            SwingUtilities.invokeAndWait(() -> frame.setVisible(true));
            Assertions.assertEquals(Native.getWindowID(frame), PointerUtil.getHWND(frame));
            SwingUtilities.invokeLater(() -> {
                frame.setVisible(false);
                frame.dispose();
            });
        }
    }

}
