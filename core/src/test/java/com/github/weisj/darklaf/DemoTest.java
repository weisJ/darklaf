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
package com.github.weisj.darklaf;


import java.awt.Window;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.DemoLauncher;
import com.github.weisj.darklaf.util.LogUtil;

class DemoTest implements NonThreadSafeTest {

    private static final Logger LOGGER = LogUtil.getLogger(DemoTest.class);

    @Test
    void runningDemosDoesNotThrow() {
        LafManager.setLogLevel(Level.WARNING);
        for (Theme theme : LafManager.getRegisteredThemes()) {
            LOGGER.warning("Testing " + theme.getDisplayName());
            runDemos(theme);
            if (!Boolean.getBoolean("runAllThemes")) break;
        }
    }

    private void runDemos(final Theme theme) {
        List<DemoLauncher.DemoEntry> demos = new DemoLauncher().getDemoClasses();
        int count = demos.size();
        int index = 0;
        for (DemoLauncher.DemoEntry demo : demos) {
            TestUtils.ensureLafInstalled(theme);
            LOGGER.warning("Running: " + (++index) + "/" + count + " " + demo);
            if (demo.isDelicate()) {
                LOGGER.warning("Skipping");
                continue;
            }
            AtomicReference<Window> windowRef = demo.start(Level.WARNING);
            TestUtils.runOnSwingThreadNotThrowing(() -> {
                Assertions.assertNotNull(windowRef.get());
                TestUtils.closeWindow(windowRef.get());
            });
        }
    }

}
