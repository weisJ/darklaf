/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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


import java.awt.Robot;
import java.awt.Window;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;

import javax.swing.JComponent;
import javax.swing.UIManager;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.ui.DemoLauncher;
import com.github.weisj.darklaf.ui.demo.ComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoSpec;
import com.github.weisj.darklaf.util.Lambdas;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Pair;

class DemoTest implements NonThreadSafeTest {

    private static final Logger LOGGER = LogUtil.getLogger(DemoTest.class);
    private static final boolean EXTENSIVE = false;

    @Test
    void runningDemosDoesNotThrow() {
        Robot robot = Lambdas.orDefault(() -> new Robot(), null).get();
        LafManager.setLogLevel(Level.WARNING);
        for (Theme theme : LafManager.getRegisteredThemes()) {
            LOGGER.warning("Testing " + theme.getDisplayName());
            runDemos(theme, robot);
            if (!Boolean.getBoolean("runAllThemes")) break;
        }
    }

    private void runDemos(final Theme theme, final Robot robot) {
        List<DemoLauncher.DemoEntry> demos = new DemoLauncher().getDemoClasses();
        int count = demos.size();
        int index = 0;
        Map<Boolean, List<DemoLauncher.DemoEntry>> groupedDemos = demos.stream()
                .filter(Objects::nonNull)
                .collect(Collectors.groupingBy(DemoLauncher.DemoEntry::isDarklafOnly, Collectors.toList()));
        for (DemoLauncher.DemoEntry demo : groupedDemos.get(false)) {
            runDemo(theme, robot, count, index, demo);
            index++;
        }
        for (DemoLauncher.DemoEntry demo : groupedDemos.get(true)) {
            runDemo(theme, robot, count, index, demo);
            index++;
        }
    }

    private void runDemo(final Theme theme, final Robot robot, final int count, final int index,
            final DemoLauncher.DemoEntry demo) {
        TestUtils.ensureLafInstalled(theme, true);
        LOGGER.warning("Running: " + (index + 1) + "/" + count + " " + demo);
        if (demo.isDelicate()) {
            LOGGER.warning("Skipping");
            return;
        }
        try {
            String demoName = demo.toString();
            Pair<AtomicReference<Window>, ComponentDemo> startedDemo = demo.start(Level.WARNING);
            AtomicReference<Window> windowRef = startedDemo.getFirst();
            ComponentDemo componentDemo = startedDemo.getSecond();
            if (robot != null) robot.waitForIdle();
            Assertions.assertNotNull(windowRef.get(), demoName);
            TestUtils.runOnSwingThreadNotThrowing(
                    () -> Assertions.assertNotNull(windowRef.get(), demoName));
            TestUtils.runOnSwingThreadNotThrowing(
                    () -> testConfigurations(componentDemo));

            if (!demo.isDarklafOnly()) {
                TestUtils.runOnSwingThreadNotThrowing(() -> {
                    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                    LafManager.updateLaf();
                    LafManager.install(theme);
                });
            }
            TestUtils.runOnSwingThreadNotThrowing(
                    () -> TestUtils.closeWindow(windowRef.get()));
        } catch (final Exception e) {
            Assertions.fail(e);
        }
    }

    private void testConfigurations(final ComponentDemo demo) {
        if (!EXTENSIVE) return;
        final List<DemoSpec<?>> specs = demo.getSpecs();
        testConfigurations(demo.getComponent(), specs, 0);
    }

    @SuppressWarnings("unchecked")
    private void testConfigurations(final JComponent c, final List<DemoSpec<?>> specs, final int index) {
        if (index >= specs.size()) return;
        DemoSpec<Object> spec = (DemoSpec<Object>) specs.get(index);
        for (Object possibleValue : spec.getPossibleValues()) {
            spec.set(c, possibleValue);
            testConfigurations(c, specs, index + 1);
        }
    }

}
