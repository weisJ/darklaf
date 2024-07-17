/*
 * MIT License
 *
 * Copyright (c) 2020-2024 Jannis Weis
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
package com.github.weisj.darklaf.ui;

import java.awt.Window;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.logging.Level;
import java.util.stream.Collectors;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.core.test.DarklafOnly;
import com.github.weisj.darklaf.core.test.DelicateDemo;
import com.github.weisj.darklaf.core.test.util.ClassFinder;
import com.github.weisj.darklaf.core.test.util.Instantiable;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.ComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.util.Pair;

public class DemoLauncher extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new DemoLauncher());
    }

    List<DemoEntry> demoClasses;

    public List<DemoEntry> getDemoClasses() {
        return demoClasses;
    }

    public DemoLauncher() {
        Class<ComponentDemo> demoType = ComponentDemo.class;
        String[] packages = {
                "com/github/weisj/darklaf/ui",
                "com/github/weisj/darklaf/icon",
                "com/github/weisj/darklaf/defaults"
        };
        demoClasses = ClassFinder.getInstancesOfType(demoType, packages).stream()
                .filter(i -> !DemoLauncher.class.isAssignableFrom(i.getType()))
                .map(DemoEntry::new)
                .distinct()
                .sorted(Comparator.comparing(DemoEntry::toString))
                .collect(Collectors.toList());
    }

    @Override
    public JComponent createComponent() {
        JComboBox<DemoEntry> demos = new JComboBox<>(demoClasses.toArray(new DemoEntry[0]));
        Box box = Box.createHorizontalBox();
        box.add(demos);
        JButton button = new JButton("Start");
        button.addActionListener(
                e -> Optional.ofNullable((DemoEntry) demos.getSelectedItem()).ifPresent(DemoEntry::start));
        box.add(Box.createHorizontalStrut(10));
        box.add(button);
        return new DemoPanel(box);
    }

    @Override
    public String getName() {
        return "Demo Launcher";
    }

    public static class DemoEntry {

        private final Instantiable<ComponentDemo> demo;

        public DemoEntry(final Instantiable<ComponentDemo> demo) {
            this.demo = demo;
        }

        public void start() {
            start(null);
        }

        public Pair<AtomicReference<Window>, ComponentDemo> start(final Level logLevel) {
            ComponentDemo componentDemo = demo.instantiate();
            LafManager.setLogLevel(logLevel != null ? logLevel : Level.FINE);
            return new Pair<>(
                    DemoExecutor.showDemoWithoutSetup(componentDemo, true),
                    componentDemo);
        }

        @Override
        public String toString() {
            return demo.getType().getSimpleName() + ".java";
        }

        public boolean isDelicate() {
            return demo.getType().getAnnotation(DelicateDemo.class) != null;
        }

        public boolean isDarklafOnly() {
            return demo.getType().getAnnotation(DarklafOnly.class) != null;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) {
                return true;
            }
            if (!(o instanceof DemoEntry demoEntry)) {
                return false;
            }
            return demo.getType().equals(demoEntry.demo.getType());
        }

        @Override
        public int hashCode() {
            return Objects.hash(demo.getType());
        }
    }
}
