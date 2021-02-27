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
import java.util.*;
import java.util.stream.Collectors;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;
import util.ClassFinder;


public class DemoLauncher implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new DemoLauncher());
    }

    List<DemoEntry> demoClasses;

    public DemoLauncher() {
        Class<ComponentDemo> demoType = ComponentDemo.class;
        demoClasses = ClassFinder.getInstancesOfType(demoType, "ui", "icon", "defaults").stream()
                .filter(obj -> !(obj instanceof DemoLauncher)).map(DemoEntry::new)
                .sorted(Comparator.comparing(DemoEntry::toString)).collect(Collectors.toList());
    }

    @Override
    public JComponent createComponent() {
        JComboBox<DemoEntry> demos = new JComboBox<>(demoClasses.toArray(new DemoEntry[0]));
        Box box = Box.createHorizontalBox();
        box.add(demos);
        JButton button = new JButton("Start");
        button.addActionListener(
                e -> Optional.ofNullable(((DemoEntry) demos.getSelectedItem())).ifPresent(DemoEntry::start));
        box.add(Box.createHorizontalStrut(10));
        box.add(button);
        return new DemoPanel(box);
    }

    @Override
    public String getTitle() {
        return "Demo Launcher";
    }

    private static class DemoEntry {

        private final ComponentDemo demo;

        public DemoEntry(final ComponentDemo demo) {
            this.demo = demo;
        }

        public void start() {
            ComponentDemo.showDemo(demo, true);
        }

        @Override
        public String toString() {
            return demo.getClass().getSimpleName() + ".java";
        }
    }
}
