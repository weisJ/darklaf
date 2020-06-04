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
import java.io.File;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import javax.swing.*;

import ui.ComponentDemo;
import ui.DemoPanel;

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
 */
public class DemoLauncher implements ComponentDemo {

    public static void main(final String[] args) {
        ComponentDemo.showDemo(new DemoLauncher());
    }

    List<DemoEntry> demoClasses;

    public DemoLauncher() {
        Class<ComponentDemo> demoType = ComponentDemo.class;
        demoClasses = getClasses("ui", "icon", "defaults").filter(demoType::isAssignableFrom)
                                                          .filter(cls -> !cls.equals(DemoEntry.class))
                                                          .filter(cls -> !cls.isInterface())
                                                          .filter(cls -> !Modifier.isAbstract(cls.getModifiers()))
                                                          .map(this::getInstance)
                                                          .filter(Objects::nonNull)
                                                          .map(demoType::cast)
                                                          .map(DemoEntry::new)
                                                          .sorted(Comparator.comparing(DemoEntry::toString))
                                                          .collect(Collectors.toList());

    }

    private <T> T getInstance(final Class<T> type) {
        try {
            return type.getDeclaredConstructor().newInstance();
        } catch (InstantiationException
                 | IllegalAccessException
                 | InvocationTargetException
                 | NoSuchMethodException e) {
            return null;
        }
    }

    private Stream<Class<?>> getClasses(final String... packageNames) {
        return Stream.of(packageNames).flatMap(wrap(this::getClasses));
    }

    private Stream<Class<?>> getClasses(final String packageName) throws IOException {
        ClassLoader classLoader = getClass().getClassLoader();
        String pack = packageName.replace(".", "/");
        Enumeration<URL> resources = classLoader.getResources(pack);
        return enumerationAsStream(resources).map(this::URLtoFile)
                                             .filter(Objects::nonNull)
                                             .map(dir -> findClasses(dir, packageName))
                                             .flatMap(List::stream);
    }

    private File URLtoFile(final URL url) {
        try {
            return new File(url.toURI());
        } catch (URISyntaxException e) {
            return null;
        }
    }

    private List<Class<?>> findClasses(final File dir, final String packageName) {
        File directory = dir.getAbsoluteFile();
        List<Class<?>> classes = new ArrayList<>();
        File[] files = directory.listFiles();
        if (files == null) return classes;
        for (File file : files) {
            if (file.isDirectory()) {
                assert !file.getName().contains(".");
                classes.addAll(findClasses(file, packageName + "." + file.getName()));
            } else if (file.getName().endsWith(".class")) {
                try {
                    classes.add(Class.forName(packageName + '.'
                                              + file.getName().substring(0, file.getName().length() - 6)));
                } catch (ClassNotFoundException ignored) {}
            }
        }
        return classes;
    }

    public <T> Stream<T> enumerationAsStream(final Enumeration<T> e) {
        return StreamSupport.stream(Spliterators.spliteratorUnknownSize(new Iterator<T>() {
            public T next() {
                return e.nextElement();
            }

            public boolean hasNext() {
                return e.hasMoreElements();
            }
        }, Spliterator.ORDERED), false);
    }

    @Override
    public JComponent createComponent() {
        JComboBox<DemoEntry> demos = new JComboBox<>(demoClasses.toArray(new DemoEntry[0]));
        Box box = Box.createHorizontalBox();
        box.add(demos);
        JButton button = new JButton("Start");
        button.addActionListener(e -> Optional.ofNullable(((DemoEntry) demos.getSelectedItem()))
                                              .ifPresent(DemoEntry::start));
        box.add(Box.createHorizontalStrut(10));
        box.add(button);
        return new DemoPanel(box);
    }

    @Override
    public String getTitle() {
        return "Demo Launcher";
    }

    private <T, K, E extends Throwable> Function<T, K> wrap(final CheckedFunction<T, K, E> wrappee) {
        return t -> {
            try {
                return wrappee.apply(t);
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
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

    private interface CheckedFunction<T, K, E extends Throwable> {

        K apply(final T value) throws E;
    }
}
