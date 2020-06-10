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

public class ClassFinder {

    public static <T> List<T> getInstancesOfType(final Class<T> type, final String... packages) {
        return getClasses(packages).filter(type::isAssignableFrom)
                                   .filter(cls -> !cls.isInterface())
                                   .filter(cls -> !Modifier.isAbstract(cls.getModifiers()))
                                   .map(ClassFinder::getInstance)
                                   .filter(Objects::nonNull)
                                   .map(type::cast)
                                   .collect(Collectors.toList());
    }

    private static <T> T getInstance(final Class<T> type) {
        try {
            return type.getDeclaredConstructor().newInstance();
        } catch (InstantiationException
                 | IllegalAccessException
                 | InvocationTargetException
                 | NoSuchMethodException e) {
            return null;
        }
    }

    private static Stream<Class<?>> getClasses(final String... packageNames) {
        return Stream.of(packageNames).flatMap(wrap(ClassFinder::getClasses));
    }

    private static Stream<Class<?>> getClasses(final String packageName) throws IOException {
        ClassLoader classLoader = ClassFinder.class.getClassLoader();
        String pack = packageName.replace(".", "/");
        Enumeration<URL> resources = classLoader.getResources(pack);
        return enumerationAsStream(resources).map(ClassFinder::URLtoFile)
                                             .filter(Objects::nonNull)
                                             .map(dir -> findClasses(dir, packageName))
                                             .flatMap(List::stream);
    }

    private static File URLtoFile(final URL url) {
        try {
            return new File(url.toURI());
        } catch (URISyntaxException e) {
            return null;
        }
    }

    private static List<Class<?>> findClasses(final File dir, final String packageName) {
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

    public static <T> Stream<T> enumerationAsStream(final Enumeration<T> e) {
        return StreamSupport.stream(Spliterators.spliteratorUnknownSize(new Iterator<T>() {
            public T next() {
                return e.nextElement();
            }

            public boolean hasNext() {
                return e.hasMoreElements();
            }
        }, Spliterator.ORDERED), false);
    }

    public static <T, K, E extends Throwable> Function<T, K> wrap(final CheckedFunction<T, K, E> wrappee) {
        return t -> {
            try {
                return wrappee.apply(t);
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
    }

    public interface CheckedFunction<T, K, E extends Throwable> {

        K apply(final T value) throws E;
    }
}
