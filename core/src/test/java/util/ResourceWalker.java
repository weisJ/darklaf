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
package util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

public class ResourceWalker implements AutoCloseable {

    private final List<FileSystem> fileSystemList = new ArrayList<>();
    private final String[] packages;
    private Stream<?> stream;

    private ResourceWalker(final String... packages) {
        this.packages = packages;
    }

    public Stream<String> stream() {
        if (stream != null) throw new IllegalStateException("Stream already open");
        Stream<String> s = Arrays.stream(packages)
                                 .flatMap(this::walk);
        stream = s;
        return s;
    }

    @Override
    public void close() {
        stream.close();
        stream = null;
        for (FileSystem fileSystem : fileSystemList) {
            try {
                fileSystem.close();
            } catch (IOException ignored) {}
        }
        fileSystemList.clear();
    }

    public static ResourceWalker walkResources(final String... packages) {
        return new ResourceWalker(packages);
    }

    private Stream<String> walk(final String path) {
        String pack = path.replace('.', '/');
        pack = pack.endsWith("/") ? pack : pack + "/";
        String pathName = pack;
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        Stream<URL> stream = enumerationAsStream(orDefault(classLoader::getResources,
                                                           Collections.<URL>emptyEnumeration()).apply(pathName));
        return stream.map(wrap(URL::toURI))
                     .flatMap(orDefault(uri -> {
                         if ("jar".equals(uri.getScheme())) {
                             FileSystem fileSystem = FileSystems.newFileSystem(uri, Collections.emptyMap());
                             Path resourcePath = fileSystem.getPath(pathName);
                             fileSystemList.add(fileSystem);
                             return Files.walk(resourcePath, Integer.MAX_VALUE);
                         } else {
                             return walkFolder(new File(uri)).map(File::toPath);
                         }
                     }, Stream.empty()))
                     .map(Path::toString)
                     .map(p -> p.replace(File.separatorChar, '/'))
                     .map(p -> {
                         int index = p.indexOf(pathName);
                         return index >= 0 ? p.substring(index) : null;
                     })
                     .filter(Objects::nonNull);
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

    private Stream<File> walkFolder(final File file) {
        if (!file.isDirectory()) return Stream.of(file);
        File[] files = file.listFiles();
        if (files == null) files = new File[0];
        return Arrays.stream(files).flatMap(this::walkFolder);
    }

    public static <T, K, E extends Throwable> Function<T, K> orDefault(final CheckedFunction<T, K, E> wrappee,
                                                                       final K fallback) {
        return t -> {
            try {
                return wrappee.apply(t);
            } catch (final Throwable e) {
                return fallback;
            }
        };
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
