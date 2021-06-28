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
package com.github.weisj.darklaf.util;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.FileSystem;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Stream;

public class ResourceWalker implements AutoCloseable {

    private final List<FileSystem> fileSystemList = new ArrayList<>();
    private final String[] packages;
    private Stream<?> stream;

    private ResourceWalker(final String... packages) {
        this.packages = packages;
    }

    public Stream<String> stream() {
        if (stream != null) throw new IllegalStateException("Stream already open");
        Stream<String> s = Arrays.stream(packages).flatMap(this::walk);
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
            } catch (final IOException ignored) {
            }
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
        Stream<URL> stream = StreamUtil.enumerationAsStream(
                Lambdas.orDefault(classLoader::getResources, Collections.<URL>emptyEnumeration()).apply(pathName));
        return stream.map(Lambdas.wrap(URL::toURI)).flatMap(uri -> {
            if ("jar".equals(uri.getScheme())) {
                try {
                    FileSystem fileSystem = FileSystems.newFileSystem(uri, Collections.emptyMap());
                    Path resourcePath = fileSystem.getPath(pathName);
                    fileSystemList.add(fileSystem);
                    return Files.walk(resourcePath, Integer.MAX_VALUE);
                } catch (final IOException e) {
                    return Stream.empty();
                }
            } else {
                return walkFolder(new File(uri)).map(File::toPath);
            }
        }).map(Path::toString).map(p -> p.replace(File.separatorChar, '/')).map(p -> {
            int index = p.indexOf(pathName);
            return index >= 0 ? p.substring(index) : null;
        }).filter(Objects::nonNull);
    }

    private Stream<File> walkFolder(final File file) {
        if (!file.isDirectory()) return Stream.of(file);
        File[] files = file.listFiles();
        if (files == null) files = new File[0];
        return Arrays.stream(files).flatMap(this::walkFolder);
    }
}
