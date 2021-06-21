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
package com.github.weisj.darklaf.components.filetree;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.stream.Stream;

import javax.swing.*;
import javax.swing.filechooser.FileSystemView;

/**
 * @deprecated {@link FileTree} will be moved to https://github.com/weisJ/swing-dsl
 */
@Deprecated
public class FileNode implements Comparable<FileNode> {

    private static final AtomicBoolean LOCKED = new AtomicBoolean(false);
    private final Object lock = new Object();
    private volatile File file;
    private final Path path;
    private final String pathName;
    private boolean empty;
    private boolean valid;
    private Icon icon;

    public FileNode(final File file, final Path path) {
        this.file = file;
        this.path = path;
        this.pathName = this.file != null ? this.file.getAbsolutePath() : null;
        if (this.file == null) {
            this.file = getFile();
        }
    }

    public void invalidate() {
        valid = false;
        icon = null;
    }

    protected boolean validateEmptyFlag(final boolean showHiddenFiles) {
        if (!valid && path != null) {
            try (Stream<Path> s = Files.list(path)) {
                empty = s.filter(Files::isReadable).noneMatch(p -> showHiddenFiles || !isHidden(p));
                valid = true;
            } catch (final IOException ignored) {
            }
        }
        return valid;
    }

    public static FileNode fromPath(final Path path) {
        return new FileNode(null, path);
    }

    public static FileNode fromFile(final File file) {
        return new FileNode(file, toPath(file));
    }

    private static Path toPath(final File file) {
        try {
            return file.toPath();
        } catch (final InvalidPathException e) {
            return null;
        }
    }

    public boolean isDirectory() {
        if (path != null) return Files.isDirectory(path);
        return file != null && file.isDirectory();
    }

    public boolean exists() {
        if (path != null) return Files.exists(path);
        return file != null && file.exists();
    }

    public boolean notExists() {
        if (path != null) return Files.notExists(path);
        return file == null || !file.exists();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FileNode on = (FileNode) o;
        if (this.path != null && on.path != null) return path.equals(on.path);
        return Objects.equals(pathName, ((FileNode) o).pathName);
    }

    @Override
    public int hashCode() {
        if (path != null) return path.hashCode();
        return Objects.hashCode(pathName);
    }

    @Override
    public String toString() {
        return path != null ? path.toString() : file != null ? pathName : "null node";
    }

    @Override
    public int compareTo(final FileNode o) {
        if (o == null) return -1;
        if (path != null && o.path != null) {
            return path.compareTo(o.path);
        }
        return pathName.compareTo(o.pathName);
    }

    public boolean isHidden() {
        boolean hidden;
        hidden = isHidden(path);
        if (file != null) {
            hidden = hidden || file.isHidden();
        }
        return hidden;
    }

    private static boolean isHidden(final Path path) {
        if (path != null) {
            try {
                return Files.isHidden(path);
            } catch (final IOException ignored) {
            }
        }
        return false;
    }

    public boolean isEmpty(final boolean showHiddenFiles) {
        if (path != null && validateEmptyFlag(showHiddenFiles)) {
            return empty;
        }
        return file == null || !file.isDirectory();
    }

    public boolean isReadable() {
        if (path != null) return Files.isReadable(path);
        return file != null && file.canRead();
    }

    public Stream<FileNode> list(final FileTreeModel model) throws IOException {
        Stream<FileNode> stream;
        if (path != null) {
            stream = Files.list(path).map(FileNode::fromPath);
        } else if (file != null) {
            LOCKED.set(true);
            File[] files = model.fsv.getFiles(file, !model.showHiddenFiles);
            stream = Arrays.stream(files).map(FileNode::fromFile).onClose(() -> LOCKED.set(false));
        } else {
            stream = Stream.empty();
        }
        return stream;
    }

    public Icon getSystemIcon(final FileSystemView fsv) {
        if (icon == null) {
            icon = fsv.getSystemIcon(getFile());
        }
        return icon;
    }

    public String getSystemDisplayName(final FileSystemView fsv) {
        File f = getFile();
        if (f == null) return path != null ? String.valueOf(path.getFileName()) : "";
        return fsv.getSystemDisplayName(f);
    }

    public Path getPath() {
        return path;
    }

    public File getFile() {
        synchronized (lock) {
            if (file == null && this.path != null && !LOCKED.get()) {
                file = path.toFile();
            }
            return file;
        }
    }
}
