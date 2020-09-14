/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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

import javax.swing.*;
import javax.swing.filechooser.FileSystemView;

public class WatchFileTree extends FileTree {

    public WatchFileTree() {
        super(null);
    }

    public WatchFileTree(final File rootFile) {
        super(rootFile, false);
    }

    public WatchFileTree(final File rootFile, final boolean showHiddenFiles) {
        super(rootFile, showHiddenFiles);
    }

    @Override
    protected FileTreeModel createModel(final FileSystemView fsv, final File rootFile, final boolean showHiddenFiles) {
        return new WatchFileTreeModel(fsv, rootFile, showHiddenFiles);
    }

    @Override
    public void setFileTreeModel(final FileTreeModel fileTreeModel) {
        if (getModel() == fileTreeModel)
            return;
        if (getModel() instanceof WatchFileTreeModel) {
            ((WatchFileTreeModel) getModel()).stopWatching();
        }
        if (isVisible() && fileTreeModel instanceof WatchFileTreeModel) {
            ((WatchFileTreeModel) fileTreeModel).startWatching();
        }
        super.setFileTreeModel(fileTreeModel);
    }

    @Override
    public void addNotify() {
        super.addNotify();
        SwingUtilities.invokeLater(() -> {
            if (getModel() instanceof WatchFileTreeModel) {
                ((WatchFileTreeModel) getModel()).startWatching();
            }
        });
    }

    @Override
    public void removeNotify() {
        super.removeNotify();
        SwingUtilities.invokeLater(() -> {
            if (getModel() instanceof WatchFileTreeModel) {
                ((WatchFileTreeModel) getModel()).stopWatching();
            }
        });
    }
}
