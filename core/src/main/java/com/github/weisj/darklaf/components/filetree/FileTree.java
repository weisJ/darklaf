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
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import javax.swing.*;
import javax.swing.filechooser.FileSystemView;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;


public class FileTree extends JTree {

    public FileTree() {
        this((File[]) null);
    }

    public FileTree(final File... rootFile) {
        this(false, rootFile);
    }

    public FileTree(final boolean showHiddenFiles, final File... rootFiles) {
        FileSystemView fileSystemView = FileSystemView.getFileSystemView();
        setFileTreeModel(createModel(fileSystemView, showHiddenFiles, rootFiles));
        setCellRenderer(new FileTreeCellRenderer(fileSystemView));
        setRootVisible(false);
    }

    protected FileTreeModel createModel(final FileSystemView fsv, final boolean showHiddenFiles,
            final File... rootFiles) {
        return new FileTreeModel(fsv, showHiddenFiles, rootFiles);
    }

    public boolean isShowHiddenFiles() {
        return getModel().isShowHiddenFiles();
    }

    public void setShowHiddenFiles(final boolean showHiddenFiles) {
        getModel().setShowHiddenFiles(showHiddenFiles);
    }

    @Override
    public FileTreeModel getModel() {
        return (FileTreeModel) super.getModel();
    }

    public void setFileTreeModel(final FileTreeModel fileTreeModel) {
        super.setModel(fileTreeModel);
    }

    @Override
    public void setModel(final TreeModel newModel) {}

    public void reload() {
        getModel().reload();
    }

    public Path getSelectedFile() {
        TreePath path = getSelectionPath();
        if (path == null) return null;
        return ((FileTreeNode) path.getLastPathComponent()).getFile();
    }

    public List<Path> getSelectedFiles() {
        TreePath[] paths = getSelectionPaths();
        if (paths == null) return Collections.emptyList();
        return Arrays.stream(getSelectionPaths()).map(TreePath::getLastPathComponent).map(FileTreeNode.class::cast)
                .map(FileTreeNode::getFile).collect(Collectors.toList());
    }
}
