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

import java.io.IOException;
import java.nio.file.FileSystems;
import java.nio.file.WatchKey;
import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;
import java.util.stream.Stream;

import javax.swing.*;
import javax.swing.tree.TreeNode;

import com.github.weisj.darklaf.util.StreamUtil;

public class FileTreeNode implements TreeNode, Comparable<FileTreeNode> {

    protected final FileTreeNode parent;
    protected final FileTreeModel model;
    protected final FileNode fileNode;
    protected AtomicInteger taskCount = new AtomicInteger();
    protected AtomicReference<List<FileTreeNode>> children;
    protected WatchKey watchKey;

    public FileTreeNode(final FileTreeNode parent, final FileNode fileNode, final FileTreeModel model) {
        if (fileNode == null) throw new IllegalArgumentException("File node is null");
        this.model = model;
        this.fileNode = fileNode;
        this.parent = parent;
        this.children = new AtomicReference<>();
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FileTreeNode that = (FileTreeNode) o;
        return Objects.equals(fileNode, that.fileNode);
    }

    @Override
    public int hashCode() {
        return fileNode.hashCode();
    }

    public FileNode getFile() {
        return fileNode;
    }

    private int addSorted(final List<FileTreeNode> nodes, final FileTreeNode node) {
        int index = Collections.binarySearch(nodes, node);
        if (index < 0) index = ~index;
        nodes.add(index, node);
        model.register(node);
        return index;
    }

    private int remove(final List<FileTreeNode> nodes, final FileTreeNode node) {
        int index = nodes.indexOf(node);
        if (index >= 0) {
            nodes.remove(node);
            model.unregister(node);
        }
        return index;
    }

    public void reload() {
        reload(Integer.MAX_VALUE);
    }

    protected void reload(final int depth) {
        if (depth < 0) return;
        if (children.get() == null) return;
        List<FileTreeNode> fileList = children.get();
        this.<ReloadOp>doInBackground(pub -> {
            this.traverseChildren(s -> Stream.concat(s.map(this::toNode), fileList.stream()).map(n -> {
                if (n.fileNode.notExists()) {
                    return ReloadOp.remove(n, remove(fileList, n));
                } else {
                    if (model.showHiddenFiles) {
                        if (!fileList.contains(n)) {
                            return ReloadOp.add(n, addSorted(fileList, n));
                        }
                    } else {
                        if (n.fileNode.isHidden()) {
                            return ReloadOp.remove(n, remove(fileList, n));
                        } else if (!fileList.contains(n)) {
                            return ReloadOp.add(n, addSorted(fileList, n));
                        }
                    }
                }
                return null;
            }).forEach(pub));
        }, chunk -> {
            for (ReloadOp op : chunk) {
                if (op != null && op.index >= 0) {
                    switch (op.type) {
                        case ADD:
                            model.nodesWereInserted(FileTreeNode.this, new int[] {op.index});
                            break;
                        case REMOVE:
                            model.nodesWereRemoved(FileTreeNode.this, new int[] {op.index}, new Object[] {op.node});
                            break;
                    }
                }
            }
        }, () -> {
            fileNode.invalidate();
            if (depth > 0) fileList.forEach(n -> n.reload(depth - 1));
        });
    }

    private List<FileTreeNode> getChildren() {
        return children.updateAndGet(list -> {
            if (list != null) {
                return list;
            }
            List<FileTreeNode> fileList = Collections.synchronizedList(new ArrayList<>());
            this.<Integer>doInBackground(pub -> {
                traverseChildren(s -> {
                    s.filter(p -> model.showHiddenFiles || !p.isHidden()).map(this::toNode).sorted()
                            .map(n -> addSorted(fileList, n)).forEach(pub);
                });
            }, chunks -> {
                int[] indices = chunks.stream().mapToInt(Integer::intValue).toArray();
                model.nodesWereInserted(FileTreeNode.this, indices);
            }, () -> model.nodeChanged(FileTreeNode.this));
            return fileList;
        });
    }

    @Override
    public String toString() {
        return fileNode.toString();
    }

    protected FileTreeNode toNode(final FileNode fileNode) {
        return model.createNode(FileTreeNode.this, fileNode);
    }

    protected void traverseChildren(final Consumer<Stream<FileNode>> consumer) {
        if (fileNode.isDirectory()) {
            try (Stream<FileNode> files = fileNode.list(model)) {
                consumer.accept(files.filter(FileNode::isReadable));
            } catch (IOException ignored) {
            }
        }
    }

    protected <T> void doInBackground(final Consumer<Consumer<T>> task, final Consumer<List<T>> processor) {
        doInBackground(task, processor, () -> {
        });
    }

    protected <T> void doInBackground(final Consumer<Consumer<T>> task, final Consumer<List<T>> processor,
            final Runnable doneTask) {
        taskCount.getAndIncrement();
        SwingWorker<Void, T> worker = new SwingWorker<Void, T>() {
            @Override
            public Void doInBackground() {
                task.accept(this::publish);
                return null;
            }

            @Override
            protected void process(final List<T> chunks) {
                processor.accept(chunks);
            }

            @Override
            protected void done() {
                doneTask.run();
                taskCount.getAndDecrement();
            }
        };
        worker.execute();
    }

    @Override
    public TreeNode getChildAt(final int childIndex) {
        return getChildren().get(childIndex);
    }

    @Override
    public int getChildCount() {
        return getChildren().size();
    }

    @Override
    public TreeNode getParent() {
        return parent;
    }

    @Override
    public int getIndex(final TreeNode node) {
        if (node == null) {
            throw new IllegalArgumentException("argument is null");
        }
        if (!isNodeChild(node)) {
            return -1;
        }
        return getChildren().indexOf(node);
    }

    @Override
    public boolean getAllowsChildren() {
        return fileNode.isDirectory();
    }

    @Override
    public boolean isLeaf() {
        if (!fileNode.isDirectory()) return true;
        if (children.get() != null && !isBusy()) {
            return children.get().size() == 0;
        }
        return fileNode.isEmpty(model.showHiddenFiles);
    }

    @Override
    public Enumeration<? extends TreeNode> children() {
        return Collections.enumeration(children.get());
    }

    public boolean isNodeChild(final TreeNode aNode) {
        if (aNode == null) return false;
        return (aNode.getParent() == this);
    }

    @Override
    public int compareTo(final FileTreeNode o) {
        if (o == null) return -1;
        boolean thisDir = fileNode.isDirectory();
        boolean oDir = fileNode.isDirectory();
        if (thisDir == oDir) {
            return fileNode.compareTo(o.fileNode);
        } else {
            return thisDir ? -1 : 1;
        }
    }

    public boolean isBusy() {
        return taskCount.get() > 0;
    }

    public static class RootNode extends FileTreeNode {

        private final List<FileNode> rootPaths;

        public RootNode(final FileTreeModel model, final List<FileNode> rootPaths) {
            super(null, new FileNode(null, null), model);
            this.rootPaths = rootPaths != null ? rootPaths : Collections.emptyList();
            init();
        }

        protected Stream<FileNode> createInitialDirectories() {
            return rootPaths.size() > 0 ? rootPaths.stream()
                    : StreamUtil.iterableAsStream(FileSystems.getDefault().getRootDirectories())
                            .map(FileNode::fromPath);
        }

        protected void init() {
            List<FileTreeNode> nodes = new ArrayList<>();
            createInitialDirectories().forEach(p -> {
                FileTreeNode node = model.createNode(RootNode.this, p);
                model.register(node);
                nodes.add(node);
            });
            children.set(nodes);
        }

        @Override
        protected void reload(final int depth) {
            if (depth < 0) return;
            List<FileTreeNode> nodes = children.get();
            createInitialDirectories().forEach(p -> {
                FileTreeNode node = model.createNode(this, p);
                if (!nodes.contains(node)) {
                    model.register(node);
                    nodes.add(node);
                }
            });
            nodes.removeIf(n -> {
                if (n.fileNode.notExists()) {
                    model.unregister(n);
                    return true;
                }
                return false;
            });
            if (depth > 0) children.get().forEach(n -> n.reload(depth - 1));
            fileNode.invalidate();
        }

        @Override
        public boolean isLeaf() {
            return children.get().size() == 0;
        }

        @Override
        public boolean getAllowsChildren() {
            return true;
        }
    }

    private static class ReloadOp {

        private final Type type;
        private final FileTreeNode node;
        private final int index;

        private ReloadOp(final Type type, final FileTreeNode node, final int index) {
            this.type = type;
            this.node = node;
            this.index = index;
        }

        private static ReloadOp add(final FileTreeNode n, final int index) {
            return new ReloadOp(Type.ADD, n, index);
        }

        private static ReloadOp remove(final FileTreeNode n, final int index) {
            return new ReloadOp(Type.REMOVE, n, index);
        }

        private enum Type {
            ADD, REMOVE
        }

    }
}
