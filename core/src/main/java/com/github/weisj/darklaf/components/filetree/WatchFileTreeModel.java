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
import java.io.IOException;
import java.nio.file.*;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.logging.Logger;

import javax.swing.filechooser.FileSystemView;

import com.github.weisj.darklaf.util.LogUtil;

public class WatchFileTreeModel extends FileTreeModel {

    private static final Logger LOGGER = LogUtil.getLogger(WatchFileTreeModel.class);
    private static final ScheduledExecutorService scheduler = createScheduler();
    private WatchService watchService;
    private Map<Watchable, FileTreeNode> nodeMap;
    private Object lock;

    private final AtomicBoolean isScheduled = new AtomicBoolean(false);
    private ScheduledFuture<?> watchTask;

    private static WatchService createWatchService() {
        WatchService ws = null;
        try {
            ws = FileSystems.getDefault().newWatchService();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return ws;
    }

    private static ScheduledExecutorService createScheduler() {
        ScheduledThreadPoolExecutor executor = new ScheduledThreadPoolExecutor(1, r -> {
            final Thread thread = new Thread(r, "File Tree Watch Thread");
            thread.setDaemon(true);
            thread.setPriority(Thread.MIN_PRIORITY);
            return thread;
        });
        executor.setContinueExistingPeriodicTasksAfterShutdownPolicy(false);
        executor.setExecuteExistingDelayedTasksAfterShutdownPolicy(false);
        return executor;
    }

    public WatchFileTreeModel(final FileSystemView fileSystemView) {
        super(fileSystemView);
    }

    public WatchFileTreeModel(final FileSystemView fileSystemView, final File root, final boolean showHiddenFiles) {
        super(fileSystemView, root, showHiddenFiles);
    }

    public WatchFileTreeModel(final FileSystemView fileSystemView, final Path root, final boolean showHiddenFiles) {
        super(fileSystemView, root, showHiddenFiles);
    }

    @Override
    protected void init() {
        lock = new Object();
        watchService = createWatchService();
        nodeMap = Collections.synchronizedMap(new HashMap<>());
    }

    private Object getLock() {
        return lock;
    }

    protected WatchService getWatchService() {
        return watchService;
    }

    protected Map<Watchable, FileTreeNode> getNodeMap() {
        return nodeMap;
    }

    public void startWatching() {
        if (watchTask != null)
            return;
        isScheduled.set(true);
        watchTask = scheduler.schedule(this::watch, 0, TimeUnit.SECONDS);
    }

    public void stopWatching() {
        if (watchTask != null) {
            isScheduled.set(false);
            watchTask.cancel(true);
            watchTask = null;
        }
    }

    private void watch() {
        while (isScheduled.get()) {
            WatchKey key;
            try {
                key = watchService.take();
            } catch (InterruptedException x) {
                x.printStackTrace();
                return;
            }

            FileTreeNode parent = getNodeMap().get(key.watchable());
            if (parent != null) {
                LOGGER.fine(() -> "Event for \"" + parent.file + "\"");
                if (parent.parent != null) {
                    parent.parent.reload(1);
                } else {
                    parent.reload(0);
                }
            }

            List<WatchEvent<?>> watchEventList = key.pollEvents();
            for (WatchEvent<?> event : watchEventList) {
                WatchEvent.Kind<?> kind = event.kind();

                Path path = (Path) event.context();
                if (kind == StandardWatchEventKinds.OVERFLOW) {
                    continue;
                }
                LOGGER.finer("Event Type " + kind.name());
                FileTreeNode node = getNodeMap().get(((Path) key.watchable()).resolve(path));
                if (node != null) {
                    LOGGER.finer(() -> "Affected node \"" + node.file + "\"");
                    node.reload(0);
                }
            }

            key.reset();
        }
    }

    protected void register(final FileTreeNode node) {
        synchronized (getLock()) {
            WatchService ws = getWatchService();
            if (ws == null || !Files.isDirectory(node.file))
                return;
            if (getNodeMap().containsKey(node.file))
                return;
            try {
                LOGGER.finer(() -> "Register watch service for \"" + node.file + "\"");
                node.watchKey = node.file.register(ws, StandardWatchEventKinds.ENTRY_CREATE,
                        StandardWatchEventKinds.ENTRY_DELETE, StandardWatchEventKinds.ENTRY_MODIFY);
                getNodeMap().put(node.file, node);
            } catch (IOException ignored) {
            }
        }
    }

    protected void unregister(final FileTreeNode node) {
        synchronized (getLock()) {
            if (node.watchKey == null)
                return;
            LOGGER.finer(() -> "Unregister watch service for \"" + node.file + "\"");
            getNodeMap().remove(node.file);
            node.watchKey.cancel();
        }
    }
}
