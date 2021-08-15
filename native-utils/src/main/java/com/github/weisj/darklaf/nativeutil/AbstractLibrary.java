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
package com.github.weisj.darklaf.nativeutil;

import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;


public abstract class AbstractLibrary {

    private final String name;
    protected final Logger logger;
    private boolean loaded;
    private boolean attemptedLoad;

    public AbstractLibrary(final String name) {
        this(name, Logger.getLogger(name));
    }

    public AbstractLibrary(final String name, final Logger logger) {
        this.name = name;
        this.logger = logger;
    }

    /** Load the decorations-library if necessary. */
    public void updateLibrary() {
        if (!isLoaded() && !attemptedLoad) {
            loadLibrary();
        }
    }

    private void loadLibrary() {
        attemptedLoad = true;
        if (!canLoad() || isLoaded()) {
            return;
        }
        try {
            String path = getLibraryPath();
            if (path != null && !path.isEmpty()) {
                List<NativeUtil.Resource> resources = getResourcePaths();
                if (resources == null || resources.isEmpty()) {
                    NativeUtil.loadLibraryFromJar(getLoaderClass(), path);
                } else {
                    NativeUtil.loadLibraryFromJarWithExtraResources(getLoaderClass(), path, resources);
                }
                loaded = true;
                info("Loaded " + name + " at " + path + ".");
            }
        } catch (final Throwable e) {
            // Library not found, SecurityManager prevents library loading etc.
            error("Could not load library " + name + ".", e);
        }
    }

    protected abstract Class<?> getLoaderClass();

    protected List<NativeUtil.Resource> getResourcePaths() {
        return Collections.emptyList();
    }

    public abstract String getLibraryPath();

    protected abstract boolean canLoad();

    public boolean isLoaded() {
        return loaded;
    }

    protected void info(final String message) {
        if (logger != null) logger.info(message);
    }

    protected void warning(final String message) {
        if (logger != null) logger.warning(message);
    }

    protected void error(final String message, final Throwable e) {
        if (logger != null) logger.log(Level.SEVERE, message, e);
    }
}
