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
package com.github.weisj.darklaf.platform;

import java.util.logging.Level;
import java.util.logging.Logger;

public abstract class AbstractLibrary {

    protected final Logger logger;
    private final String path;
    private final String libraryName;
    private boolean loaded;
    private boolean attemptedLoad;

    protected AbstractLibrary(final String path, final String libraryName) {
        this.path = path;
        this.libraryName = libraryName;
        logger = Logger.getLogger(libraryName);
    }

    /**
     * Load the decorations-library if necessary.
     */
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
                NativeUtil.loadLibraryFromJar(getLibraryPath());
            }
            loaded = true;
            logger.info("Loaded " + getLibraryName() + ". Native features are enabled.");
        } catch (Throwable e) {
            // Library not found, SecurityManager prevents library loading etc.
            logger.log(Level.SEVERE,
                       "Could not load library " + getLibraryName() + ". Native features will be disabled",
                       e);
        }
    }

    protected String getLibraryPath() {
        return getPath() + getLibraryName();
    }

    protected String getPath() {
        return path;
    }

    public String getLibraryName() {
        return libraryName;
    }

    protected abstract boolean canLoad();

    public boolean isLoaded() {
        return loaded;
    }
}
