/*
 * MIT License
 *
 * Copyright (c) 2023 Jannis Weis
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
 */
package com.github.weisj.darklaf.defaults;

import java.util.*;

import javax.swing.*;

public class UIDefaultsWithResourceBundleCache extends UIDefaults {

    private final Locale defaultLocale = Locale.getDefault();
    private final ResourceBundleCache resourceBundleCache = new ResourceBundleCache();

    public UIDefaultsWithResourceBundleCache(final int initialCapacity, final float loadFactor) {
        super(initialCapacity, loadFactor);
    }

    @Override
    @SuppressWarnings("UnsynchronizedOverridesSynchronized")
    public Object get(final Object key) {
        Object value = super.get(key);
        return (value != null) ? value : getFromResourceBundle(key, null);
    }

    @Override
    public Object get(final Object key, final Locale l) {
        Object value = super.get(key, l);
        return (value != null) ? value : getFromResourceBundle(key, l);
    }

    private Object getFromResourceBundle(final Object key, final Locale locale) {
        Locale l = locale;

        if (resourceBundleCache.isEmpty() || !(key instanceof String)) {
            return null;
        }

        // A null locale means use the default locale.
        if (l == null) {
            if (defaultLocale == null) return null;
            l = defaultLocale;
        }

        synchronized (this) {
            return resourceBundleCache.get((String) key, l);
        }
    }

    @Override
    @Deprecated
    public synchronized void addResourceBundle(final String bundleName) {
        super.addResourceBundle(bundleName);
    }

    public synchronized void addResourceBundle(final Class<?> clazz, final String bundleName) {
        resourceBundleCache.addBundle(clazz, bundleName);
    }
}
