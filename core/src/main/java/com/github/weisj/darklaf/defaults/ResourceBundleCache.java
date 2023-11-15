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

import java.lang.ref.WeakReference;
import java.util.*;

class ResourceBundleCache {

    private static final class ResourceBundleEntry {
        private final WeakReference<ClassLoader> classLoader;
        private final String path;

        private ResourceBundleEntry(final ClassLoader classLoader, final String path) {
            this.classLoader = new WeakReference<>(classLoader);
            this.path = path;
        }

        @Override
        public boolean equals(final Object o) {
            if (this == o) return true;
            if (!(o instanceof ResourceBundleEntry that)) return false;
            ClassLoader loaderA = classLoader.get();
            ClassLoader loaderB = that.classLoader.get();
            return Objects.equals(loaderA, loaderB) && Objects.equals(path, that.path);
        }

        @Override
        public int hashCode() {
            return Objects.hash(classLoader.get(), path);
        }
    }

    private final List<ResourceBundleEntry> resourceBundles = new ArrayList<>();

    private final Map<Locale, Map<String, Object>> resourceCache = new HashMap<>();

    public boolean isEmpty() {
        return resourceBundles.isEmpty();
    }

    public Object get(final String key, final Locale l) {
        return getResourceCache(l).get(key);
    }

    private Map<String, Object> getResourceCache(final Locale l) {
        Map<String, Object> values = resourceCache.get(l);

        if (values == null) {
            values = new TextAndMnemonicHashMap();
            for (int i = resourceBundles.size() - 1; i >= 0; i--) {
                ResourceBundleEntry bundle = resourceBundles.get(i);
                ClassLoader classLoader = bundle.classLoader.get();
                if (classLoader == null) continue;

                try {
                    ResourceBundle b = ResourceBundle.getBundle(bundle.path, l, classLoader);
                    Enumeration<String> keys = b.getKeys();

                    while (keys.hasMoreElements()) {
                        String key = keys.nextElement();

                        if (values.get(key) == null) {
                            values.put(key, b.getObject(key));
                        }
                    }
                } catch (MissingResourceException mre) {
                    // Keep looking
                }
            }
            resourceCache.put(l, values);
        }
        return values;
    }

    public void addBundle(final Class<?> clazz, final String bundleName) {
        if (bundleName == null) return;

        ClassLoader classLoader = clazz.getClassLoader();
        ResourceBundleEntry entry = new ResourceBundleEntry(classLoader, bundleName);
        if (!resourceBundles.contains(entry)) {
            resourceBundles.add(entry);
            resourceCache.clear();
        }
    }
}
