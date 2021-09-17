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
package com.github.weisj.darklaf.util.cache;

import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.util.Map;
import java.util.WeakHashMap;

public class SoftCache<K, V extends SoftCache.Cacheable<K>> {

    private final Map<K, SoftReference<V>> cache = new WeakHashMap<>();

    public V get(final K key) {
        Reference<V> reference = cache.get(key);
        if (reference == null) return null;
        V value = reference.get();
        if (value == null) {
            cache.remove(key);
        }
        return value;
    }

    public void put(final K key, final V value) {
        if (value != null) {
            cache.put(key, new SoftReference<>(value));
            value.setCacheKey(key);
        } else {
            cache.remove(key);
        }
    }

    public void clear() {
        cache.clear();
    }

    public int size() {
        return cache.size();
    }

    public boolean isEmpty() {
        return cache.isEmpty();
    }

    public interface Cacheable<K> {
        void setCacheKey(final K key);
    }
}
