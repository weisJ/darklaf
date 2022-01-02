/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.core.theme;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

public class RecordingProperties extends Properties {

    public static final Object REMOVED = new Object() {

        @Override
        public String toString() {
            return "REMOVED";
        }
    };

    private final Map<Object, Object> recording = new HashMap<>();
    private final Properties base;

    public RecordingProperties(final Properties base) {
        this.base = base;
    }

    @Override
    public synchronized Object put(final Object key, final Object value) {
        base.put(key, value);
        recording.put(key, value);
        return super.put(key, value);
    }

    @Override
    public synchronized int size() {
        return base.size();
    }

    @Override
    public synchronized boolean isEmpty() {
        return base.isEmpty();
    }

    @Override
    public synchronized boolean contains(final Object value) {
        return base.contains(value);
    }

    @Override
    public synchronized boolean containsKey(final Object key) {
        return base.containsKey(key);
    }

    @Override
    public boolean containsValue(final Object value) {
        return base.containsValue(value);
    }

    @Override
    public synchronized Object get(final Object key) {
        return base.get(key);
    }

    @Override
    public synchronized boolean remove(final Object key, final Object value) {
        recording.put(key, REMOVED);
        return base.remove(key, value);
    }

    @Override
    public synchronized void putAll(final Map<?, ?> t) {
        base.putAll(t);
        recording.putAll(t);
    }

    @Override
    public synchronized void clear() {
        recording.keySet().forEach(k -> recording.put(k, REMOVED));
        base.keySet().forEach(k -> recording.put(k, REMOVED));
        base.clear();
    }

    @Override
    public Set<Object> keySet() {
        return base.keySet();
    }

    @Override
    public Collection<Object> values() {
        return base.values();
    }

    @Override
    public Set<Map.Entry<Object, Object>> entrySet() {
        return base.entrySet();
    }

    public Map<Object, Object> getRecording() {
        return recording;
    }
}
