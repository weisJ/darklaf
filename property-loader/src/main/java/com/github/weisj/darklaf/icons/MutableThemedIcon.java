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
package com.github.weisj.darklaf.icons;

import java.util.Map;

public interface MutableThemedIcon extends ThemedIcon {

    Map<Object, Object> getProperties();

    void setProperties(final Map<Object, Object> props);

    Map<Object, Object> getContextProperties();

    void setContextProperties(final Map<Object, Object> props);

    /**
     * Set a property if the underlying property map supports mutation.
     *
     * @param key the property key.
     * @param value the property value.
     * @throws UnsupportedOperationException if the underlying property map doesnt support mutation.
     */
    default void setProperty(final Object key, final Object value) throws UnsupportedOperationException {
        getProperties().put(key, value);
    }

    /**
     * Get a property.
     *
     * @param key the property key.
     * @return the property value.
     */
    default Object getProperty(final Object key) {
        return getProperties().get(key);
    }

    /**
     * Get a property of a given type.
     *
     * @param key the property key.
     * @param type the type.
     * @param <T> the types type parameter.
     * @return the property value if the type matches or null otherwise.
     */
    default <T> T getPropertyOfType(final Object key, final Class<T> type) {
        Object obj = getProperty(key);
        if (type != null && type.isInstance(obj)) return type.cast(obj);
        return null;
    }
}
