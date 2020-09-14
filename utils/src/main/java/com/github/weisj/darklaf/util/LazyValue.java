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
package com.github.weisj.darklaf.util;

import java.util.Optional;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class LazyValue<T> {

    private Supplier<T> supplier;
    private T value;

    public LazyValue(final T value) {
        this.value = value;
    }

    public LazyValue(final Supplier<T> supplier) {
        this.supplier = supplier;
    }

    public boolean isInitialized() {
        return supplier == null;
    }

    protected T load() {
        if (supplier != null) {
            T obj = supplier.get();
            supplier = null;
            return obj;
        }
        return value;
    }

    public void ifPresent(final Consumer<T> action) {
        if (isInitialized() && value != null) {
            action.accept(value);
        }
    }

    public void ifPresentNullable(final Consumer<Optional<T>> action) {
        if (isInitialized()) {
            action.accept(Optional.ofNullable(value));
        }
    }

    public T get() {
        if (value == null) value = load();
        return value;
    }
}
