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
package com.github.weisj.darklaf.util.value;

import java.lang.ref.WeakReference;
import java.util.function.Supplier;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class WeakShared<T> {

    private @NotNull final Supplier<T> supplier;
    private @Nullable WeakReference<T> reference = null;

    public WeakShared(@NotNull final Supplier<@NotNull T> supplier) {
        this.supplier = supplier;
    }

    @NotNull
    protected T create() {
        return supplier.get();
    }

    @NotNull
    public T get() {
        T value;
        if (reference == null) {
            value = create();
            reference = new WeakReference<>(value);
        } else {
            value = reference.get();
            if (value == null) {
                value = create();
                reference = new WeakReference<>(value);
            }
        }
        return value;
    }

    public T getIfPresent() {
        WeakReference<T> ref = reference;
        return ref != null ? ref.get() : null;
    }
}
