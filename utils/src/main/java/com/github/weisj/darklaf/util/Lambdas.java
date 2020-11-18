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

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class Lambdas {

    private Lambdas() {}

    public static final Runnable DO_NOTHING = () -> {
    };

    public static <T, K, E extends Throwable> Function<T, K> orDefault(final CheckedFunction<T, K, E> wrappee,
            final K fallback) {
        return t -> {
            try {
                return wrappee.apply(t);
            } catch (final Throwable e) {
                return fallback;
            }
        };
    }

    public static <T, E extends Throwable> Supplier<T> orDefault(final CheckedSupplier<T, E> wrappee,
            final T fallback) {
        return () -> {
            try {
                return wrappee.get();
            } catch (final Throwable e) {
                return fallback;
            }
        };
    }

    public static <T, E extends Throwable> Predicate<T> orDefault(final CheckedPredicate<T, E> wrappee,
            final boolean fallback) {
        return t -> {
            try {
                return wrappee.test(t);
            } catch (final Throwable e) {
                return fallback;
            }
        };
    }

    public static <T, K, E extends Throwable> Function<T, K> wrap(final CheckedFunction<T, K, E> wrappee) {
        return t -> {
            try {
                return wrappee.apply(t);
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
    }

    public static <T, E extends Throwable> Consumer<T> wrap(final CheckedConsumer<T, E> wrappee) {
        return t -> {
            try {
                wrappee.accept(t);
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
    }

    public static <T, E extends Throwable> Supplier<T> wrap(final CheckedSupplier<T, E> wrappee) {
        return () -> {
            try {
                return wrappee.get();
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
    }

    public static <T, E extends Throwable> Predicate<T> wrap(final CheckedPredicate<T, E> wrappee) {
        return t -> {
            try {
                return wrappee.test(t);
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
    }

    public static <E extends Throwable> Runnable wrap(final CheckedRunnable<E> wrappee) {
        return () -> {
            try {
                wrappee.run();
            } catch (final Throwable e) {
                throw new RuntimeException(e);
            }
        };
    }

    public interface CheckedFunction<T, K, E extends Throwable> {

        K apply(final T value) throws E;
    }

    public interface CheckedConsumer<T, E extends Throwable> {

        void accept(final T value) throws E;
    }

    public interface CheckedSupplier<T, E extends Throwable> {

        T get() throws E;
    }

    public interface CheckedPredicate<T, E extends Throwable> {

        boolean test(final T value) throws E;
    }

    public interface CheckedRunnable<E extends Throwable> {

        void run() throws E;
    }
}
