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
package com.github.weisj.darklaf.util;

import java.util.Comparator;
import java.util.function.Function;

public class Pair<T, H> {

    private T first;
    private H second;

    public Pair(final T first, final H second) {
        this.first = first;
        this.second = second;
    }

    public H getSecond() {
        return second;
    }

    public T getFirst() {
        return first;
    }

    public void setFirst(final T first) {
        this.first = first;
    }

    public void setSecond(final H second) {
        this.second = second;
    }

    @Override
    public String toString() {
        return "[" + first.toString() + "," + second.toString() + "]";
    }

    public static <L extends Comparable<L>, R> Comparator<Pair<L, R>> compareFirst() {
        return compareFirst(first -> first);
    }

    public static <L, R, C extends Comparable<C>> Comparator<Pair<L, R>> compareFirst(final Function<L, C> mapping) {
        return Comparator.comparing(pair -> mapping.apply(pair.first));
    }

    public static <L, R extends Comparable<R>> Comparator<Pair<L, R>> compareSecond() {
        return compareSecond(second -> second);
    }

    public static <L, R, C extends Comparable<C>> Comparator<Pair<L, R>> compareSecond(final Function<R, C> mapping) {
        return Comparator.comparing(pair -> mapping.apply(pair.second));
    }
}
