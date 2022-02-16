/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
package com.github.weisj.darklaf.theme.spec;

import java.awt.*;
import java.io.Serializable;
import java.util.Objects;

public class FontPrototype implements Serializable {
    private final String family;

    public static FontPrototype getDefault() {
        return new FontPrototype(null);
    }

    public FontPrototype(String family) {
        this.family = family;
    }

    public String family() {
        return family;
    }

    public static FontPrototype fromFont(final Font font) {
        return new FontPrototype(font.getFamily());
    }

    @Override
    public String toString() {
        return "FontPrototype{" +
                "family='" + family + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof FontPrototype)) return false;
        FontPrototype that = (FontPrototype) o;
        return Objects.equals(family, that.family);
    }

    @Override
    public int hashCode() {
        return Objects.hash(family);
    }
}
