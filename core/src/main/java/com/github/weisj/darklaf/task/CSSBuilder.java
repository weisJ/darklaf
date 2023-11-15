/*
 * MIT License
 *
 * Copyright (c) 2020-2023 Jannis Weis
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
package com.github.weisj.darklaf.task;

class CSSBuilder {
    private final StringBuilder b = new StringBuilder();
    private boolean groupOpen = false;

    public CSSGroup group(final String name) {
        return nextGroup(name);
    }

    private CSSBuilder closeGroup() {
        if (groupOpen) {
            b.append("}");
            groupOpen = false;
        }
        return this;
    }

    private CSSGroup nextGroup(final String name) {
        closeGroup();
        b.append(name).append(" {");
        groupOpen = true;
        return new CSSGroup(this);
    }

    private void addProperty(final String key, final String value) {
        b.append(key).append(":").append(value).append(";");
    }

    @Override
    public String toString() {
        return b.toString();
    }

    protected record CSSGroup(CSSBuilder builder) {

        public CSSBuilder end() {
            return builder.closeGroup();
        }

        public CSSGroup group(final String name) {
            return builder.nextGroup(name);
        }

        public CSSGroup property(final String key, final Object value) {
            builder.addProperty(key, value.toString());
            return this;
        }

        public CSSGroup color(final String value) {
            return property("color", value);
        }

        public CSSGroup border(final Object value) {
            return property("border", value);
        }

        public CSSGroup borderColor(final String value) {
            return property("border-color", value);
        }

        public CSSGroup borderStyle(final String value) {
            return property("border-style", value);
        }

        public CSSGroup borderWidth(final String value) {
            return property("border-width", value);
        }

        public CSSGroup padding(final Object value) {
            return property("padding", value);
        }

        public CSSGroup borderCollapse(final String value) {
            return property("border-collapse", value);
        }

        public CSSGroup fontSize(final Object value) {
            return property("font-size", value);
        }

        public CSSGroup fontWeight(final String value) {
            return property("font-weight", value);
        }

        public CSSGroup fontStyle(final String value) {
            return property("font-style", value);
        }

        public CSSGroup textDecoration(final String value) {
            return property("text-decoration", value);
        }

        public CSSGroup fontFamily(final String family, final String name) {
            return property("font-family", family + ", " + name);
        }

        public CSSGroup marginTop(final Object value) {
            return property("margin-top", value);
        }

        public CSSGroup marginBottom(final Object value) {
            return property("margin-bottom", value);
        }

        public CSSGroup marginLeft(final Object value) {
            return property("margin-left", value);
        }

        public CSSGroup marginRight(final Object value) {
            return property("margin-right", value);
        }

        public CSSGroup marginLeftLTR(final Object value) {
            return property("margin-left-ltr", value);
        }

        public CSSGroup marginRightRTL(final Object value) {
            return property("margin-right-rtl", value);
        }

        public CSSGroup listStyleType(final String value) {
            return property("list-style-type", value);
        }

        public CSSGroup margin(final Object value) {
            return property("margin", value);
        }
    }
}
