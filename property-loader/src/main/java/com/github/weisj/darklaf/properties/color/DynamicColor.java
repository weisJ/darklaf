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
package com.github.weisj.darklaf.properties.color;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.util.ColorWrapper;

public class DynamicColor extends ColorWrapper {

    private static Object currentDynamicKey;

    public static void setDynamicKey(final Object obj) {
        currentDynamicKey = obj;
    }

    protected Object currentKey;
    private final String key;

    public DynamicColor() {
        this(null);
    }

    public DynamicColor(final String key) {
        super(null);
        this.key = key;
    }

    public String getKey() {
        return key;
    }

    protected boolean isColorSet() {
        return !DEFAULT_COLOR.equals(super.getColor());
    }

    protected Object getCurrentDynamicKey() {
        return currentDynamicKey;
    }

    protected Object getCurrentKey() {
        return currentKey;
    }

    protected void setCurrentKey(final Object currentKey) {
        this.currentKey = currentKey;
    }

    protected void ensureCorrectColor() {
        if (!isColorSet() || getCurrentKey() != getCurrentDynamicKey()) {
            setCurrentKey(getCurrentDynamicKey());
            setColor(getUpdatedColor());
        }
    }

    @Override
    public Color getColor() {
        ensureCorrectColor();
        return super.getColor();
    }

    protected Color getUpdatedColor() {
        return UIManager.getColor(key);
    }
}
