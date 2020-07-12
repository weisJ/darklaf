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
package com.github.weisj.darklaf.theme.laf;

import java.lang.reflect.InvocationTargetException;

import javax.swing.*;

import com.github.weisj.darklaf.theme.Theme;

public class ReflectiveDelegatingThemedLaf extends DelegatingThemedLaf {

    public ReflectiveDelegatingThemedLaf(final Theme theme, final String baseLafClass) {
        super(theme, getLaf(baseLafClass));
    }

    @Override
    public UIDefaults getDefaults() {
        return super.getDefaults();
    }

    private static ThemedLookAndFeel getLaf(final String baseLafClass) {
        try {
            Class<?> base = Class.forName(baseLafClass);
            if (!ThemedLookAndFeel.class.isAssignableFrom(base)) {
                throw new IllegalArgumentException(base + " is not of type " + ThemedLookAndFeel.class);
            }
            return (ThemedLookAndFeel) base.getDeclaredConstructor().newInstance();
        } catch (ClassNotFoundException | NoSuchMethodException
                 | InstantiationException | IllegalAccessException | InvocationTargetException e) {
            throw new RuntimeException(e);
        }
    }
}
