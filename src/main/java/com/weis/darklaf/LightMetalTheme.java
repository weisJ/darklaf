/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
 */
package com.weis.darklaf;

import org.jetbrains.annotations.NotNull;

import javax.swing.plaf.ColorUIResource;
import javax.swing.plaf.metal.DefaultMetalTheme;
import java.awt.*;

/**
 * Metal theme with light colours for {@link LightLaf}.
 */
public class LightMetalTheme extends DefaultMetalTheme {
    public static final ColorUIResource primary1 = new ColorUIResource(10, 36, 106);
    private static final ColorUIResource darkGray = new ColorUIResource(132, 130, 132);
    private static final ColorUIResource white = new ColorUIResource(255, 255, 255);
    private static final ColorUIResource darkBlue = new ColorUIResource(82, 108, 164);
    //      private static ColorUIResource lightGray = new ColorUIResource(214, 211, 206);
    private static final ColorUIResource lightGray = new ColorUIResource(214, 214, 214);
    private static final ColorUIResource primary2 = new ColorUIResource(91, 135, 206);
    private static final ColorUIResource primary3 = new ColorUIResource(166, 202, 240);

    @NotNull
    @Override
    public String getName() {
        return "Light Metal Theme";
    }

    @NotNull
    @Override
    protected ColorUIResource getPrimary1() {
        return primary1;
    }

    @NotNull
    @Override
    protected ColorUIResource getPrimary2() {
        return primary2;
    }

    @NotNull
    @Override
    protected ColorUIResource getPrimary3() {
        return primary3;
    }

    @NotNull
    @Override
    public ColorUIResource getFocusColor() {
        return new ColorUIResource(Color.black);
    }

    @NotNull
    @Override
    public ColorUIResource getControl() {
        return lightGray;
    }

    @NotNull
    @Override
    public ColorUIResource getMenuBackground() {
        return lightGray;
    }

    @NotNull
    @Override
    public ColorUIResource getMenuSelectedBackground() {
        return darkBlue;
    }

    @NotNull
    @Override
    public ColorUIResource getMenuSelectedForeground() {
        return white;
    }

    @NotNull
    @Override
    public ColorUIResource getSeparatorBackground() {
        return white;
    }

    @NotNull
    @Override
    public ColorUIResource getSeparatorForeground() {
        return darkGray;
    }

    @NotNull
    @Override
    public ColorUIResource getAcceleratorSelectedForeground() {
        return white;
    }
}
