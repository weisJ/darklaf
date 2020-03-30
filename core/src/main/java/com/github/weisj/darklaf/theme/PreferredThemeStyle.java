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
 */
package com.github.weisj.darklaf.theme;

public class PreferredThemeStyle {

    private final ContrastRule contrastRule;
    private final ColorToneRule colorToneRule;
    private final FontSizeRule fontSizeRule;

    public PreferredThemeStyle(final ContrastRule contrastRule,
                               final ColorToneRule colorToneRule,
                               final FontSizeRule fontSizeRule) {
        if (contrastRule == null) throw new IllegalArgumentException("null is not a valid contrast rule");
        if (colorToneRule == null) throw new IllegalArgumentException("null is not a valid style rule");
        if (fontSizeRule == null) throw new IllegalArgumentException("null is not a valid font size rule");
        this.contrastRule = contrastRule;
        this.colorToneRule = colorToneRule;
        this.fontSizeRule = fontSizeRule;
    }

    public ContrastRule getContrastRule() {
        return contrastRule;
    }

    public ColorToneRule getColorToneRule() {
        return colorToneRule;
    }

    public FontSizeRule getFontSizeRule() {
        return fontSizeRule;
    }

}
