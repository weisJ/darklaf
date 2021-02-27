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
package com.github.weisj.darklaf.theme.info;

import java.util.Objects;

public class PreferredThemeStyle {

    private final ContrastRule contrastRule;
    private final ColorToneRule colorToneRule;
    private final FontSizeRule fontSizeRule;
    private final AccentColorRule accentColorRule;

    public PreferredThemeStyle() {
        this(ContrastRule.STANDARD, ColorToneRule.LIGHT);
    }

    public PreferredThemeStyle(final ContrastRule contrastRule, final ColorToneRule colorToneRule) {
        if (contrastRule == null) throw new IllegalArgumentException("null is not a valid contrast rule");
        if (colorToneRule == null) throw new IllegalArgumentException("null is not a valid style rule");
        this.contrastRule = contrastRule;
        this.colorToneRule = colorToneRule;
        this.fontSizeRule = FontSizeRule.getDefault();
        this.accentColorRule = AccentColorRule.getDefault();
    }

    public PreferredThemeStyle(final ContrastRule contrastRule, final ColorToneRule colorToneRule,
            final AccentColorRule accentColorRule) {
        if (contrastRule == null) throw new IllegalArgumentException("null is not a valid contrast rule");
        if (colorToneRule == null) throw new IllegalArgumentException("null is not a valid style rule");
        if (accentColorRule == null) throw new IllegalArgumentException("null is not a valid accent color rule");
        this.contrastRule = contrastRule;
        this.colorToneRule = colorToneRule;
        this.fontSizeRule = FontSizeRule.getDefault();
        this.accentColorRule = accentColorRule;
    }

    public PreferredThemeStyle(final ContrastRule contrastRule, final ColorToneRule colorToneRule,
            final FontSizeRule fontSizeRule) {
        if (contrastRule == null) throw new IllegalArgumentException("null is not a valid contrast rule");
        if (colorToneRule == null) throw new IllegalArgumentException("null is not a valid style rule");
        if (fontSizeRule == null) throw new IllegalArgumentException("null is not a valid font size rule");
        this.contrastRule = contrastRule;
        this.colorToneRule = colorToneRule;
        this.fontSizeRule = fontSizeRule;
        this.accentColorRule = AccentColorRule.getDefault();
    }

    public PreferredThemeStyle(final ContrastRule contrastRule, final ColorToneRule colorToneRule,
            final AccentColorRule accentColorRule, final FontSizeRule fontSizeRule) {
        if (contrastRule == null) throw new IllegalArgumentException("null is not a valid contrast rule");
        if (colorToneRule == null) throw new IllegalArgumentException("null is not a valid style rule");
        if (fontSizeRule == null) throw new IllegalArgumentException("null is not a valid font size rule");
        if (accentColorRule == null) throw new IllegalArgumentException("null is not a valid accent color rule");
        this.contrastRule = contrastRule;
        this.colorToneRule = colorToneRule;
        this.fontSizeRule = fontSizeRule;
        this.accentColorRule = accentColorRule;
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

    public AccentColorRule getAccentColorRule() {
        return accentColorRule;
    }

    @Override
    public String toString() {
        return "PreferredThemeStyle{" + "contrastRule=" + contrastRule + ", colorToneRule=" + colorToneRule
                + ", accentColorRule=" + accentColorRule + ", fontSizeRule=" + fontSizeRule + '}';
    }

    @Override
    public boolean equals(final Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        PreferredThemeStyle that = (PreferredThemeStyle) o;
        return contrastRule == that.contrastRule && colorToneRule == that.colorToneRule
                && Objects.equals(fontSizeRule, that.fontSizeRule)
                && Objects.equals(accentColorRule, that.accentColorRule);
    }

    @Override
    public int hashCode() {
        int result = contrastRule != null ? contrastRule.hashCode() : 0;
        result = 31 * result + (colorToneRule != null ? colorToneRule.hashCode() : 0);
        result = 31 * result + (fontSizeRule != null ? fontSizeRule.hashCode() : 0);
        result = 31 * result + (accentColorRule != null ? accentColorRule.hashCode() : 0);
        return result;
    }
}
