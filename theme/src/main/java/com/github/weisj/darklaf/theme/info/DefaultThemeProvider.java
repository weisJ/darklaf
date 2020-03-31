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
package com.github.weisj.darklaf.theme.info;

import com.github.weisj.darklaf.theme.*;

public class DefaultThemeProvider implements ThemeProvider {

    private final Theme lightTheme;
    private final Theme darkTheme;
    private final Theme lightHighContrastTheme;
    private final Theme darkHighContrastTheme;

    public DefaultThemeProvider() {
        this(new IntelliJTheme(), new DarculaTheme(),
             new HighContrastLightTheme(), new HighContrastDarkTheme());
    }

    public DefaultThemeProvider(final Theme lightTheme,
                                final Theme darkTheme,
                                final Theme lightHighContrastTheme,
                                final Theme darkHighContrastTheme) {
        /*
         * Ensure the given themes actually serve the purpose they are intended for.
         */
        if (Theme.isDark(lightTheme)) {
            throw new UnsupportedThemeException("Given light theme " + lightTheme
                                                + "is declared as dark");
        }
        if (Theme.isDark(lightHighContrastTheme)) {
            throw new UnsupportedThemeException("Given light high-contrast theme " + lightHighContrastTheme
                                                + " is declared as dark");
        }
        if (!Theme.isDark(darkTheme)) {
            throw new UnsupportedThemeException("Given dark theme " + darkTheme
                                                + "is not declared as dark");
        }
        if (!Theme.isDark(darkHighContrastTheme)) {
            throw new UnsupportedThemeException("Given dark high-contrast theme " + darkTheme
                                                + "is not declared as dark");
        }
        /*
         * A high contrast theme may serve as a standard theme, but a standard theme should never be used
         * for high contrast themes.
         */
        if (!Theme.isHighContrast(lightHighContrastTheme)) {
            throw new UnsupportedThemeException("Given light high-contrast theme " + lightHighContrastTheme
                                                + " is not declared as high-contrast");
        }
        if (!Theme.isHighContrast(darkHighContrastTheme)) {
            throw new UnsupportedThemeException("Given dark high-contrast theme " + darkHighContrastTheme
                                                + " is not declared as high-contrast");
        }
        this.lightTheme = lightTheme;
        this.darkTheme = darkTheme;
        this.lightHighContrastTheme = lightHighContrastTheme;
        this.darkHighContrastTheme = darkHighContrastTheme;
    }


    @Override
    public Theme getTheme(final PreferredThemeStyle themeStyle) {
        if (themeStyle == null) return lightTheme;
        boolean dark = themeStyle.getColorToneRule() == ColorToneRule.DARK;
        boolean highContrast = themeStyle.getContrastRule() == ContrastRule.HIGH_CONTRAST;
        Theme theme = dark ? highContrast ? darkHighContrastTheme : darkTheme
                           : highContrast ? lightHighContrastTheme : lightTheme;
        // Apply the font size.
        theme.setFontSizeRule(themeStyle.getFontSizeRule());
        return theme;
    }
}
