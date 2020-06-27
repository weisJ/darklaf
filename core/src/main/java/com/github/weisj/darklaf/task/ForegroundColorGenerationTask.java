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
package com.github.weisj.darklaf.task;

import java.awt.*;
import java.util.List;
import java.util.Properties;

import javax.swing.plaf.ColorUIResource;

import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.color.DarkColorModelHSL;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.Pair;

public class ForegroundColorGenerationTask extends ColorAdjustmentTask {

    private static final String FOREGROUND_LIST_KEY = "selectionForeground.propertyList";
    private static final String ACCENT_LIST_KEY = "accentForeground.propertyList";
    private static final double MIN_FOREGROUND_DIFFERENCE = 0.4;

    @Override
    protected void beforeTask(final Theme currentTheme, final Properties properties) {
        super.beforeTask(currentTheme, properties);
        DEFAULTS.putAll(properties);
    }

    @Override
    protected void runTask(final Theme currentTheme, final Properties properties) {
        Properties props = currentTheme.loadPropertyFile("accents", true);
        AccentColorRule accentColorRule = currentTheme.getAccentColorRule();
        if (accentColorRule.getAccentColor() != null && currentTheme.supportsCustomAccentColor()) {
            adjust(ACCENT_LIST_KEY, props, list -> adjustForegroundList(list, properties));
        }
        if (accentColorRule.getSelectionColor() != null && currentTheme.supportsCustomSelectionColor()) {
            adjust(FOREGROUND_LIST_KEY, props, list -> adjustForegroundList(list, properties));
        }
    }

    private void adjustForegroundList(final List<?> list, final Properties properties) {
        list.stream()
            .filter(o -> o instanceof Pair<?, ?>)
            .map(Pair.class::cast)
            .filter(p -> p.getFirst() instanceof Color)
            .forEach(p -> properties.put(p.getSecond(), makeForeground((Color) p.getFirst(),
                                                                       MIN_FOREGROUND_DIFFERENCE)));
    }

    public static ColorUIResource makeForeground(final Color bg, final double minimumBrightnessDifference) {
        double[] hsbBG = DarkColorModelHSB.RGBtoHSBValues(bg.getRed(), bg.getGreen(), bg.getBlue());
        Color fg = DarkColorModelHSB.getColorFromHSBValues(hsbBG[0], 0, 1 - hsbBG[2]);
        return makeAdjustedForeground(fg, bg, Bias.BACKGROUND, minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg,
                                                         final double minimumBrightnessDifference) {
        return makeAdjustedForeground(fg, bg, Bias.BACKGROUND, minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg, final Bias bias,
                                                         final double minimumBrightnessDifference) {
        final double[] hslFG = DarkColorModelHSL.RGBtoHSLValues(fg.getRed(), fg.getGreen(), fg.getBlue());
        final double[] hslBG = DarkColorModelHSL.RGBtoHSLValues(bg.getRed(), bg.getGreen(), bg.getBlue());
        double bgBrightness = hslBG[2];
        double fgBrightness = hslFG[2];

        Bias b = bias != null ? bias : Bias.BACKGROUND;

        if (b == Bias.BACKGROUND) {
            double bgBright = getLuminance(bg);
            b = bgBright <= 0.552 ? Bias.WHITE : Bias.BLACK;
        }

        double bright1 = fgBrightness > bgBrightness && (fgBrightness - bgBrightness) >= minimumBrightnessDifference
                ? hslFG[2]
                : Math.min(bgBrightness + minimumBrightnessDifference, 1);
        double bright2 = fgBrightness < bgBrightness && (bgBrightness - fgBrightness) >= minimumBrightnessDifference
                ? hslFG[2]
                : Math.max(bgBrightness - minimumBrightnessDifference, 0);

        double brightness = b == Bias.WHITE ? bright1 : bright2;
        return new DarkColorUIResource(DarkColorModelHSL.getColorFromHSLValues(hslFG[0], hslFG[1], brightness));
    }

    private static double getLuminance(final Color c) {
        return getLuminance(c.getRed(), c.getGreen(), c.getBlue());
    }

    private static double getLuminance(final int red, final int green, final int blue) {
        double r = red / 255.0;
        double g = green / 255.0;
        double b = blue / 255.0;
        double R = r <= 0.03928 ? r / 12.92 : Math.pow((r + 0.055) / 1.055, 2.4);
        double G = g <= 0.03928 ? g / 12.92 : Math.pow((g + 0.055) / 1.055, 2.4);
        double B = b <= 0.03928 ? b / 12.92 : Math.pow((b + 0.055) / 1.055, 2.4);
        return 0.2126 * R + 0.7152 * G + 0.0722 * B;
    }

    private static double calculateContrast(final double fgBrightness, final double bgBrightness) {
        double bright = Math.max(fgBrightness, bgBrightness);
        double dark = Math.min(fgBrightness, bgBrightness);
        return (bright + 0.05) / (dark + 0.05);
    }

    public enum Bias {
        BACKGROUND(0),
        WHITE(1),
        BLACK(-1);

        private final int direction;

        Bias(final int direction) {
            this.direction = direction;
        }
    }
}
