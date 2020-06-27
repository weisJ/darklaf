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
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.Pair;

public class ForegroundColorGenerationTask extends ColorAdjustmentTask {

    private static final String FOREGROUND_LIST_KEY = "selectionForeground.propertyList";
    private static final String ACCENT_LIST_KEY = "accentForeground.propertyList";
    private static final double MIN_FOREGROUND_DIFFERENCE = 0.5;

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
            .forEach(p -> properties.put(p.getSecond(), makeForeground((Color) p.getFirst())));
    }

    public static ColorUIResource makeForeground(final Color bg) {
        return makeForeground(bg, MIN_FOREGROUND_DIFFERENCE);
    }

    public static ColorUIResource makeForeground(final Color bg, final double minimumBrightnessDifference) {
        double[] hsbBG = DarkColorModelHSB.RGBtoHSBValues(bg.getRed(), bg.getGreen(), bg.getBlue());
        Color fg = DarkColorModelHSB.getColorFromHSBValues(hsbBG[0], 0, 1 - hsbBG[2]);
        return makeAdjustedForeground(fg, bg, Bias.getBackground(), minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg,
                                                         final double minimumBrightnessDifference) {
        return makeAdjustedForeground(fg, bg, Bias.getBackground(), minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg, final Bias bias,
                                                         final double minimumBrightnessDifference) {
        final double[] hslFG = DarkColorModelHSL.RGBtoHSLValues(fg.getRed(), fg.getGreen(), fg.getBlue());
        final double[] hslBG = DarkColorModelHSL.RGBtoHSLValues(bg.getRed(), bg.getGreen(), bg.getBlue());
        double bgBrightness = hslBG[2];
        double fgBrightness = hslFG[2];

        Bias b = bias != null ? bias : Bias.getBackground();

        if (b == Bias.BACKGROUND) {
            double bgBright = ColorUtil.getLuminance(bg);
            b = bgBright <= b.threshold ? Bias.WHITE : Bias.BLACK;
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

    public enum Bias {
        BACKGROUND,
        WHITE,
        BLACK;

        public static Bias getBackground() {
            return getBackground(0.5);
        }

        public static Bias getBackground(final double threshold) {
            BACKGROUND.setThreshold(threshold);
            return BACKGROUND;
        }

        private double threshold = 0.5;

        public void setThreshold(final double threshold) {
            this.threshold = threshold;
        }
    }
}
