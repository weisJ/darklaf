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
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.ColorUtil;
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

    public static ColorUIResource makeForeground(final Color c, final double minimumBrightnessDifference) {
        double[] hsbBG = DarkColorModelHSB.RGBtoHSBValues(c.getRed(), c.getGreen(), c.getBlue());
        double[] hsbFG = new double[]{hsbBG[0], 0, 1 - hsbBG[2]};
        return makeAdjustedForeground(hsbFG, hsbBG, Bias.BACKGROUND, minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg,
                                                         final double minimumBrightnessDifference) {
        return makeAdjustedForeground(fg, bg, Bias.BACKGROUND, minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg, final Bias bias,
                                                         final double minimumBrightnessDifference) {
        double[] hsbBG = DarkColorModelHSB.RGBtoHSBValues(bg.getRed(), bg.getGreen(), bg.getBlue());
        double[] hsbFG = DarkColorModelHSB.RGBtoHSBValues(fg.getRed(), fg.getGreen(), fg.getBlue());
        return makeAdjustedForeground(hsbFG, hsbBG, bias, minimumBrightnessDifference);
    }

    private static ColorUIResource makeAdjustedForeground(final double[] hsbFG, final double[] hsbBG, final Bias bias,
                                                          final double minimumBrightnessDifference) {
        double bgBrightness = hsbBG[2];
        double fgBrightness = hsbFG[2];

        Bias b = bias != null ? bias : Bias.BACKGROUND;
        if (b == Bias.BACKGROUND) {
            Color c = DarkColorModelHSB.getColorFromHSBValues(hsbBG[0], hsbBG[1], hsbBG[2]);
            double bgBright = (1 - hsbBG[1]) * ColorUtil.getPerceivedBrightness(c);
            b = bgBright <= 127.5f ? Bias.WHITE : Bias.BLACK;
        }

        double difference = Math.abs(fgBrightness - bgBrightness);
        if (difference < minimumBrightnessDifference) {
            fgBrightness += b.direction * (Math.abs(minimumBrightnessDifference - difference));
        }
        return new DarkColorUIResource(DarkColorModelHSB.getColorFromHSBValues(hsbFG[0], hsbFG[1], fgBrightness));
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
