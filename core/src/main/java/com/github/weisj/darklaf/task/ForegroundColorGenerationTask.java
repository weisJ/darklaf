/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.task;

import java.awt.*;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

import javax.swing.plaf.ColorUIResource;

import com.github.weisj.darklaf.color.ColorUtil;
import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.color.DarkColorModelHSL;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Types;

public class ForegroundColorGenerationTask extends ColorAdjustmentTask {


    private static final Logger LOGGER = LogUtil.getLogger(ForegroundColorGenerationTask.class);
    private static final String FOREGROUND_LIST_KEY = "selectionForeground.propertyList";
    private static final String ACCENT_LIST_KEY = "accentForeground.propertyList";

    private static final double DEFAULT_COLOR_ADJUSTED_BG_THRESHOLD = 0.552;
    private static final double DEFAULT_COLOR_ADJUSTED_FG_DIFFERENCE = 0.8;
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
            adjust(ACCENT_LIST_KEY, props, map -> adjustForegroundList(map, properties));
        }
        if (accentColorRule.getSelectionColor() != null && currentTheme.supportsCustomSelectionColor()) {
            adjust(FOREGROUND_LIST_KEY, props, map -> adjustForegroundList(map, properties));
        }
    }

    private void adjustForegroundList(final Map<?, ?> map, final Properties properties) {
        map.entrySet().stream()
                .filter(e -> e.getValue() instanceof Color)
                .forEach(e -> {
                    Object targets = e.getKey();
                    if (targets instanceof String) {
                        adjustForeground(targets.toString(), (Color) e.getValue(), properties);
                    } else if (targets instanceof List) {
                        for (Object target : (List<?>) targets) {
                            if (target instanceof String) {
                                adjustForeground(target.toString(), (Color) e.getValue(), properties);
                            } else {
                                LOGGER.warning("Invalid target " + target);
                            }
                        }
                    } else {
                        LOGGER.warning("Invalid target declaration " + targets);
                    }
                });
    }

    private void adjustForeground(final String fgKey, final Color bg, final Properties properties) {
        Color suggestion = Types.safeCast(properties.get(fgKey), Color.class);
        Color c = makeAdjustedForeground(suggestion, bg, Bias.BACKGROUND,
                DEFAULT_COLOR_ADJUSTED_FG_DIFFERENCE, DEFAULT_COLOR_ADJUSTED_BG_THRESHOLD);
        properties.put(fgKey, c);
    }


    public static ColorUIResource makeForeground(final Color bg) {
        return makeForeground(bg, MIN_FOREGROUND_DIFFERENCE);
    }

    public static ColorUIResource makeForeground(final Color bg, final double minimumBrightnessDifference) {
        return makeAdjustedForeground(null, bg, Bias.BACKGROUND, minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg) {
        return makeAdjustedForeground(fg, bg, MIN_FOREGROUND_DIFFERENCE);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fg, final Color bg,
            final double minimumBrightnessDifference) {
        return makeAdjustedForeground(fg, bg, Bias.BACKGROUND, minimumBrightnessDifference);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fgSuggestion, final Color bg, final Bias bias,
            final double minimumBrightnessDifference) {
        return makeAdjustedForeground(fgSuggestion, bg, bias, minimumBrightnessDifference,
                DEFAULT_COLOR_ADJUSTED_BG_THRESHOLD);
    }

    public static ColorUIResource makeAdjustedForeground(final Color fgSuggestion, final Color bg, final Bias bias,
            final double minimumBrightnessDifference, final double backgroundBiasThreshold) {
        Color fg = fgSuggestion;
        if (fg == null) {
            double[] hsbBG = DarkColorModelHSB.RGBtoHSBValues(bg.getRed(), bg.getGreen(), bg.getBlue());
            fg = DarkColorModelHSB.getColorFromHSBValues(hsbBG[0], 0, 1 - hsbBG[2]);
        }

        final double[] hslFG = DarkColorModelHSL.RGBtoHSLValues(fg.getRed(), fg.getGreen(), fg.getBlue());
        final double[] hslBG = DarkColorModelHSL.RGBtoHSLValues(bg.getRed(), bg.getGreen(), bg.getBlue());
        double bgBrightness = hslBG[2];
        double fgBrightness = hslFG[2];

        Bias b = bias != null ? bias : Bias.BACKGROUND;

        if (b == Bias.BACKGROUND) {
            double bgBright = ColorUtil.getLuminance(bg);
            double threshold = backgroundBiasThreshold < 0 ? 0.5 : backgroundBiasThreshold;
            b = bgBright <= threshold ? Bias.WHITE : Bias.BLACK;
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
        BLACK
    }
}
