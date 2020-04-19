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

import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.AccentColorRule;
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
        if (accentColorRule.getAccentColor() != null) {
            adjust(ACCENT_LIST_KEY, props, list -> adjustForegroundList(list, properties));
        }
        if (accentColorRule.getSelectionColor() != null) {
            adjust(FOREGROUND_LIST_KEY, props, list -> adjustForegroundList(list, properties));
        }
    }

    private void adjustForegroundList(final List<?> list, final Properties properties) {
        for (Object o : list) {
            if (!(o instanceof Pair<?, ?>)) continue;
            Pair<?, ?> pair = (Pair<?, ?>) o;
            if (!(pair.getFirst() instanceof Color)) continue;
            Color c = (Color) pair.getFirst();
            properties.put(pair.getSecond(), makeForeground(c));
        }
    }

    private Color makeForeground(final Color c) {
        double[] hsb = DarkColorModelHSB.RGBtoHSBValues(c.getRed(), c.getGreen(), c.getBlue());
        double b = hsb[2];
        double brightness = 1 - b;
        double difference = Math.abs(brightness - b);
        if (difference < MIN_FOREGROUND_DIFFERENCE) {
            int bias = b < 0.5 ? 1 : -1;
            brightness += bias * (Math.abs(MIN_FOREGROUND_DIFFERENCE - difference));
        }
        return DarkColorModelHSB.getColorFromHSBValues(hsb[0], 0, brightness);
    }
}
