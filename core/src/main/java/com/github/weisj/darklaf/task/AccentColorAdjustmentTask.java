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
import java.util.logging.Logger;

import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.Pair;

public class AccentColorAdjustmentTask extends ColorAdjustmentTask {

    private static final Logger LOGGER = LogUtil.getLogger(AccentColorAdjustmentTask.class);
    private static final String MAIN_ACCENT_LIST_KEY = "accent.propertyList";
    private static final String SELECTION_ACCENT_LIST_KEY = "selection.propertyList";

    @Override
    protected void runTask(final Theme currentTheme, final Properties properties) {
        Color accentColor = currentTheme.getAccentColorRule().getAccentColor();
        Color selectionColor = currentTheme.getAccentColorRule().getSelectionColor();
        applyColors(currentTheme, properties, accentColor, selectionColor);
    }

    public void applyColors(final Theme currentTheme, final Properties properties,
                            final Color accentColor, final Color selectionColor) {
        Properties props = currentTheme.loadAccentProperties();
        if (props == null || props.isEmpty()) return;
        if (accentColor != null) {
            adjustColors(MAIN_ACCENT_LIST_KEY, accentColor, props, properties);
        }
        if (selectionColor != null) {
            adjustColors(SELECTION_ACCENT_LIST_KEY, selectionColor, props, properties);
        }
    }

    private void adjustColors(final String listKey, final Color c,
                              final Properties listProperties, final Properties properties) {
        adjust(listKey, listProperties, list -> {
            double[] hsb = DarkColorModelHSB.RGBtoHSBValues(c.getRed(), c.getGreen(), c.getBlue());
            adjustColorList(list, hsb, properties);
        });
    }

    private void adjustColorList(final List<?> list, final double[] hsb,
                                 final Properties properties) {
        ColorInfo info = new ColorInfo();
        for (Object o : list) {
            setColorInfo(o, info);
            if (info.key == null) continue;
            Object c = mapColor(info, hsb, properties);
            if (c instanceof Color) {
                properties.put(info.key, c);
            } else {
                LOGGER.warning("Color with key '" + info.key
                               + "' could not be adjusted because the value '" + c + "' is not a color");
            }
        }
    }

    private void setColorInfo(final Object o, final ColorInfo info) {
        info.set(null, 0, 0, 0);
        if (o instanceof String) {
            info.set(o.toString(), 100, 100, 100);
            return;
        }
        if (o instanceof Pair<?, ?>) {
            Object first = ((Pair<?, ?>) o).getFirst();
            Object second = ((Pair<?, ?>) o).getSecond();
            if (!(first instanceof String)) return;
            if (!(second instanceof List<?>)) return;
            String key = first.toString();
            List<?> list = (List<?>) second;
            if (list.size() != 3
                || !(list.get(0) instanceof Integer)
                || !(list.get(1) instanceof Integer)
                || !(list.get(2) instanceof Integer)) {
                return;
            }
            info.set(key, (Integer) list.get(0), (Integer) list.get(1), (Integer) list.get(2));
        }
    }

    private Object mapColor(final ColorInfo info, final double[] hsbMatch, final Properties properties) {
        Object obj = properties.get(info.key);
        if (obj instanceof Color) {
            Color color = DarkColorModelHSB.getColorFromHSBValues(mapValue(hsbMatch[0], info.hAdj),
                                                                  mapValue(hsbMatch[1], info.sAdj),
                                                                  mapValue(hsbMatch[2], info.bAdj));
            return new DarkColorUIResource(color);
        }
        return obj;
    }

    private double mapValue(final double value, final int adjustment) {
        return value * (adjustment / 100.0);
    }

    private static class ColorInfo {
        private String key;
        private int hAdj;
        private int sAdj;
        private int bAdj;

        private void set(final String key, final int hAdj, final int sAdj, final int bAdj) {
            this.key = key;
            this.hAdj = hAdj;
            this.sAdj = sAdj;
            this.bAdj = bAdj;
        }
    }
}
