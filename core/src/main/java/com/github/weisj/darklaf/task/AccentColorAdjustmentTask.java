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

import javax.swing.*;
import javax.swing.plaf.ColorUIResource;

import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.color.DarkColorModelHSB;
import com.github.weisj.darklaf.icons.IconLoader;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.Pair;

public class AccentColorAdjustmentTask implements DefaultsAdjustmentTask {

    private static final Logger LOGGER = Logger.getLogger(AccentColorAdjustmentTask.class.getName());
    private static final String MAIN_ACCENT_LIST_KEY = "mainAccent.propertyList";
    private static final String SELECTION_ACCENT_LIST_KEY = "selectionAccent.propertyList";
    private static final UIDefaults DEFAULTS = new UIDefaults();

    @Override
    public void run(final Theme currentTheme, final Properties properties) {
        Color accentColor = currentTheme.getAccentColorRule().getAccentColor();
        Color selectionColor = currentTheme.getAccentColorRule().getSelectionColor();
        applyColors(currentTheme, properties, accentColor, selectionColor);
    }

    public void applyColors(final Theme currentTheme, final Properties properties,
                            final Color accentColor, final Color selectionColor) {
        Properties props = currentTheme.loadPropertyFile("accents", true);
        if (props.isEmpty()) return;
        adjust(MAIN_ACCENT_LIST_KEY, accentColor, props, properties);
        adjust(SELECTION_ACCENT_LIST_KEY, selectionColor, props, properties);
    }

    private void adjust(final String listKey, final Color c,
                        final Properties listProperties, final Properties properties) {
        if (c == null) return;
        Object obj = PropertyLoader.parseValue(listKey,
                                               listProperties.getProperty(listKey),
                                               listProperties, DEFAULTS, IconLoader.get());
        DEFAULTS.clear();
        if (obj instanceof List<?>) {
            double[] hsb = DarkColorModelHSB.RGBtoHSBValues(c.getRed(), c.getGreen(), c.getBlue());
            adjustList((List<?>) obj, hsb, properties);
        }
    }

    private void adjustList(final List<?> list, final double[] hsb,
                            final Properties properties) {
        ColorInfo info = new ColorInfo();
        for (Object o : list) {
            info = getColorInfo(o, info);
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

    private ColorInfo getColorInfo(final Object o, final ColorInfo info) {
        info.set(null, 0, 0, 0);
        if (o instanceof String) {
            info.set(o.toString(), 100, 100, 100);
            return info;
        }
        if (o instanceof Pair<?, ?>) {
            Object first = ((Pair<?, ?>) o).getFirst();
            Object second = ((Pair<?, ?>) o).getSecond();
            if (!(first instanceof String)) return null;
            if (!(second instanceof List<?>)) return null;
            String key = first.toString();
            List<?> list = (List<?>) second;
            if (list.size() != 3
                || !(list.get(0) instanceof Integer)
                || !(list.get(1) instanceof Integer)
                || !(list.get(2) instanceof Integer)) {
                return info;
            }
            info.set(key, (Integer) list.get(0), (Integer) list.get(1), (Integer) list.get(2));
            return info;
        }
        return info;
    }

    private Object mapColor(final ColorInfo info, final double[] hsbMatch, final Properties properties) {
        Object obj = properties.get(info.key);
        if (obj instanceof Color) {
            Color color = DarkColorModelHSB.getColorFromHSBValues(mapValue(hsbMatch[0], info.hAdj),
                                                                  mapValue(hsbMatch[1], info.sAdj),
                                                                  mapValue(hsbMatch[2], info.bAdj));
            return new ColorUIResource(color);
        }
        return obj;
    }

    private double mapValue(final double value, final int adjustment) {
        if (adjustment >= 0) {
            return value * (adjustment / 100.0);
        } else {
            return 1 - (value * (Math.abs(adjustment) / 100.0));
        }
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
