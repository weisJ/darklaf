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
package com.github.weisj.darklaf.components.border;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.util.Map;
import java.util.WeakHashMap;

public final class DarkBorders {

    private static final WeakLineBorder KEY = new WeakLineBorder(0, 0, 0, 0);
    private static Map<WeakLineBorder, WeakLineBorder> lineBorderMap = new WeakHashMap<>();
    private static Map<WeakLineBorder, WeakLineBorder> lineWidgetBorderMap = new WeakHashMap<>();


    public static Border createLineBorder(final int top, final int left, final int bottom, final int right) {
        return createBorder(top, left, bottom, right, lineBorderMap, "border");
    }


    private static Border createBorder(final int top, final int left, final int bottom, final int right,
                                       final Map<WeakLineBorder, WeakLineBorder> map, final String key) {
        WeakLineBorder border = null;
        KEY.setInsets(top, left, bottom, right);
        if (map.containsKey(KEY)) {
            border = map.get(KEY);
        }
        if (border == null) {
            border = new WeakLineBorder(top, left, bottom, right);
            map.put(KEY, border);
        }
        border.setColor(UIManager.getColor(key));
        return border;
    }


    public static Border createWidgetLineBorder(final int top, final int left, final int bottom, final int right) {
        return createBorder(top, left, bottom, right, lineWidgetBorderMap, "borderSecondary");
    }

    public static void update(final UIDefaults defaults) {
        Color borderColor = defaults.getColor("border");
        for (WeakLineBorder border : lineBorderMap.values()) {
            border.setColor(borderColor);
        }
        Color borderSecondaryColor = defaults.getColor("borderSecondary");
        for (WeakLineBorder border : lineWidgetBorderMap.values()) {
            border.setColor(borderSecondaryColor);
        }
    }
}
