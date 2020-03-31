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

import javax.swing.*;
import java.util.logging.Logger;

public enum FontSizePreset {
    TINY("tiny"),
    SMALLER("smaller"),
    SMALL("small"),
    MEDIUM("medium"),
    LARGE("large"),
    LARGER("larger"),
    HUGE("huge");

    private static final Logger LOGGER = Logger.getLogger(FontSizePreset.class.getName());
    private final String propertyName;
    private final FontSizeRule.AdjustmentType type;

    FontSizePreset(final String propertyName) {
        this(propertyName, FontSizeRule.AdjustmentType.ABSOLUTE_ADJUSTMENT);
    }

    FontSizePreset(final String propertyName, final FontSizeRule.AdjustmentType type) {
        this.propertyName = propertyName;
        this.type = type;
    }

    public float adjustFontSize(final float size, final UIDefaults defaults) {
        int adjustment = getSize(defaults);
        return type.adjustSize(size, adjustment, adjustment / 100f);
    }

    private String getPropertyName() {
        return "fontSize." + propertyName;
    }

    public FontSizeRule.AdjustmentType getType() {
        return type;
    }

    private int getSize(final UIDefaults defaults) {
        String key = getPropertyName();
        Object obj = defaults.get(getPropertyName());
        if (!(obj instanceof Integer)) {
            LOGGER.warning("Font size property '" + key + "' not specified. Font will not be changed");
            return Integer.MAX_VALUE;
        } else {
            return (int) obj;
        }
    }
}
