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
import java.util.logging.Logger;

import javax.swing.*;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.LogUtil;

public class PropertyFontMapper implements FontMapper {

    private static final Logger LOGGER = LogUtil.getLogger(PropertyFontMapper.class);

    private Theme lastTheme = null;
    private int adjustment;
    private String propertyKey;

    public PropertyFontMapper() {
        this(null);
    }

    public PropertyFontMapper(final String propertyKey) {
        this.propertyKey = propertyKey;
    }

    public void setPropertyKey(final String propertyKey) {
        this.propertyKey = propertyKey;
    }

    @Override
    public Font map(final Font font, final UIDefaults defaults) {
        if (propertyKey == null) return font;
        adjustment = getSize(defaults);
        // No need to create a new font.
        if (adjustment == 0) return font;
        if (adjustment + font.getSize2D() <= 0) {
            if (adjustment != Integer.MIN_VALUE) {
                LOGGER.warning("Font " + font + " would be invisible after applying "
                               + "an adjustment of " + adjustment + ". Aborting!");
            }
            return font;
        }
        return font.deriveFont(font.getSize2D() + adjustment);
    }

    private int getSize(final UIDefaults defaults) {
        // Use cached value if already queried.
        if (lastTheme == LafManager.getTheme()) return adjustment;
        lastTheme = LafManager.getTheme();
        Object obj = defaults.get(propertyKey);
        if (!(obj instanceof Integer)) {
            LOGGER.warning("Font size property '" + propertyKey + "' not specified. Mapper will do nothing");
            return Integer.MIN_VALUE;
        } else {
            return (int) obj;
        }
    }
}
