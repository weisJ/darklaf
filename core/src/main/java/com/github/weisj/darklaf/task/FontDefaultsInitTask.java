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
package com.github.weisj.darklaf.task;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.PropertyLoader;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.FontSizeRule;
import com.github.weisj.darklaf.util.SystemInfo;

import javax.swing.*;
import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Logger;

public class FontDefaultsInitTask implements DefaultsInitTask {

    private static final Logger LOGGER = Logger.getLogger(FontDefaultsInitTask.class.getName());
    private static final String FONT_PROPERTY_PATH = "properties/";
    private static final String FONT_SIZE_DEFAULTS_NAME = "font_sizes";
    private static final String FONT_DEFAULTS_NAME = "font";

    private static final Map<AttributedCharacterIterator.Attribute, Integer> ENABLE_KERNING
        = Collections.singletonMap(TextAttribute.KERNING, TextAttribute.KERNING_ON);
    private static final Map<AttributedCharacterIterator.Attribute, Integer> DISABLE_KERNING
        = Collections.singletonMap(TextAttribute.KERNING, null);
    private static final String MAC_OS_CATALINA_FONT_NAME = ".AppleSystemUIFont";
    private static final String MAC_OS_FONT_NAME = ".SF NS Text";

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        loadFontProperties(defaults);
        if (SystemInfo.isMac) {
            patchMacOSFonts(defaults);
        }
        applyFontRule(currentTheme, defaults);
    }

    private void loadFontProperties(final UIDefaults defaults) {
        Properties fontSizeProps = PropertyLoader.loadProperties(DarkLaf.class,
                                                                 FONT_SIZE_DEFAULTS_NAME,
                                                                 FONT_PROPERTY_PATH);
        PropertyLoader.putProperties(fontSizeProps, defaults);
        Properties fontProps = PropertyLoader.loadProperties(DarkLaf.class,
                                                             FONT_DEFAULTS_NAME,
                                                             FONT_PROPERTY_PATH);
        PropertyLoader.putProperties(fontProps, defaults);
    }

    private void patchMacOSFonts(final UIDefaults defaults) {
        PropertyLoader.replacePropertiesOfType(Font.class, defaults, this::macOSFontFromFont);
    }

    private Font macOSFontFromFont(final Font font) {
        String fontName = SystemInfo.isMacOSCatalina ? MAC_OS_CATALINA_FONT_NAME : MAC_OS_FONT_NAME;
        Font macFont = new Font(fontName, font.getStyle(), font.getSize()).deriveFont(ENABLE_KERNING);
        if (font instanceof UIResource) {
            macFont = new FontUIResource(macFont);
        }
        return macFont == null ? font : macFont;
    }

    private void applyFontRule(final Theme currentTheme, final UIDefaults defaults) {
        FontSizeRule rule = currentTheme.getFontSizeRule();
        if (rule == null || rule.getType() == FontSizeRule.AdjustmentType.NO_ADJUSTMENT) return;
        PropertyLoader.replacePropertiesOfType(Font.class, defaults, f -> fontWithRule(f, rule));
    }

    private Font fontWithRule(final Font font, final FontSizeRule rule) {
        if (font == null) return null;
        float size = font.getSize2D();
        float newSize = rule.adjustFontSize(size);
        if (newSize == size) return font;
        if (newSize <= 0) {
            LOGGER.warning("Font " + font + " would be invisible after applying " + rule + ". Font won't be changed!");
            return font;
        }
        Font withRule = font.deriveFont(newSize);
        if (font instanceof UIResource
            && !(withRule instanceof UIResource)) {
            withRule = new FontUIResource(withRule);
        }
        return withRule;
    }
}
