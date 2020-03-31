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
import com.github.weisj.darklaf.theme.FontMapper;
import com.github.weisj.darklaf.theme.FontSizeRule;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.util.SystemInfo;

import javax.swing.plaf.FontUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.util.Collections;
import java.util.Map;
import java.util.Properties;

public class FontDefaultsInitTask implements DefaultsInitTask {

    private static final String FONT_PROPERTY_PATH = "properties/";
    private static final String FONT_SIZE_DEFAULTS_NAME = "font_sizes";
    private static final String FONT_DEFAULTS_NAME = "font_sizes";

    private static final String MAC_OS_CATALINA_FONT_NAME = ".AppleSystemUIFont";
    private static final String MAC_OS_FONT_NAME = ".SF NS Text";

    @Override
    public void run(final Theme currentTheme, final Map<Object, Object> defaults) {
        loadFontProperties(defaults);
        if (SystemInfo.isMac) {
            patchMacOSFonts(defaults);
        }
        applyFontRule(currentTheme, defaults);
    }

    private void loadFontProperties(final Map<Object, Object> defaults) {
        Properties fontSizeProps = PropertyLoader.loadProperties(DarkLaf.class,
                                                                 FONT_SIZE_DEFAULTS_NAME,
                                                                 FONT_PROPERTY_PATH);
        PropertyLoader.putProperties(fontSizeProps, defaults);
        Properties fontProps = PropertyLoader.loadProperties(DarkLaf.class,
                                                             FONT_DEFAULTS_NAME,
                                                             FONT_PROPERTY_PATH);
        PropertyLoader.putProperties(fontProps, defaults);
    }

    private void patchMacOSFonts(final Map<Object, Object> defaults) {
        for (Map.Entry<Object, Object> entry : defaults.entrySet()) {
            if (entry.getValue() instanceof Font) {
                Font font = (Font) entry.getValue();
                entry.setValue(macOSFontFromFont(font));
            }
        }
    }

    private Font macOSFontFromFont(final Font font) {
        Map<AttributedCharacterIterator.Attribute, Integer> attributes
            = Collections.singletonMap(TextAttribute.KERNING, TextAttribute.KERNING_ON);
        String fontName = SystemInfo.isMacOSCatalina ? MAC_OS_CATALINA_FONT_NAME : MAC_OS_FONT_NAME;
        Font macFont = new Font(fontName, font.getStyle(), font.getSize()).deriveFont(attributes);
        if (font instanceof UIResource) {
            macFont = new FontUIResource(macFont);
        }
        return macFont == null ? font : macFont;
    }

    private void applyFontRule(final Theme currentTheme, final Map<Object, Object> defaults) {
        FontSizeRule rule = currentTheme.getFontSizeRule();
        if (rule == null || rule == FontSizeRule.DEFAULT) return;
        for (Map.Entry<Object, Object> entry : defaults.entrySet()) {
            if (entry != null && entry.getValue() instanceof Font) {
                entry.setValue(fontWithRule((Font) entry.getValue(), rule, defaults));
            }
        }
    }

    private Font fontWithRule(final Font font, final FontSizeRule rule, final Map<Object, Object> defaults) {
        Font withRule = getFontMapper(rule).map(font, defaults);
        if (font instanceof UIResource
            && !(withRule instanceof UIResource)) {
            withRule = new FontUIResource(withRule);
        }
        return withRule;
    }

    private FontMapper getFontMapper(final FontSizeRule rule) {
        if (rule == null) return (font, defaults) -> font;
        return rule.getFontMapper();
    }
}
