/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.task;

import java.awt.*;
import java.awt.font.TextAttribute;
import java.text.AttributedCharacterIterator;
import java.util.*;
import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.logging.Logger;

import javax.swing.*;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.DarkLaf;
import com.github.weisj.darklaf.properties.PropertyLoader;
import com.github.weisj.darklaf.properties.icons.IconResolver;
import com.github.weisj.darklaf.properties.uiresource.DarkFontUIResource;
import com.github.weisj.darklaf.theme.Theme;
import com.github.weisj.darklaf.theme.info.FontSizeRule;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.FontUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.SystemInfo;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public class FontDefaultsInitTask implements DefaultsInitTask {

    private static final Logger LOGGER = LogUtil.getLogger(FontDefaultsInitTask.class);
    private static final String SWING_AA_KEY = "swing.aatext";
    private static final String SWING_AA_DEFAULT_VALUE = "true";
    private static final String FONT_PROPERTY_PATH = "";
    private static final String FONT_SIZE_DEFAULTS_NAME = "font_sizes";
    private static final String FONT_DEFAULTS_NAME = "font";
    private static final String KERNING_ALLOW_LIST = "kerning.allowList";
    private static final String KERNING_BLOCK_LIST = "kerning.blockList";

    /*
     * Per https://docs.oracle.com/javase/7/docs/api/java/awt/RenderingHints.html#
     * VALUE_TEXT_ANTIALIAS_LCD_HRGB a minimum bit depth of 15 is recommended for using lcd text
     * antialiasing.
     */
    private static final int LCD_TEXT_ANTIALIASING_MIN_BIT_DEPTH = 15;

    private static final String ALL_FONTS = "__all__";

    private static final Map<AttributedCharacterIterator.Attribute, Integer> ENABLE_KERNING =
            Collections.singletonMap(TextAttribute.KERNING, TextAttribute.KERNING_ON);
    /*
     * On Catalina the are issues with font kerning and .AppleSystemUIFont. For now Helvetica Neue is
     * used instead.
     */
    @SuppressWarnings("UnusedVariable")
    private static final String MAC_OS_CATALINA_FONT_NAME = ".AppleSystemUIFont";
    private static final String MAC_OS_CATALINA_FONT_NAME_FALLBACK = "Helvetica Neue";
    private static final String MAC_OS_FONT_NAME = ".SF NS Text";
    private static final String WINDOWS_10_FONT_NAME = "Segoe UI";
    private static final String WINDOWS_10_MONO_FONT_NAME = "Consolas";

    @Override
    public void run(final Theme currentTheme, final UIDefaults defaults) {
        loadFontProperties(defaults);

        if (SystemInfo.isMac) {
            patchOSFonts(defaults, this::mapMacOSFont);
        } else if (SystemInfo.isWindows) {
            patchOSFonts(defaults, this::mapWindowsFont);
        }

        if (SystemInfo.isMacOSCatalina) {
            defaults.put(RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON);
        }

        if (systemKerningEnabled()) {
            List<String> allowedFonts = PropertyUtil.getList(defaults, KERNING_ALLOW_LIST, String.class);
            List<String> blockedFonts = PropertyUtil.getList(defaults, KERNING_BLOCK_LIST, String.class);

            if (!allowedFonts.isEmpty()) {
                Set<String> allowedFontsSet = new HashSet<>(allowedFonts);
                Set<String> blockedFontSet = new HashSet<>(blockedFonts);
                boolean enabledAll = ALL_FONTS.equals(allowedFonts.get(0));

                setupKerningPerFont(defaults,
                        key -> (enabledAll || allowedFontsSet.contains(key)) && !blockedFontSet.contains(key));
            }
        }

        applyFontRule(currentTheme, defaults);
        setupRenderingHints(defaults);
        defaults.remove(KERNING_ALLOW_LIST);
        defaults.remove(KERNING_BLOCK_LIST);
    }

    private boolean systemKerningEnabled() {
        if (SystemInfo.isMac) return SystemInfo.isMacOSMojave;
        if (SystemInfo.isWindows) return SystemInfo.isWindowsVista;
        return false;
    }

    private void setupRenderingHints(final UIDefaults defaults) {
        if (!SystemInfo.isMacOSMojave) {
            PropertyUtil.installSystemProperty(SWING_AA_KEY, SWING_AA_DEFAULT_VALUE);

            Toolkit toolkit = Toolkit.getDefaultToolkit();
            Map<?, ?> desktopHints = (Map<?, ?>) toolkit.getDesktopProperty(GraphicsUtil.DESKTOP_HINTS_KEY);

            if (desktopHints == null) {
                desktopHints = Collections.emptyMap();
            }

            Object aaHint = desktopHints.get(RenderingHints.KEY_TEXT_ANTIALIASING);
            Object lcdContrastHint = desktopHints.get(RenderingHints.KEY_TEXT_LCD_CONTRAST);

            if (!GraphicsEnvironment.isHeadless()) {
                if (aaHint == null) {
                    GraphicsDevice device = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice();
                    DisplayMode displayMode = device.getDisplayMode();
                    int bitDepth = displayMode.getBitDepth();
                    boolean appropriateBitDepth =
                            bitDepth >= LCD_TEXT_ANTIALIASING_MIN_BIT_DEPTH || bitDepth == DisplayMode.BIT_DEPTH_MULTI;
                    // LCD Subpixel AA shouldn't be turned on for printer devices.
                    boolean appropriateDisplayMode = device.getType() != GraphicsDevice.TYPE_PRINTER;
                    if (appropriateBitDepth && appropriateDisplayMode) {
                        aaHint = RenderingHints.VALUE_TEXT_ANTIALIAS_LCD_HRGB;
                    } else {
                        aaHint = RenderingHints.VALUE_TEXT_ANTIALIAS_ON;
                    }
                    // The fallback value is an educated guess so issue a warning for the user.
                    String message = "System property 'awt.useSystemAAFontSettings' is not set. Using '" + aaHint
                            + "'\nIt's recommended to manually add the property to the JVM startup parameters on your "
                            + "platform for optimal text antialiasing. Refer to the "
                            + "[documentation](https://docs.oracle.com/javase/8/docs/technotes/guides/2d/flags.html#aaFonts)"
                            + "for more information on the possible values.";
                    if (!SystemInfo.isJava9OrGreater) {
                        message += "\n";
                        message += "If you are using a Java version of 8 or earlier try switching to at least Java 9 "
                                + "for improved scaling and font rendering support.";
                    }
                    LOGGER.warning(message);
                }
            }

            if (aaHint != null) {
                LOGGER.fine(String.format("Setting '%s' = '%s'", RenderingHints.KEY_TEXT_ANTIALIASING, aaHint));
                defaults.put(RenderingHints.KEY_TEXT_ANTIALIASING, aaHint);
            }
            if (lcdContrastHint != null) {
                LOGGER.fine(
                        String.format("Setting '%s' = '%s'", RenderingHints.KEY_TEXT_LCD_CONTRAST, lcdContrastHint));
                defaults.put(RenderingHints.KEY_TEXT_LCD_CONTRAST, lcdContrastHint);
            }
        }
    }

    private void loadFontProperties(final UIDefaults defaults) {
        IconResolver iconResolver = DarkUIUtil.iconResolver();
        Properties fontSizeProps =
                PropertyLoader.loadProperties(DarkLaf.class, FONT_SIZE_DEFAULTS_NAME, FONT_PROPERTY_PATH);
        PropertyLoader.putProperties(fontSizeProps, defaults, iconResolver);
        Properties fontProps = PropertyLoader.loadProperties(DarkLaf.class, FONT_DEFAULTS_NAME, FONT_PROPERTY_PATH);
        PropertyLoader.putProperties(fontProps, defaults, iconResolver);
    }

    private void patchOSFonts(final UIDefaults defaults, final Function<Map.Entry<Object, Font>, Font> mapper) {
        PropertyLoader.replacePropertyEntriesOfType(Font.class, defaults,
                e -> isDefaultFont(e.getValue()) || isMonospaceDefault(e.getValue()), mapper);
    }

    private boolean isDefaultFont(final Font font) {
        return Font.DIALOG.equals(font.getFamily());
    }

    private boolean isMonospaceDefault(final Font font) {
        return Font.MONOSPACED.equals(font.getFamily());
    }

    private Font mapMacOSFont(final Map.Entry<Object, Font> entry) {
        Font font = entry.getValue();
        if (isMonospaceDefault(font)) return font;

        String fontName = SystemInfo.isMacOSCatalina
                ? MAC_OS_CATALINA_FONT_NAME_FALLBACK
                : MAC_OS_FONT_NAME;
        Font macFont = FontUtil.createFont(fontName, font.getStyle(), font.getSize());
        if (SystemInfo.isMacOSMojave) macFont = macFont.deriveFont(ENABLE_KERNING);
        if (font instanceof UIResource) {
            macFont = new DarkFontUIResource(macFont);
        }
        return macFont == null ? font : macFont;
    }

    private Font mapWindowsFont(final Map.Entry<Object, Font> entry) {
        Font font = entry.getValue();
        if (!SystemInfo.isWindowsVista) return font;
        String fontName = isMonospaceDefault(font)
                ? WINDOWS_10_MONO_FONT_NAME
                : WINDOWS_10_FONT_NAME;
        Font windowsFont = FontUtil.createFont(fontName, font.getStyle(), font.getSize());
        if (font instanceof UIResource) {
            windowsFont = new DarkFontUIResource(windowsFont);
        }
        return windowsFont;
    }

    private void setupKerningPerFont(final UIDefaults defaults, final Predicate<String> kerningPredicate) {
        PropertyLoader.replacePropertiesOfType(Font.class, defaults, e -> kerningPredicate.test(e.getKey().toString()),
                f -> {
                    Font font = f.deriveFont(ENABLE_KERNING);
                    if (f instanceof UIResource) font = new DarkFontUIResource(font);
                    return font;
                });
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
        if (font instanceof UIResource && !(withRule instanceof UIResource)) {
            withRule = new DarkFontUIResource(withRule);
        }
        return withRule;
    }
}
