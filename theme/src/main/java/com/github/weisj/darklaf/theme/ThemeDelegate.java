/*
 * MIT License
 *
 * Copyright (c) 2020-2022 Jannis Weis
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
package com.github.weisj.darklaf.theme;

import java.util.Properties;

import javax.swing.*;

import com.github.weisj.darklaf.properties.icons.IconResolver;
import com.github.weisj.darklaf.theme.info.*;
import com.github.weisj.darklaf.theme.spec.*;

public class ThemeDelegate extends Theme {

    private final Theme delegate;
    private final boolean overwriteFontSize;
    private final boolean overwriteFontPrototype;
    private final boolean overwriteAccentColor;

    public ThemeDelegate(final Theme delegate) {
        this(delegate, null, null, null);
    }

    public ThemeDelegate(final Theme delegate, final FontSizeRule fontSizeRule, final FontPrototype fontPrototype,
            final AccentColorRule accentColorRule) {
        super(fontSizeRule, fontPrototype, accentColorRule);
        if (delegate == null) {
            throw new IllegalArgumentException("Theme delegate cannot be null");
        }
        this.delegate = delegate;
        this.overwriteFontSize = fontSizeRule != null;
        this.overwriteFontPrototype = fontPrototype != null;
        this.overwriteAccentColor = accentColorRule != null;
    }

    public Theme getDelegate() {
        return delegate;
    }

    @Override
    public Class<? extends Theme> getThemeClass() {
        return delegate.getThemeClass();
    }

    @Override
    public boolean appearsEqualTo(final Theme theme) {
        if (overwriteFontSize || overwriteAccentColor || overwriteFontPrototype) {
            return super.appearsEqualTo(theme);
        } else {
            return getDelegate().appearsEqualTo(theme);
        }
    }

    @Override
    public Theme derive(final FontSizeRule fontSizeRule, final FontPrototype fontPrototype,
            final AccentColorRule accentColorRule) {
        return getDelegate().derive(fontSizeRule, fontPrototype, accentColorRule);
    }

    @Override
    public Theme copy() {
        return getDelegate().copy();
    }

    @Override
    public FontSizeRule getFontSizeRule() {
        return overwriteFontSize ? super.getFontSizeRule() : getDelegate().getFontSizeRule();
    }

    @Override
    public AccentColorRule getAccentColorRule() {
        return overwriteAccentColor ? super.getAccentColorRule() : getDelegate().getAccentColorRule();
    }

    @Override
    public FontPrototype getFontPrototype() {
        return overwriteFontPrototype ? super.getFontPrototype() : getDelegate().getFontPrototype();
    }

    @Override
    public ColorToneRule getColorToneRule() {
        return getDelegate().getColorToneRule();
    }

    @Override
    public ContrastRule getContrastRule() {
        return getDelegate().getContrastRule();
    }

    @Override
    protected PresetIconRule getPresetIconRule() {
        return getDelegate().getPresetIconRule();
    }

    @Override
    public void loadDefaults(final Properties properties, final UIDefaults currentDefaults,
            final IconResolver iconResolver) {
        getDelegate().loadDefaults(properties, currentDefaults, iconResolver);
    }

    @Override
    public void customizeGlobals(final Properties properties, final UIDefaults currentDefaults,
            final IconResolver iconResolver) {
        getDelegate().customizeGlobals(properties, currentDefaults, iconResolver);
    }

    @Override
    public void customizeIconTheme(final Properties properties, final UIDefaults currentDefaults,
            final IconResolver iconResolver) {
        getDelegate().customizeIconTheme(properties, currentDefaults, iconResolver);
    }

    @Override
    public void loadIconTheme(final Properties properties, final UIDefaults currentDefaults,
            final IconResolver iconResolver) {
        getDelegate().loadIconTheme(properties, currentDefaults, iconResolver);
    }

    @Override
    public void customizePlatformProperties(final Properties properties, final UIDefaults currentDefaults,
            final IconResolver iconResolver) {
        getDelegate().customizePlatformProperties(properties, currentDefaults, iconResolver);
    }

    @Override
    public void customizeUIProperties(final Properties properties, final UIDefaults currentDefaults,
            final IconResolver iconResolver) {
        getDelegate().customizeUIProperties(properties, currentDefaults, iconResolver);
    }

    @Override
    protected String getResourcePath() {
        return getDelegate().getResourcePath();
    }

    @Override
    public String getPrefix() {
        return getDelegate().getPrefix();
    }

    @Override
    public String getName() {
        return getDelegate().getName();
    }

    @Override
    public String getDisplayName() {
        return getDelegate().getDisplayName();
    }

    @Override
    protected Class<? extends Theme> getLoaderClass() {
        return getDelegate().getClass();
    }

    @Override
    protected String getPropertyFilePath(final String name) {
        return getDelegate().getPropertyFilePath(name);
    }

    @Override
    public boolean useCustomDecorations() {
        return getDelegate().useCustomDecorations();
    }

    @Override
    public boolean supportsCustomSelectionColor() {
        return getDelegate().supportsCustomSelectionColor();
    }

    @Override
    public boolean supportsCustomAccentColor() {
        return getDelegate().supportsCustomAccentColor();
    }
}
