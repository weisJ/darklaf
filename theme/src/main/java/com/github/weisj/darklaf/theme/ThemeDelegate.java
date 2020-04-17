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
package com.github.weisj.darklaf.theme;

import com.github.weisj.darklaf.theme.info.*;

import javax.swing.*;
import java.util.Properties;

public class ThemeDelegate extends Theme {

    private final Theme delegate;
    private final boolean overwriteFontSize;
    private final boolean overWriteAccentColor;

    public ThemeDelegate(final Theme delegate) {
        this(delegate, null, null);
    }

    public ThemeDelegate(final Theme delegate, final FontSizeRule fontSizeRule, final AccentColorRule accentColorRule) {
        super(fontSizeRule, accentColorRule);
        if (delegate == null) {
            throw new IllegalArgumentException("Theme delegate cannot be null");
        }
        this.delegate = delegate;
        this.overwriteFontSize = fontSizeRule != null;
        this.overWriteAccentColor = accentColorRule != null;
    }

    public Theme getDelegate() {
        return delegate;
    }

    @Override
    public Theme derive(final FontSizeRule fontSizeRule,
                        final AccentColorRule accentColorRule) {
        return getDelegate().derive(fontSizeRule, accentColorRule);
    }

    @Override
    public Theme copy() {
        return getDelegate().copy();
    }

    @Override
    public FontSizeRule getFontSizeRule() {
        return overwriteFontSize ? super.getFontSizeRule() : delegate.getFontSizeRule();
    }

    @Override
    public AccentColorRule getAccentColorRule() {
        return overWriteAccentColor ? super.getAccentColorRule() : delegate.getAccentColorRule();
    }

    @Override
    public ColorToneRule getColorToneRule() {
        return delegate.getColorToneRule();
    }

    @Override
    public ContrastRule getContrastRule() {
        return delegate.getContrastRule();
    }

    @Override
    protected PresetIconRule getPresetIconRule() {
        return delegate.getPresetIconRule();
    }

    @Override
    public void loadDefaults(final Properties properties, final UIDefaults currentDefaults) {
        delegate.loadDefaults(properties, currentDefaults);
    }

    @Override
    public void customizeGlobals(final Properties properties, final UIDefaults currentDefaults) {
        delegate.customizeGlobals(properties, currentDefaults);
    }

    @Override
    public void customizeIconTheme(final Properties properties, final UIDefaults currentDefaults) {
        delegate.customizeIconTheme(properties, currentDefaults);
    }

    @Override
    public void loadIconTheme(final Properties properties, final UIDefaults currentDefaults) {
        delegate.loadIconTheme(properties, currentDefaults);
    }

    @Override
    public void customizePlatformProperties(final Properties properties, final UIDefaults currentDefaults) {
        delegate.customizePlatformProperties(properties, currentDefaults);
    }

    @Override
    public void customizeUIProperties(final Properties properties, final UIDefaults currentDefaults) {
        delegate.customizeUIProperties(properties, currentDefaults);
    }

    @Override
    public String toString() {
        return delegate.getName();
    }

    @Override
    protected String getResourcePath() {
        return delegate.getResourcePath();
    }

    @Override
    public String getPrefix() {
        return delegate.getPrefix();
    }

    @Override
    public String getName() {
        return delegate.getName();
    }

    @Override
    protected Class<? extends Theme> getLoaderClass() {
        return delegate.getClass();
    }

    @Override
    protected String getPropertyFilePath(final String name) {
        return delegate.getPropertyFilePath(name);
    }

    @Override
    public boolean useCustomDecorations() {
        return delegate.useCustomDecorations();
    }
}
