/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.theme.laf;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.theme.Theme;

public class DelegatingThemedLaf extends ThemedLookAndFeel {

    private final Theme theme;
    private final LookAndFeel lafBase;

    public DelegatingThemedLaf(final Theme theme, final ThemedLookAndFeel lafBase) {
        this.theme = theme;
        this.lafBase = lafBase;
        lafBase.setTheme(theme);
    }

    @Override
    public String getName() {
        return theme.getName();
    }

    @Override
    public String getID() {
        return lafBase.getID() + " - " + getName();
    }

    @Override
    public String getDescription() {
        return lafBase.getDescription();
    }

    @Override
    public boolean isNativeLookAndFeel() {
        return lafBase.isNativeLookAndFeel();
    }

    @Override
    public boolean isSupportedLookAndFeel() {
        return lafBase.isSupportedLookAndFeel();
    }

    @Override
    protected void setTheme(final Theme theme) {
        // Do nothing.
    }

    @Override
    public Theme getTheme() {
        return theme;
    }

    @Override
    public LayoutStyle getLayoutStyle() {
        return lafBase.getLayoutStyle();
    }

    @Override
    public void provideErrorFeedback(final Component component) {
        lafBase.provideErrorFeedback(component);
    }

    @Override
    public Icon getDisabledIcon(final JComponent component, final Icon icon) {
        return lafBase.getDisabledIcon(component, icon);
    }

    @Override
    public Icon getDisabledSelectedIcon(final JComponent component, final Icon icon) {
        return lafBase.getDisabledSelectedIcon(component, icon);
    }

    @Override
    public void initialize() {
        lafBase.initialize();
    }

    @Override
    public void uninitialize() {
        lafBase.uninitialize();
    }

    @Override
    public UIDefaults getDefaults() {
        return lafBase.getDefaults();
    }
}
