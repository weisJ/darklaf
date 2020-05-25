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
package com.github.weisj.darklaf.ui.menu;

import java.awt.*;
import java.awt.font.TextAttribute;

import javax.swing.*;
import javax.swing.plaf.basic.BasicHTML;

public class MenuItemLayoutDelegate extends JMenuItem {

    private JMenuItem delegate;

    public MenuItemLayoutDelegate(final JMenuItem delegate) {
        setDelegate(delegate);
    }

    public void setDelegate(final JMenuItem delegate) {
        this.delegate = delegate;
        if (delegate != null) {
            putClientProperty(BasicHTML.propertyKey, delegate.getClientProperty(BasicHTML.propertyKey));
            putClientProperty(TextAttribute.NUMERIC_SHAPING, delegate.getClientProperty(TextAttribute.NUMERIC_SHAPING));
        }
    }

    @Override
    public Insets getInsets(final Insets insets) {
        if (delegate != null) return delegate.getInsets(insets);
        return super.getInsets(insets);
    }

    @Override
    public Insets getInsets() {
        if (delegate != null) return delegate.getInsets();
        return super.getInsets();
    }

    @Override
    public Font getFont() {
        if (delegate != null) return delegate.getFont();
        return super.getFont();
    }

    @Override
    public ComponentOrientation getComponentOrientation() {
        if (delegate != null) return delegate.getComponentOrientation();
        return super.getComponentOrientation();
    }

    @Override
    public Container getParent() {
        if (delegate != null) return delegate.getParent();
        return super.getParent();
    }

    @Override
    public KeyStroke getAccelerator() {
        if (delegate != null) return delegate.getAccelerator();
        return super.getAccelerator();
    }

    @Override
    public int getVerticalAlignment() {
        if (delegate != null) return delegate.getVerticalAlignment();
        return super.getVerticalAlignment();
    }

    @Override
    public int getHorizontalAlignment() {
        if (delegate != null) return delegate.getHorizontalAlignment();
        return super.getHorizontalAlignment();
    }

    @Override
    public int getVerticalTextPosition() {
        if (delegate != null) return delegate.getVerticalTextPosition();
        return super.getVerticalTextPosition();
    }

    @Override
    public int getHorizontalTextPosition() {
        if (delegate != null) return delegate.getHorizontalTextPosition();
        return super.getHorizontalTextPosition();
    }

    @Override
    public FontMetrics getFontMetrics(final Font font) {
        if (delegate != null) return delegate.getFontMetrics(font);
        return super.getFontMetrics(font);
    }

    @Override
    public Icon getIcon() {
        if (delegate != null) return delegate.getIcon();
        return super.getIcon();
    }

    @Override
    public String getText() {
        if (delegate != null) return delegate.getText();
        return super.getText();
    }
}
