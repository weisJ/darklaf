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
package com.github.weisj.darklaf.delegate;

import static java.awt.RenderingHints.*;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyKey;

public class AbstractButtonLayoutDelegate extends AbstractButton {

    protected AbstractButton delegate;

    public void setDelegate(final AbstractButton delegate) {
        this.delegate = delegate;
        if (delegate != null) {
            putClientProperty(PropertyKey.HTML, delegate.getClientProperty(PropertyKey.HTML));
            putClientProperty(KEY_TEXT_ANTIALIASING, delegate.getClientProperty(KEY_TEXT_ANTIALIASING));
            putClientProperty(KEY_TEXT_LCD_CONTRAST, delegate.getClientProperty(KEY_TEXT_LCD_CONTRAST));
            putClientProperty(KEY_FRACTIONALMETRICS, delegate.getClientProperty(KEY_FRACTIONALMETRICS));
        }
    }

    @Override
    public Icon getIcon() {
        return delegate.getIcon();
    }

    @Override
    public String getText() {
        return delegate.getText();
    }

    @Override
    public Font getFont() {
        return delegate.getFont();
    }

    @Override
    public FontMetrics getFontMetrics(final Font font) {
        return delegate.getFontMetrics(font);
    }

    @Override
    public int getVerticalAlignment() {
        return delegate.getVerticalAlignment();
    }

    @Override
    public int getHorizontalAlignment() {
        return delegate.getHorizontalAlignment();
    }

    @Override
    public int getVerticalTextPosition() {
        return delegate.getVerticalTextPosition();
    }

    @Override
    public int getHorizontalTextPosition() {
        return delegate.getHorizontalTextPosition();
    }

    @Override
    public Insets getInsets() {
        return delegate.getInsets();
    }

    @Override
    public ButtonModel getModel() {
        return delegate.getModel();
    }

    @Override
    public ComponentOrientation getComponentOrientation() {
        return delegate.getComponentOrientation();
    }
}
