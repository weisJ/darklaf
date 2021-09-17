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
package com.github.weisj.darklaf.ui.text.popup;

import java.awt.event.ActionEvent;

import javax.swing.*;
import javax.swing.text.JTextComponent;


public class CopyMenuItem extends EditMenuItem {

    public CopyMenuItem(final JTextComponent editor) {
        this(UIManager.getString("Actions.copy", editor != null ? editor.getLocale() : null), editor);
    }

    public CopyMenuItem(final String title, final JTextComponent editor) {
        super(title, editor);
    }

    @Override
    protected void setupIcons() {
        setIcon(UIManager.getIcon("TextComponent.copy.icon"));
        setDisabledIcon(UIManager.getIcon("TextComponent.copyDisabled.icon"));
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (editor != null) editor.copy();
    }

    @Override
    protected boolean canPerformAction() {
        return editor.getCaret() != null && editor.getSelectionEnd() != editor.getSelectionStart();
    }
}
