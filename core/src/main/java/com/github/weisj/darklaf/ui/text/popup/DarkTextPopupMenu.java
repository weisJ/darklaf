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
package com.github.weisj.darklaf.ui.text.popup;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.text.JTextComponent;

public class DarkTextPopupMenu extends JPopupMenu implements UIResource {

    public DarkTextPopupMenu(final JTextComponent editor) {
        add(new CutMenuItem("Cut", editor));
        add(new CopyMenuItem("Copy", editor));
        add(new PasteMenuItem("Paste", editor));
    }

    protected abstract static class EditMenuItem extends JMenuItem implements ActionListener {

        protected final JTextComponent editor;

        public EditMenuItem(final String title, final JTextComponent editor) {
            this.editor = editor;
            setupIcons();
            setText(title);
            addActionListener(this);
        }

        protected void setupIcons() {}

        @Override
        public boolean isEnabled() {
            return editor != null && editor.isEnabled() && editor.isEditable() && canPerformAction();
        }

        protected abstract boolean canPerformAction();

        @Override
        public void actionPerformed(final ActionEvent e) {}
    }

    protected static class PasteMenuItem extends EditMenuItem {

        public PasteMenuItem(final String title, final JTextComponent editor) {
            super(title, editor);
        }

        @Override
        protected void setupIcons() {
            setIcon(UIManager.getIcon("TextComponent.paste.icon"));
            setDisabledIcon(UIManager.getIcon("TextComponent.pasteDisabled.icon"));
        }

        @Override
        protected boolean canPerformAction() {
            return true;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (editor != null) editor.paste();
        }
    }

    protected static class CopyMenuItem extends EditMenuItem {

        public CopyMenuItem(final String title, final JTextComponent editor) {
            super(title, editor);
        }

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

    protected static class CutMenuItem extends CopyMenuItem {

        public CutMenuItem(final String title, final JTextComponent editor) {
            super(title, editor);
        }

        @Override
        protected void setupIcons() {
            setIcon(UIManager.getIcon("TextComponent.cut.icon"));
            setDisabledIcon(UIManager.getIcon("TextComponent.cutDisabled.icon"));
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            if (editor != null) editor.cut();
        }
    }
}
