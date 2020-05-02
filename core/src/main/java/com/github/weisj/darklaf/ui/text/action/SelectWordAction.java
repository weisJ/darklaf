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
package com.github.weisj.darklaf.ui.text.action;

import java.awt.event.ActionEvent;

import javax.swing.*;
import javax.swing.text.*;

/**
 * @author Jannis Weis
 */
public class SelectWordAction extends TextAction {

    protected final Action start;
    protected final Action end;

    public SelectWordAction() {
        super(DefaultEditorKit.selectWordAction);
        start = new BeginWordAction("darklaf", false);
        end = new EndWordAction("darklaf", true);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        start.actionPerformed(e);
        end.actionPerformed(e);
    }

    public static class BeginWordAction extends TextAction {
        private final boolean select;

        public BeginWordAction(final String name, final boolean select) {
            super(name);
            this.select = select;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            try {
                JTextComponent textArea = getTextComponent(e);
                int offs = textArea.getCaretPosition();
                int begOffs = getWordStart(textArea, offs);
                if (select) {
                    textArea.moveCaretPosition(begOffs);
                } else {
                    textArea.setCaretPosition(begOffs);
                }
            } catch (BadLocationException ble) {
                ble.printStackTrace();
            }
        }

        public int getWordStart(final JTextComponent textArea, final int offs)
                                                                               throws BadLocationException {
            return Utilities.getWordStart(textArea, offs);
        }
    }

    public static class EndWordAction extends TextAction {
        private final boolean select;

        public EndWordAction(final String name, final boolean select) {
            super(name);
            this.select = select;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            try {
                JTextComponent textArea = getTextComponent(e);
                int offs = textArea.getCaretPosition();
                int endOffs = getWordEnd(textArea, offs);
                if (select) {
                    textArea.moveCaretPosition(endOffs);
                } else {
                    textArea.setCaretPosition(endOffs);
                }
            } catch (BadLocationException ble) {
                ble.printStackTrace();
            }
        }

        protected int getWordEnd(final JTextComponent textArea, final int offs) throws BadLocationException {
            return Utilities.getWordEnd(textArea, offs);
        }
    }
}
