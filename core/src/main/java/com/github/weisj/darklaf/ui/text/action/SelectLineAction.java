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
package com.github.weisj.darklaf.ui.text.action;

import java.awt.event.ActionEvent;

import javax.swing.*;
import javax.swing.text.*;

/** @author Jannis Weis */
public class SelectLineAction extends TextAction {

    private final Action start;
    private final Action end;

    public SelectLineAction() {
        super(DefaultEditorKit.selectLineAction);
        start = new BeginLineAction("darklaf", false);
        end = new EndLineAction("darklaf", true);
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        start.actionPerformed(e);
        end.actionPerformed(e);
    }

    public static class BeginLineAction extends TextAction {
        private final Segment currentLine = new Segment();
        private final boolean select;

        public BeginLineAction(final String name, final boolean select) {
            super(name);
            this.select = select;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            int newPos;
            try {
                JTextComponent textArea = getTextComponent(e);
                int caretPosition = textArea.getCaretPosition();
                Document document = textArea.getDocument();
                Element map = document.getDefaultRootElement();
                int currentLineNum = map.getElementIndex(caretPosition);
                Element currentLineElement = map.getElement(currentLineNum);
                int currentLineStart = currentLineElement.getStartOffset();
                int currentLineEnd = currentLineElement.getEndOffset();
                int count = currentLineEnd - currentLineStart;

                if (count > 0) { // If there are chars in the line...
                    document.getText(currentLineStart, count, currentLine);
                    int firstNonWhitespace = getFirstNonWhitespacePos();
                    firstNonWhitespace = currentLineStart + (firstNonWhitespace - currentLine.offset);
                    if (caretPosition != firstNonWhitespace) {
                        newPos = firstNonWhitespace;
                    } else {
                        newPos = currentLineStart;
                    }
                } else { // Empty line (at end of the document only).
                    newPos = currentLineStart;
                }
                if (select) {
                    textArea.moveCaretPosition(newPos);
                } else {
                    textArea.setCaretPosition(newPos);
                }

            } catch (final BadLocationException ble) {
                throw new IllegalStateException("This shouldn't happen", ble);
            }
        }

        private int getFirstNonWhitespacePos() {
            int offset = currentLine.offset;
            int end = offset + currentLine.count - 1;
            int pos = offset;
            char[] array = currentLine.array;
            char currentChar = array[pos];
            while ((currentChar == '\t' || currentChar == ' ') && (++pos < end)) {
                currentChar = array[pos];
            }
            return pos;
        }
    }

    public static class EndLineAction extends TextAction {

        private final boolean select;

        public EndLineAction(final String name, final boolean select) {
            super(name);
            this.select = select;
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            JTextComponent textArea = getTextComponent(e);
            int offs = textArea.getCaretPosition();
            try {
                Element root = textArea.getDocument().getDefaultRootElement();
                int line = root.getElementIndex(offs);
                int endOffs = root.getElement(line).getEndOffset() - 1;
                if (select) {
                    textArea.moveCaretPosition(endOffs);
                } else {
                    textArea.setCaretPosition(endOffs);
                }
            } catch (final Exception ex) {
                UIManager.getLookAndFeel().provideErrorFeedback(textArea);
            }
        }
    }
}
