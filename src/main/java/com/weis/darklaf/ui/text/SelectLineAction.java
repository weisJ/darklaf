package com.weis.darklaf.ui.text;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.Segment;
import javax.swing.text.TextAction;
import java.awt.event.ActionEvent;

public class SelectLineAction extends TextAction {

    private final Action start;
    private final Action end;

    public SelectLineAction() {
        super(DefaultEditorKit.selectLineAction);
        start = new BeginLineAction("pigdog", false);
        end = new EndLineAction("pigdog", true);
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
                var textArea = getTextComponent(e);
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
                    firstNonWhitespace = currentLineStart +
                                         (firstNonWhitespace - currentLine.offset);
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

            } catch (BadLocationException ble) {
                /* Shouldn't ever happen. */
                ble.printStackTrace();
            }

        }

        @Contract(pure = true)
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
            var textArea = getTextComponent(e);
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
            } catch (Exception ex) {
                UIManager.getLookAndFeel().provideErrorFeedback(textArea);
            }
        }
    }

}
