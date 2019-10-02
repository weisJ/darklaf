package com.weis.darklaf.ui.text;

import javax.swing.*;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.JTextComponent;
import javax.swing.text.TextAction;
import javax.swing.text.Utilities;
import java.awt.event.ActionEvent;

public class SelectWordAction extends TextAction {

    protected final Action start;
    protected final Action end;

    public SelectWordAction() {
        super(DefaultEditorKit.selectWordAction);
        start = new BeginWordAction("pigdog", false);
        end = new EndWordAction("pigdog", true);    }

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
                var textArea = getTextComponent(e);
                int offs = textArea.getCaretPosition();
                int begOffs = getWordStart(textArea, offs);
                if (select) {
                    textArea.moveCaretPosition(begOffs);
                }
                else {
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
                var textArea = getTextComponent(e);
                int offs = textArea.getCaretPosition();
                int endOffs = getWordEnd(textArea, offs);
                if (select) {
                    textArea.moveCaretPosition(endOffs);
                }
                else {
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
