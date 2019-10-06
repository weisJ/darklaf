package com.weis.darklaf.components;

import com.weis.darklaf.decorators.PlainAction;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class TextFieldHistory extends ScrollPopupMenu implements ActionListener {

    private final LinkedHashSet<String> history;
    private JTextField textField;

    /**
     * Create Scroll Popup Menu.
     *
     * @param textField the text field.
     * @param length    the length of the history.
     * @param maxH      maximum height.
     */
    public TextFieldHistory(final JTextField textField, final int length, final int maxH) {
        super(maxH);
        this.history = new LinkedHashSet<>(length);
        this.textField = textField;
        if (textField != null) {
            textField.addActionListener(this);
        }
    }

    public void setTextField(final JTextField textField) {
        if (this.textField != null) {
            textField.removeActionListener(this);
        }
        this.textField = textField;
        if (this.textField != null) {
            textField.addActionListener(this);
        }
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        var text = textField.getText();
        if (!text.isBlank()) {
            history.remove(text);
            history.add(text);
        }
    }

    @Override
    protected void showPopup() {
        this.removeAll();
        LinkedList<String> list = new LinkedList<>(history);
        Iterator<String> itr = list.descendingIterator();
        while (itr.hasNext()) {
            String item = itr.next();
            add(new JMenuItem(new PlainAction(item, () -> textField.setText(item))));
        }
        super.showPopup();
    }
}
