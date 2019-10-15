/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.components;

import com.weis.darklaf.decorators.PlainAction;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;

/**
 * @author Jannis Weis
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
    public void show(final Component invoker, final int x, final int y) {
        if (history.size() == 0) return;
        super.show(invoker, x, y);
    }

    @Override
    protected void showPopup() {
        if (history.size() == 0) {
            firePopupMenuCanceled();
            return;
        }
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
