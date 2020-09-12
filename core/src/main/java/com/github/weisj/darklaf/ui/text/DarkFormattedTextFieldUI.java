/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.text;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.Format;
import java.text.ParsePosition;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.Document;
import javax.swing.text.InternationalFormatter;

import com.github.weisj.darklaf.util.PropertyKey;

/** @author Jannis Weis */
public class DarkFormattedTextFieldUI extends DarkTextFieldUI implements PropertyChangeListener, DocumentListener {

    private JFormattedTextField textField;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkFormattedTextFieldUI();
    }

    protected String getPropertyPrefix() {
        return "FormattedTextField";
    }

    @Override
    public void installUI(final JComponent c) {
        textField = (JFormattedTextField) c;
        super.installUI(c);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        if (PropertyKey.DOCUMENT.equals(evt.getPropertyName())) {
            Object oldDoc = evt.getOldValue();
            Object newDoc = evt.getNewValue();
            if (oldDoc instanceof Document) {
                ((Document) oldDoc).removeDocumentListener(this);
            }
            if (newDoc instanceof Document) {
                ((Document) newDoc).addDocumentListener(this);
            }
        }
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        textField.getDocument().addDocumentListener(this);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        textField.getDocument().removeDocumentListener(this);
    }

    @Override
    public void insertUpdate(final DocumentEvent e) {
        SwingUtilities.invokeLater(this::update);
    }

    @Override
    public void removeUpdate(final DocumentEvent e) {
        SwingUtilities.invokeLater(this::update);
    }

    @Override
    public void changedUpdate(final DocumentEvent e) {}

    protected void update() {
        if (textField == null)
            return;
        textField.putClientProperty(DarkTextUI.KEY_HAS_ERROR, !isInputValid(textField));
    }

    protected boolean isInputValid(final JFormattedTextField textField) {
        String value = textField.getText();
        JFormattedTextField.AbstractFormatter formatter = textField.getFormatter();
        if (formatter instanceof InternationalFormatter) {
            Format format = ((InternationalFormatter) formatter).getFormat();
            ParsePosition position = new ParsePosition(0);
            format.parseObject(value, position);
            return position.getIndex() == value.length();
        } else {
            return textField.isEditValid();
        }
    }
}
