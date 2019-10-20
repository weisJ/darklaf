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
package com.github.weisj.darklaf.ui.text;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.Document;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.text.ParseException;

/**
 * @author Jannis Weis
 */
public class DarkFormattedTextFieldUI extends DarkTextFieldUI implements PropertyChangeListener, DocumentListener {

    private JFormattedTextField textField;

    @NotNull
    @Contract("_ -> new")
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
        if ("document".equals(evt.getPropertyName())) {
            var oldDoc = evt.getOldValue();
            var newDoc = evt.getNewValue();
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
        update();
    }

    @Override
    public void removeUpdate(final DocumentEvent e) {
        update();
    }

    @Override
    public void changedUpdate(final DocumentEvent e) {
    }

    protected void update() {
        if (textField == null) return;
        try {
            textField.getFormatter().stringToValue(textField.getText());
            textField.putClientProperty("JTextField.hasError", false);
        } catch (ParseException e) {
            textField.putClientProperty("JTextField.hasError", true);
        }
    }
}
