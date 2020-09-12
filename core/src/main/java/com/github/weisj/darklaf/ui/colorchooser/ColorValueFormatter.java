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
package com.github.weisj.darklaf.ui.colorchooser;

import static java.util.Locale.ENGLISH;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.text.ParseException;

import javax.swing.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.DocumentFilter;

import com.github.weisj.darklaf.color.DarkColorModel;
import com.github.weisj.darklaf.ui.text.DarkTextUI;

/** @author Jannis Weis */
public final class ColorValueFormatter extends JFormattedTextField.AbstractFormatter
        implements FocusListener, ActionListener {

    private final int fieldIndex;
    private final int radix;
    private final boolean hex;
    private final Timer errorTimer;
    private DarkColorModel model;
    private boolean transparencyEnabled;
    private JFormattedTextField text;
    private final DocumentFilter filter = new DocumentFilter() {
        @Override
        public void remove(final FilterBypass fb, final int offset, final int length) throws BadLocationException {
            if (isValid(fb.getDocument().getLength() - length)) {
                fb.remove(offset, length);
                commit();
            } else {
                error();
            }
        }

        @Override
        public void insertString(final FilterBypass fb, final int offset, final String text, final AttributeSet set)
                throws BadLocationException {
            if (isValid(fb.getDocument().getLength() + text.length()) && isValid(text)) {
                StringBuilder newText = new StringBuilder(fb.getDocument().getText(0, fb.getDocument().getLength()));
                newText.insert(offset, text);
                if (hex || isValidValue(newText.toString())) {
                    fb.insertString(offset, text.toUpperCase(ENGLISH), set);
                    commit();
                    return;
                }
            }
            error();
        }

        @Override
        public void replace(final FilterBypass fb, final int offset, final int length, final String text,
                final AttributeSet set) throws BadLocationException {
            if (isValid(fb.getDocument().getLength() + text.length() - length) && isValid(text)) {
                StringBuilder newText = new StringBuilder(fb.getDocument().getText(0, fb.getDocument().getLength()));
                newText.replace(offset, offset + length, text);
                if (hex || isValidValue(newText.toString())) {
                    fb.replace(offset, length, text.toUpperCase(ENGLISH), set);
                    commit();
                    return;
                }
            }
            error();
        }
    };

    private ColorValueFormatter(final DarkColorModel model, final int index, final boolean hex) {
        this.model = model;
        this.fieldIndex = index;
        this.radix = hex ? 16 : 10;
        this.hex = hex;
        this.errorTimer = new Timer(UIManager.getInt("ColorChooser.errorDelay"), this);
        errorTimer.setRepeats(false);
    }

    public static ColorValueFormatter init(final DarkColorModel model, final int index, final boolean hex,
            final JFormattedTextField text) {
        ColorValueFormatter formatter = new ColorValueFormatter(model, index, hex);
        formatter.setText(text);
        text.setFormatterFactory(new DefaultFormatterFactory(formatter));
        text.setMinimumSize(text.getPreferredSize());
        text.addFocusListener(formatter);
        return formatter;
    }

    public void setText(final JFormattedTextField text) {
        this.text = text;
    }

    protected void error() {
        text.putClientProperty(DarkTextUI.KEY_HAS_ERROR, true);
        text.repaint();
        errorTimer.restart();
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        text.putClientProperty(DarkTextUI.KEY_HAS_ERROR, false);
        text.repaint();
    }

    private void commit() {
        text.putClientProperty(DarkTextUI.KEY_HAS_ERROR, false);
        text.repaint();
        SwingUtilities.invokeLater(() -> {
            try {
                if (text != null) {
                    text.commitEdit();
                }
            } catch (ParseException e) {
                e.printStackTrace();
            }
        });
    }

    public void setModel(final DarkColorModel model) {
        this.model = model;
    }

    public void setTransparencyEnabled(final boolean transparencyEnabled) {
        this.transparencyEnabled = transparencyEnabled;
    }

    public void focusGained(final FocusEvent event) {
        Object source = event.getSource();
        if (source instanceof JFormattedTextField) {
            this.text = (JFormattedTextField) source;
            SwingUtilities.invokeLater(() -> {
                if (this.text != null) {
                    this.text.selectAll();
                }
            });
        }
    }

    public void focusLost(final FocusEvent event) {
        SwingUtilities.invokeLater(() -> text.select(0, 0));
    }

    private boolean isValid(final int length) {
        return (0 <= length) && (length <= getLength());
    }

    private int getLength() {
        return hex ? getHexLength() : String.valueOf(model.getMaximum(fieldIndex)).length();
    }

    private int getHexLength() {
        return transparencyEnabled ? 8 : 6;
    }

    private boolean isValid(final String text) {
        int length = text.length();
        for (int i = 0; i < length; i++) {
            char ch = text.charAt(i);
            if (Character.digit(ch, this.radix) < 0) {
                return false;
            }
        }
        return true;
    }

    private boolean isValidValue(final String text) {
        try {
            stringToValue(text);
        } catch (ParseException e) {
            return false;
        }
        return true;
    }

    @Override
    public Object stringToValue(final String text) throws ParseException {
        try {
            if (text.isEmpty()) {
                return model.getDefault(fieldIndex);
            }
            if (hex) {
                String hexStr = String.format("%1$-" + getHexLength() + "s", text).replaceAll(" ", "F");
                int r = Integer.valueOf(hexStr.substring(0, 2), 16);
                checkRange(r, 0, 255);
                int g = Integer.valueOf(hexStr.substring(2, 4), 16);
                checkRange(g, 0, 255);
                int b = Integer.valueOf(hexStr.substring(4, 6), 16);
                checkRange(b, 0, 255);
                int alpha = hexStr.length() >= 8 ? Integer.valueOf(hexStr.substring(6, 8), 16) : 255;
                checkRange(alpha, 0, 255);
                return new Color(r, g, b, alpha);
            } else {
                int value = Integer.valueOf(text, this.radix);
                int min = model.getMinimum(fieldIndex);
                int max = model.getMaximum(fieldIndex);
                checkRange(value, min, max);
                return value;
            }
        } catch (NumberFormatException nfe) {
            ParseException pe = new ParseException("illegal format", 0);
            pe.initCause(nfe);
            throw pe;
        }
    }

    protected void checkRange(final int value, final int min, final int max) throws ParseException {
        if (value > max || value < min) {
            throw new ParseException("Value not in range [" + min + "," + max + "]", 0);
        }
    }

    @Override
    public String valueToString(final Object object) throws ParseException {
        if (object instanceof Integer && !hex) {
            return object.toString();
        } else if (object instanceof Color && hex) {
            Color c = (Color) object;
            int r = c.getRed();
            int g = c.getGreen();
            int b = c.getBlue();
            int a = c.getAlpha();
            if (getHexLength() == 8) {
                return String.format("%02X%02X%02X%02X", r, g, b, a);
            } else {
                return String.format("%02X%02X%02X", r, g, b);
            }
        }
        throw new ParseException("illegal object", 0);
    }

    @Override
    protected DocumentFilter getDocumentFilter() {
        return this.filter;
    }
}
