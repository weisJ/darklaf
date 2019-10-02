package com.weis.darklaf.ui.colorchooser;

import com.weis.darklaf.color.DarkColorModel;
import com.weis.darklaf.util.StringUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultFormatterFactory;
import javax.swing.text.DocumentFilter;
import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.text.ParseException;

import static java.util.Locale.ENGLISH;

public final class ColorValueFormatter extends JFormattedTextField.AbstractFormatter implements FocusListener {

    private final int fieldIndex;
    private final int radix;
    private final boolean hex;
    private DarkColorModel model;
    private boolean transparencyEnabled;
    private JFormattedTextField text;
    private final DocumentFilter filter = new DocumentFilter() {
        @Override
        public void remove(@NotNull final FilterBypass fb, final int offset,
                           final int length) throws BadLocationException {
            if (isValid(fb.getDocument().getLength() - length)) {
                fb.remove(offset, length);
                commit();
            } else {
                Toolkit.getDefaultToolkit().beep();
            }
        }

        @Override
        public void replace(@NotNull final FilterBypass fb, final int offset, final int length,
                            @NotNull final String text, final AttributeSet set) throws BadLocationException {
            if (isValid(fb.getDocument().getLength() + text.length() - length) && isValid(text)) {
                var newText = new StringBuilder(fb.getDocument().getText(0, fb.getDocument().getLength()));
                newText.replace(offset, offset + length, text);
                if (hex || isValidValue(newText.toString())) {
                    fb.replace(offset, length, text.toUpperCase(ENGLISH), set);
                    commit();
                    return;
                }
            }
            Toolkit.getDefaultToolkit().beep();
        }

        @Override
        public void insertString(@NotNull final FilterBypass fb, final int offset,
                                 @NotNull final String text, final AttributeSet set) throws BadLocationException {
            if (isValid(fb.getDocument().getLength() + text.length())
                    && isValid(text)) {
                var newText = new StringBuilder(fb.getDocument().getText(0, fb.getDocument().getLength()));
                newText.insert(offset, text);
                if (hex || isValidValue(newText.toString())) {
                    fb.insertString(offset, text.toUpperCase(ENGLISH), set);
                    commit();
                    return;
                }
            }
            Toolkit.getDefaultToolkit().beep();
        }
    };

    private ColorValueFormatter(final DarkColorModel model, final int index, final boolean hex) {
        this.model = model;
        this.fieldIndex = index;
        this.radix = hex ? 16 : 10;
        this.hex = hex;
    }

    @NotNull
    static ColorValueFormatter init(final DarkColorModel model, final int index,
                                    final boolean hex, @NotNull final JFormattedTextField text) {
        ColorValueFormatter formatter = new ColorValueFormatter(model, index, hex);
        text.setFormatterFactory(new DefaultFormatterFactory(formatter));
        text.setMinimumSize(text.getPreferredSize());
        text.addFocusListener(formatter);
        return formatter;
    }

    private void commit() {
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

    @Override
    public Object stringToValue(@NotNull final String text) throws ParseException {
        try {
            if (text.isEmpty()) {
                return model.getDefault(fieldIndex);
            }
            if (hex) {
                var hexStr = text;
                if (hexStr.length() == 6) {
                    hexStr += "FF";
                }
                int r = Integer.valueOf(hexStr.substring(0, 2), 16);
                checkRange(r, 0, 255);
                int g = Integer.valueOf(hexStr.substring(2, 4), 16);
                checkRange(g, 0, 255);
                int b = Integer.valueOf(hexStr.substring(4, 6), 16);
                checkRange(b, 0, 255);
                return Integer.valueOf(text, radix);
            } else {
                var value = Integer.valueOf(text, this.radix);
                var min = model.getMinimum(fieldIndex);
                var max = model.getMaximum(fieldIndex);
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

    @Contract("null -> fail")
    @Override
    public String valueToString(final Object object) throws ParseException {
        if (object instanceof Integer) {
            if (radix == 10) {
                return object.toString();
            }
            if (hex) {
                return StringUtil.toUpperCase(Integer.toHexString((Integer) object));
            }
            int value = (Integer) object;
            int index = getLength();
            char[] array = new char[index];
            while (0 < index--) {
                array[index] = Character.forDigit(value & 0x0F, this.radix);
                value >>= 4;
            }
            return new String(array).toUpperCase(ENGLISH);
        }
        throw new ParseException("illegal object", 0);
    }

    @Contract(pure = true)
    @Override
    protected DocumentFilter getDocumentFilter() {
        return this.filter;
    }

    public void focusGained(@NotNull final FocusEvent event) {
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

    private int getLength() {
        return hex ? getHexLength() : String.valueOf(model.getMaximum(fieldIndex)).length();
    }

    @Contract(pure = true)
    private int getHexLength() {
        return transparencyEnabled ? 8 : 6;
    }

    @Contract(pure = true)
    private boolean isValid(final int length) {
        return (0 <= length) && (length <= getLength());
    }

    private boolean isValid(@NotNull final String text) {
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
}
