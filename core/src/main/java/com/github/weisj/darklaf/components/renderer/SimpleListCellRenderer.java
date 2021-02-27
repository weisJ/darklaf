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
package com.github.weisj.darklaf.components.renderer;

import java.awt.*;
import java.util.function.BiConsumer;
import java.util.function.Function;

import javax.swing.*;
import javax.swing.border.Border;

public class SimpleListCellRenderer<T> extends JLabel implements ListCellRenderer<T> {

    public static <T> SimpleListCellRenderer<T> create(final Function<? super T, String> toText) {
        return new SimpleListCellRenderer<T>() {
            @Override
            protected void customize(final T value) {
                setText(toText.apply(value));
            }
        };
    }

    public static <T> SimpleListCellRenderer<T> create(final BiConsumer<JLabel, T> customize) {
        return new SimpleListCellRenderer<T>() {
            @Override
            protected void customize(final T value) {
                customize.accept(this, value);
            }
        };
    }

    @Override
    public Component getListCellRendererComponent(final JList<? extends T> list, final T value, final int index,
            final boolean isSelected, final boolean cellHasFocus) {
        setComponentOrientation(list.getComponentOrientation());

        customize(value);

        setEnabled(list.isEnabled());
        setFont(list.getFont());

        Border border = null;
        if (cellHasFocus) {
            if (isSelected) {
                border = UIManager.getBorder("List.focusSelectedCellHighlightBorder");
            }
            if (border == null) {
                border = UIManager.getBorder("List.focusCellHighlightBorder");
            }
        } else {
            border = UIManager.getBorder("List.cellNoFocusBorder");
        }
        setBorder(border);

        return this;
    }

    protected void customize(final T value) {
        if (value instanceof Icon) {
            setIcon((Icon) value);
            setText("");
        } else {
            setIcon(null);
            setText((value == null) ? "" : value.toString());
        }
    }
}
