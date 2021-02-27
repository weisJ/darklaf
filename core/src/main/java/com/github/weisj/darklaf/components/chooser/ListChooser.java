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
package com.github.weisj.darklaf.components.chooser;

import java.awt.*;
import java.util.List;
import java.util.function.Consumer;

import javax.swing.*;


public class ListChooser<T> extends JPanel implements ChooserComponent<T> {

    protected final JList<T> listComp;
    private final List<T> values;
    private T initial;
    private Consumer<T> callback;

    public ListChooser(final List<T> values) {
        super(new BorderLayout());
        this.values = values;
        DefaultListModel<T> model = new DefaultListModel<>();
        values.forEach(model::addElement);
        listComp = new JList<>(model);
        listComp.addListSelectionListener(e -> {
            if (callback != null) callback.accept(listComp.getSelectedValue());
        });
        add(new JScrollPane(listComp));
        setOpaque(false);
    }

    @Override
    public void reset(final T initial, final Consumer<T> callback) {
        this.initial = initial;
        this.callback = callback;
        if (initial != null) {
            for (int i = 0; i < values.size(); i++) {
                T value = values.get(i);
                if (value != null && value.equals(initial)) {
                    listComp.setSelectedIndex(i);
                    break;
                }
            }
        }
    }

    @Override
    public T getInitial() {
        return initial;
    }

    @Override
    public T getSelected() {
        return listComp.getSelectedValue();
    }

    @Override
    public Color getBackground() {
        return listComp != null ? listComp.getBackground() : super.getBackground();
    }
}
