/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.combobox;

import com.github.weisj.darklaf.util.PropertyKey;

import javax.swing.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkComboBoxListener extends MouseAdapter implements PropertyChangeListener, ComboBoxConstants {

    protected final JComboBox<?> comboBox;

    public DarkComboBoxListener(final JComboBox<?> comboBox) {
        this.comboBox = comboBox;
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        comboBox.getEditor().getEditorComponent().requestFocus();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            comboBox.getEditor().getEditorComponent().setComponentOrientation(comboBox.getComponentOrientation());
            comboBox.doLayout();
            comboBox.repaint();
        } else if (PropertyKey.EDITABLE.equals(key)) {
            comboBox.repaint();
        } else if (KEY_IS_TABLE_EDITOR.equals(key) || KEY_IS_TREE_EDITOR.equals(key)) {
            comboBox.revalidate();
            comboBox.repaint();
        }
    }
}
