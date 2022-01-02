/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.spinner;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;

import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;

public class DarkSpinnerListener extends MouseAdapter
        implements PropertyChangeListener, FocusListener, SpinnerConstants {

    protected final JSpinner spinner;
    protected final DarkSpinnerUI ui;

    public DarkSpinnerListener(final JSpinner spinner, final DarkSpinnerUI ui) {
        this.spinner = spinner;
        this.ui = ui;
    }

    public void install() {
        spinner.addMouseListener(this);
        spinner.addMouseWheelListener(this);
        spinner.addPropertyChangeListener(this);
    }

    public void uninstall() {
        spinner.removeMouseListener(this);
        spinner.removeMouseWheelListener(this);
        spinner.removePropertyChangeListener(this);
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        super.mousePressed(e);
        spinner.getEditor().requestFocusInWindow();
    }

    @Override
    public void focusGained(final FocusEvent e) {
        spinner.repaint();
    }

    @Override
    public void focusLost(final FocusEvent e) {
        spinner.repaint();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        Component editorComponent = ui.getEditorComponent();
        JComponent editor = ui.getEditor();
        if (PropertyKey.OPAQUE.equals(key)) {
            boolean val = Boolean.TRUE.equals(evt.getNewValue());
            spinner.getEditor().setOpaque(val);
            if (editorComponent instanceof JComponent) {
                ((JComponent) editorComponent).setOpaque(val);
            }
        } else if (KEY_IS_TABLE_EDITOR.equals(key)) {
            if (Boolean.FALSE.equals(evt.getNewValue())) {
                if (editor instanceof JSpinner.DefaultEditor) {
                    // if editor alignment isn't set in LAF, we get 0 (CENTER) here
                    int alignment = UIManager.getInt("Spinner.editorAlignment");
                    JTextField text = ((JSpinner.DefaultEditor) editor).getTextField();
                    text.setHorizontalAlignment(alignment);
                }
            }
            spinner.revalidate();
            spinner.repaint();
        } else if (KEY_EDITOR_ALIGNMENT.equals(key) && SpinnerConstants.isTableCellEditor(spinner)) {
            if (editorComponent instanceof JTextField && evt.getNewValue() instanceof Integer) {
                int alignment = (Integer) evt.getNewValue();
                ((JTextField) editorComponent).setHorizontalAlignment(alignment);
            }
            spinner.revalidate();
        } else if (KEY_VARIANT.equals(key)) {
            spinner.repaint();
        } else if (KEY_IS_TREE_EDITOR.equals(key)) {
            spinner.revalidate();
            spinner.repaint();
        } else if (PropertyKey.ENABLED.equals(key)) {
            ui.updateBackground();
        }
    }

    @Override
    public void mouseWheelMoved(final MouseWheelEvent e) {
        super.mouseWheelMoved(e);
        if (!DarkUIUtil.hasFocus(spinner)) return;
        int rotation = e.getWheelRotation();
        try {
            if (rotation > 0) {
                Object previousValue = spinner.getModel().getPreviousValue();
                if (previousValue != null) {
                    spinner.setValue(previousValue);
                    e.consume();
                }
            } else if (rotation < 0) {
                Object nextValue = spinner.getModel().getNextValue();
                if (nextValue != null) {
                    spinner.setValue(nextValue);
                    e.consume();
                }
            }
        } catch (Exception ignored) {
            // Invalid value
        }
    }
}
