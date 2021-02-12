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

import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.text.Caret;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkTextListener implements FocusListener, PropertyChangeListener {

    private final JTextComponent editor;
    private final DarkTextUI ui;

    public DarkTextListener(final JTextComponent editor, final DarkTextUI ui) {
        this.editor = editor;
        this.ui = ui;
    }

    @Override
    public void focusGained(final FocusEvent e) {
        Caret caret = editor.getCaret();
        if (caret instanceof DarkCaret) {
            ((DarkCaret) caret).setPaintSelectionHighlight(true);
        }
        editor.repaint();
    }

    @Override
    public void focusLost(final FocusEvent e) {
        Caret caret = editor.getCaret();
        JPopupMenu popupMenu = editor.getComponentPopupMenu();
        Component other = e.getOppositeComponent();
        MenuElement[] path = MenuSelectionManager.defaultManager().getSelectedPath();
        if (popupMenu != null && other != null && SwingUtilities.isDescendingFrom(popupMenu, other)
                || path != null && path.length > 0 && path[0] == popupMenu)
            return;
        if (caret instanceof DarkCaret) {
            ((DarkCaret) caret).setPaintSelectionHighlight(false);
        }
        editor.repaint();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (PropertyKey.ANCESTOR.equals(key)
                || DarkTextUI.KEY_IS_LIST_EDITOR.equals(key)
                || DarkTextUI.KEY_IS_TABLE_EDITOR.equals(key)
                || DarkTextUI.KEY_IS_TREE_EDITOR.equals(key)) {
            ui.updateMargins();
        } else if (DarkTextUI.KEY_ROUNDED_SELECTION.equals(key)) {
            boolean rounded = PropertyUtil.getBooleanProperty(editor, DarkTextUI.KEY_ROUNDED_SELECTION);
            ui.getDarkCaret().setRoundedSelectionEdges(rounded);
            editor.repaint();
        } else if (DarkTextUI.KEY_HAS_ERROR.equals(key)
                || DarkTextUI.KEY_HAS_WARNING.equals(key)) {
            editor.repaint();
        } else if (DarkTextUI.KEY_EXTEND_LINE_SELECTION.equals(key)) {
            boolean extendLines = PropertyUtil.getBooleanProperty(editor, DarkTextUI.KEY_EXTEND_LINE_SELECTION);
            ui.getDarkCaret().setLineExtendingEnabled(extendLines);
            editor.repaint();
        } else if ("border".equals(key)) {
            ui.installBorder();
        } else if (PropertyKey.ENABLED.equals(key) || PropertyKey.EDITABLE.equals(key)) {
            ui.updateBackground(editor);
        }
    }
}
