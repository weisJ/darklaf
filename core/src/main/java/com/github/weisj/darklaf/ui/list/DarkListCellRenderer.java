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
package com.github.weisj.darklaf.ui.list;

import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import java.awt.*;

public class DarkListCellRenderer extends DefaultListCellRenderer {

    @Override
    public Component getListCellRendererComponent(final JList<?> list, final Object value,
                                                  final int index, final boolean isSelected,
                                                  final boolean cellHasFocus) {
        if (getHorizontalAlignment() != CENTER) {
            if (list.getComponentOrientation().isLeftToRight()) {
                setHorizontalAlignment(LEFT);
            } else {
                setHorizontalAlignment(RIGHT);
            }
        }
        Component comp = null;
        boolean isEditing = Boolean.TRUE.equals(list.getClientProperty(DarkListUI.KEY_IS_EDITING));
        if (isEditing) {
            if (list.getSelectionModel().getLeadSelectionIndex() == index) {
                comp = super.getListCellRendererComponent(list, value, index, false, false);
            }
        }
        if (comp == null) {
            comp = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        }
        CellUtil.setupBackground(comp, list, isSelected, index, DarkListUI.KEY_ALTERNATE_ROW_COLOR,
                                 "List.alternateRowBackground",
                                 "List.selectionNoFocusBackground");
        if (!(isSelected)) {
            comp.setForeground(list.getForeground());
        } else {
            if (DarkUIUtil.hasFocus(list) || DarkUIUtil.getParentOfType(JPopupMenu.class, list) != null) {
                comp.setForeground(list.getSelectionForeground());
                comp.setBackground(list.getSelectionBackground());
            } else {
                comp.setBackground(UIManager.getColor("List.selectionNoFocusBackground"));
                comp.setForeground(UIManager.getColor("List.selectionForegroundInactive"));
            }
        }
        if (getText().isEmpty()) {
            //Fix cell height for empty string.
            setText(" ");
        }
        return comp;
    }
}
