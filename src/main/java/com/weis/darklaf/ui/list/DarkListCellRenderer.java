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
package com.weis.darklaf.ui.list;

import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class DarkListCellRenderer extends DefaultListCellRenderer {

    @Override
    public Component getListCellRendererComponent(@NotNull final JList<?> list, final Object value,
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
        boolean isEditing = Boolean.TRUE.equals(list.getClientProperty("JList.isEditing"));
        if (isEditing) {
            if (list.getSelectionModel().getLeadSelectionIndex() == index) {
                comp = super.getListCellRendererComponent(list, value, index, false, false);
            }
        }
        if (comp == null) {
            comp = super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus);
        }
        boolean alternativeRow = Boolean.TRUE.equals(list.getClientProperty("JList.alternateRowColor"));
        var layout = list.getLayoutOrientation();
        if (layout == JList.VERTICAL) {
            alternativeRow = alternativeRow && index % 2 == 1;
        } else if (layout == JList.VERTICAL_WRAP || layout == JList.HORIZONTAL_WRAP) {
            var ui = list.getUI();
            if (ui instanceof DarkListUI) {
                int row = ((DarkListUI) ui).convertModelToRow(index);
                alternativeRow = alternativeRow && row % 2 == 1;
            } else {
                alternativeRow = false;
            }
        }
        Color alternativeRowColor = UIManager.getColor("List.alternateRowBackground");
        Color normalColor = list.getBackground();
        var background = alternativeRow ? alternativeRowColor : normalColor;
        if (!(isSelected)) {
            comp.setBackground(background);
            comp.setForeground(list.getForeground());
        } else {
            if (DarkUIUtil.hasFocus(list)) {
                comp.setForeground(list.getSelectionForeground());
            } else {
                comp.setForeground(UIManager.getColor("List.selectionForegroundInactive"));
            }
            comp.setBackground(list.getSelectionBackground());
        }
        return comp;
    }
}
