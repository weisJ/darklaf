/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.components.togglebuttonlist;

import java.awt.*;

import javax.swing.*;

public class ToggleButtonListCellRenderer implements ListCellRenderer<JToggleButton> {

    @Override
    public Component getListCellRendererComponent(final JList<? extends JToggleButton> list, final JToggleButton value,
            final int index, final boolean isSelected, final boolean cellHasFocus) {
        value.setBackground(isSelected ? list.getSelectionBackground() : list.getBackground());
        value.setForeground(isSelected ? list.getSelectionForeground() : list.getForeground());
        value.setEnabled(list.isEnabled());
        value.setFont(list.getFont());
        value.setFocusPainted(cellHasFocus);
        value.setBorderPainted(true);
        value.setBorder(isSelected ? UIManager.getBorder("List.focusCellHighlightBorder")
                : UIManager.getBorder("List.cellNoFocusBorder"));

        return value;
    }
}
