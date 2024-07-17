/*
 * MIT License
 *
 * Copyright (c) 2019-2024 Jannis Weis
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
package com.github.weisj.darklaf.ui.list;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.delegate.ListCellRendererDelegate;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.util.LazyValue;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkListCellRendererDelegate extends ListCellRendererDelegate<Object> implements SwingConstants {

    private static final LazyValue<DefaultListCellRenderer> DEFAULT_RENDERER =
            new LazyValue<>(DefaultListCellRenderer::new);

    private final Border cellBorder;
    private final Border cellFocusBorder;

    public DarkListCellRendererDelegate() {
        super(null);
        cellBorder = UIManager.getBorder("List.cellNoFocusBorder");
        cellFocusBorder = UIManager.getBorder("List.focusSelectedCellHighlightBorder");
    }

    @Override
    public void setDelegate(final ListCellRenderer<Object> delegate) {
        if (delegate == null) {
            super.setDelegate(DEFAULT_RENDERER.get());
        } else {
            super.setDelegate(delegate);
        }
    }

    @Override
    public Component getListCellRendererComponent(final JList<?> list, final Object value, final int index,
            final boolean isSelected, final boolean cellHasFocus) {
        boolean isEditing = PropertyUtil.getBooleanProperty(list, DarkListUI.KEY_IS_EDITING);
        boolean leadIndex = isEditing && list.getSelectionModel().getLeadSelectionIndex() == index;
        boolean sel = isSelected && !leadIndex;
        boolean focus = cellHasFocus && !leadIndex;
        Component renderer = super.getListCellRendererComponent(list, value, index, sel, focus);
        if (renderer instanceof JLabel label) {
            if (label.getText().isEmpty()) {
                // Fix cell height for empty string.
                label.setText(" ");
            }
        }
        CellUtil.setupListBackground(renderer, list, isSelected, index);
        CellUtil.setupListForeground(renderer, list, isSelected);
        PropertyUtil.installBorder((JComponent) renderer, setupBorder(list, isSelected, cellHasFocus, index));
        return renderer;
    }

    private Border setupBorder(final JList<?> list, final boolean isSelected, final boolean cellHasFocus,
            final int index) {
        if (cellHasFocus && !isSelected && list.getLeadSelectionIndex() == index) {
            return cellFocusBorder;
        } else {
            return cellBorder;
        }
    }
}
