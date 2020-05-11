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
 *
 */
package com.github.weisj.darklaf.ui.list;

import java.awt.*;

import javax.swing.*;

import org.jdesktop.swingx.renderer.DefaultListRenderer;

import com.github.weisj.darklaf.delegate.ListCellRendererDelegate;
import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class DarkListCellRendererDelegate extends ListCellRendererDelegate<Object> implements SwingConstants {

    public DarkListCellRendererDelegate() {
        super(null);
    }

    @Override
    public void setDelegate(final ListCellRenderer<Object> delegate) {
        if (delegate == null) {
            super.setDelegate(new DefaultListRenderer());
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
        if (renderer instanceof JLabel) {
            JLabel label = ((JLabel) renderer);
            if (label.getText().isEmpty()) {
                // Fix cell height for empty string.
                label.setText(" ");
            }
            if (label.getHorizontalAlignment() != CENTER) {
                if (list.getComponentOrientation().isLeftToRight()) {
                    label.setHorizontalAlignment(LEFT);
                } else {
                    label.setHorizontalAlignment(RIGHT);
                }
            }
        }
        CellUtil.setupListBackground(renderer, list, isSelected, index);
        CellUtil.setupListForeground(renderer, list, isSelected);
        return renderer;
    }
}
