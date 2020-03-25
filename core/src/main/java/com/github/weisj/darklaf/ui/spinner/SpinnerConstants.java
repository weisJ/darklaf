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
package com.github.weisj.darklaf.ui.spinner;

import javax.swing.*;
import java.awt.*;

public interface SpinnerConstants {
    String KEY_VARIANT = "JSpinner.variant";
    String KEY_IS_TREE_EDITOR = "JSpinner.isTreeCellEditor";
    String KEY_IS_TREE_RENDER = "JSpinner.isTreeCellRenderer";
    String KEY_IS_TABLE_EDITOR = "JSpinner.isTableCellEditor";
    String KEY_IS_TABLE_RENDERER = "JSpinner.isTableCellRenderer";
    String KEY_EDITOR_ALIGNMENT = "JSpinner.cellEditorAlignment";
    String VARIANT_PLUS_MINUS = "plusMinus";

    static boolean usePlusMinusIcons(final JSpinner spinner) {
        return VARIANT_PLUS_MINUS.equals(spinner.getClientProperty(KEY_VARIANT));
    }

    static boolean isTreeOrTableCellEditor(final Component c) {
        return isTreeCellEditor(c) || isTableCellEditor(c);
    }

    static boolean isTreeCellEditor(final Component c) {
        return c instanceof JComponent
               && Boolean.TRUE.equals(((JComponent) c).getClientProperty(KEY_IS_TREE_EDITOR));
    }

    static boolean isTableCellEditor(final Component c) {
        return c instanceof JComponent
               && Boolean.TRUE.equals(((JComponent) c).getClientProperty(KEY_IS_TABLE_EDITOR));
    }
}
