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
package com.github.weisj.darklaf.ui.table;

import javax.swing.*;

import com.github.weisj.darklaf.ui.cell.CellConstants;
import com.github.weisj.darklaf.util.PropertyUtil;

public interface TableConstants extends CellConstants {

    String KEY_IS_TABLE_EDITOR = "JComponent.isTableEditor";
    String KEY_IS_TABLE_RENDERER = "JComponent.isTableRenderer";
    String KEY_ALTERNATE_ROW_COLOR = "JTable.alternateRowColor";
    String KEY_RENDER_BOOLEAN_AS_CHECKBOX = "JTable.renderBooleanAsCheckBox";
    String KEY_BOOLEAN_RENDER_TYPE = "JTable.booleanRenderType";
    String KEY_FULL_ROW_FOCUS_BORDER = "JTable.rowFocusBorder";
    String KEY_FORCE_LEFT_BORDER = "JTable.forcePaintLeft";
    String KEY_FORCE_RIGHT_BORDER = "JTable.forcePaintRight";
    String KEY_FILE_CHOOSER_PARENT = "JTable.fileChooserParent";
    String KEY_FILENAME_COLUMN_INDEX = "JTable.fileNameColumnIndex";
    String KEY_HORIZONTAL_LINES = "showHorizontalLines";
    String KEY_VERTICAL_LINES = "showVerticalLines";
    String KEY_IS_FILE_LIST = "Table.isFileList";
    String KEY_IS_PRINT_MODE = "Table.printMode";
    String KEY_CELL_VALUE_DETERMINES_EDITOR_CLASS = "Table.cellDeterminesClass";

    static boolean isBooleanRenderingEnabled(final JTable table) {
        return PropertyUtil.getBooleanProperty(table, TableConstants.KEY_RENDER_BOOLEAN_AS_CHECKBOX);
    }

    static boolean useBooleanEditorForValue(final Object value, final JTable table, final int column,
            final boolean checkTableProperty) {
        return value instanceof Boolean && (Boolean.class.isAssignableFrom(table.getColumnClass(column))
                || !checkTableProperty || TableConstants.isBooleanRenderingEnabled(table));
    }

    static boolean useBooleanEditorForValue(final Object value, final JTable table, final int column) {
        return useBooleanEditorForValue(value, table, column, true);
    }
}
