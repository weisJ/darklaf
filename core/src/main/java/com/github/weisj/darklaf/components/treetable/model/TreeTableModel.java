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
package com.github.weisj.darklaf.components.treetable.model;

import javax.swing.tree.TreeModel;

public interface TreeTableModel extends TreeModel {

    /**
     * Returns the number of available columns.
     *
     * @return Number of Columns
     */
    int getColumnCount();

    /**
     * Returns the column name.
     *
     * @param  column Column number
     * @return        Column name
     */
    String getColumnName(int column);

    /**
     * Returns the type (class) of a column.
     *
     * @param  column Column number
     * @return        Class
     */
    Class<?> getColumnClass(int column);

    /**
     * Returns the value of a node in a column.
     *
     * @param  node   Node
     * @param  column Column number
     * @return        Value of the node in the column
     */
    Object getValueAt(Object node, int column);

    /**
     * Check if a cell of a node in one column is editable.
     *
     * @param  node   Node
     * @param  column Column number
     * @return        true/false
     */
    boolean isCellEditable(Object node, int column);

    /**
     * Sets a value for a node in one column.
     *
     * @param aValue New value
     * @param node   Node
     * @param column Column number
     */
    void setValueAt(Object aValue, Object node, int column);
}
