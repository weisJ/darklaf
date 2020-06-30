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
package com.github.weisj.darklaf.components.togglebuttonlist;

import javax.swing.*;

import com.github.weisj.darklaf.ui.togglebutton.DarkToggleButtonUI;

/**
 * Each item in the CheckBxoList will be an instance of this class
 *
 * @author Naveed Quadri
 */
public class CheckBoxListItem extends JCheckBox {

    private Object value = null;

    public CheckBoxListItem(final Object itemValue, final boolean selected) {
        super("", selected);
        setValue(itemValue);
        putClientProperty(DarkToggleButtonUI.KEY_IS_TABLE_EDITOR, true);
    }

    public Object getValue() {
        return value;
    }

    /**
     * The value of the JCheckbox label
     *
     * @param value the new value.
     */
    public void setValue(final Object value) {
        this.value = value;
        setText(value == null ? "" : "" + value);
    }

    @Override
    public String toString() {
        return value.toString();
    }
}
