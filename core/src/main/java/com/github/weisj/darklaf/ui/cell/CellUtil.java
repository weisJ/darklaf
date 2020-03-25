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
package com.github.weisj.darklaf.ui.cell;

import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import java.awt.*;

public class CellUtil {

    public static void setupForeground(final JComponent comp, final JComponent parent, final boolean selected,
                                       final String activeKey, final String inactiveKey) {
        setupForeground(comp, parent, selected, UIManager.getColor(activeKey), inactiveKey);
    }

    public static void setupForeground(final JComponent comp, final JTable parent, final boolean selected,
                                       final String inactiveKey) {
        setupForeground(comp, parent, selected, parent.getSelectionForeground(), inactiveKey);
    }

    public static void setupForeground(final JComponent comp, final JComponent parent, final boolean selected,
                                       final Color activeColor, final String inactiveKey) {
        if (selected) {
            if (DarkUIUtil.hasFocus(parent)) {
                comp.setForeground(activeColor);
            } else {
                comp.setForeground(UIManager.getColor(inactiveKey));
            }
        } else {
            comp.setForeground(parent.getForeground());
        }
    }

    public static void setupBackground(final JComponent comp, final JTable parent,
                                       final boolean selected, final int row,
                                       final String altBgKey, final String altColorKey) {
        setupBackground(comp, parent, selected, row, altBgKey,
                        parent.getBackground(), altColorKey, parent.getSelectionBackground());
    }

    public static void setupBackground(final JComponent comp, final JTable parent,
                                       final boolean selected, final int row,
                                       final String altBgKey, final String colorKey, final String altColorKey) {
        setupBackground(comp, parent, selected, row, altBgKey, UIManager.getColor(colorKey),
                        altColorKey, parent.getSelectionBackground());
    }

    protected static void setupBackground(final JComponent comp, final JComponent parent,
                                          final boolean selected, final int row,
                                          final String altBgKey, final Color bgColor, final String altColorKey,
                                          final Color selectionBackground) {
        boolean alternativeRow = Boolean.TRUE.equals(parent.getClientProperty(altBgKey));
        Color alternativeRowColor = UIManager.getColor(altColorKey);
        Color background = alternativeRow && row % 2 == 1 ? alternativeRowColor : bgColor;

        if (selected) {
            comp.setBackground(selectionBackground);
        } else {
            comp.setBackground(background);
        }
    }
}
