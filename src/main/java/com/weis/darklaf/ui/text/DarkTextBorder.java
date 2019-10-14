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
package com.weis.darklaf.ui.text;

import com.weis.darklaf.ui.table.TextFieldTableCellEditorBorder;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTextBorder implements Border, UIResource {

    private static final Border editorBorder = new TextFieldTableCellEditorBorder();

    public final static int BORDER_SIZE = 3;
    public final static int PADDING = 4;

    public void paintBorder(final Component c, final Graphics g2, final int x, final int y,
                            final int width, final int height) {
        if (isCellEditor(c)) {
            editorBorder.paintBorder(c, g2, x, y, width, height);
            return;
        }

        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        int arcSize = DarkTextFieldUI.getArcSize(c);
        if (!DarkTextFieldUI.isSearchField(c)) {
            if (DarkTextFieldUI.hasError(c)) {
                DarkUIUtil.paintOutlineBorder(g, width, height, arcSize, true,
                                              c.hasFocus(), DarkUIUtil.Outline.error);
            } else if (c.hasFocus()) {
                DarkUIUtil.paintFocusBorder(g, width, height, arcSize, true);
            }
            g.setColor(DarkTextFieldUI.getBorderColor(c));
            if (DarkTextFieldUI.chooseAlternativeArc(c)) {
                DarkUIUtil.paintLineBorder(g, BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE,
                                           height - 2 * BORDER_SIZE, arcSize, true);
            } else {
                g.drawRect(BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE, height - 2 * BORDER_SIZE);
            }
        } else if (DarkTextFieldUI.isSearchField(c)) {
            g.setColor(DarkTextFieldUI.getBorderColor(c));
            if (((JComponent) c).getClientProperty("JTextField.Search.noBorderRing") != Boolean.TRUE) {
                if (c.hasFocus()) {
                    DarkUIUtil.paintFocusBorder(g, width, height, arcSize, true);
                }
                g.setColor(DarkTextFieldUI.getBorderColor(c));
                DarkUIUtil.paintLineBorder(g, BORDER_SIZE, BORDER_SIZE, width - 2 * BORDER_SIZE,
                                           height - 2 * BORDER_SIZE, arcSize, true);
            }
        }
        g.translate(-x, -y);
        config.restore();
    }

    protected static boolean isCellEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTextField.cellEditor"));
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isCellEditor(c)) {
            return editorBorder.getBorderInsets(c);
        }
        Insets insets = new Insets(BORDER_SIZE + PADDING, BORDER_SIZE + PADDING,
                                   BORDER_SIZE + PADDING, BORDER_SIZE + PADDING);
        if (DarkTextFieldUI.isSearchField(c)) {
            int searchWidth = DarkTextFieldUI.getSearchIcon(c).getIconWidth();
            int clearWidth = DarkTextFieldUI.getClearIcon(false).getIconWidth();
            insets.left += PADDING + searchWidth;
            insets.right += PADDING + clearWidth;
        } else if (DarkPasswordFieldUI.hasShowIcon(c)) {
            int eyeWidth = DarkPasswordFieldUI.getShowIcon().getIconWidth();
            insets.right += PADDING + eyeWidth;
        }
        return insets;
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
