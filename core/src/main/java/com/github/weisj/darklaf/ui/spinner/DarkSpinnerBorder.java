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

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkSpinnerBorder implements Border, UIResource {

    protected final Color focusBorderColor;
    protected final Color borderColor;
    protected final Color inactiveBorderColor;
    protected final int arc;
    protected final int borderSize;
    protected Insets insets;
    protected Insets cellInsets;

    public DarkSpinnerBorder() {
        focusBorderColor = UIManager.getColor("Spinner.focusBorderColor");
        borderColor = UIManager.getColor("Spinner.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("Spinner.inactiveBorderColor");
        arc = UIManager.getInt("Spinner.arc");
        borderSize = UIManager.getInt("Spinner.borderThickness");
        cellInsets = UIManager.getInsets("Spinner.cellEditorInsets");
        insets = UIManager.getInsets("Spinner.insets");
        if (insets == null) insets = new Insets(0, 0, 0, 0);
        if (cellInsets == null) cellInsets = new Insets(0, 0, 0, 0);
    }

    @Override
    public void paintBorder(final Component c, final Graphics g2,
                            final int x, final int y, final int width, final int height) {
        boolean tableCellEditor = SpinnerConstants.isTableCellEditor(c);
        boolean treeCellEditor = !tableCellEditor && SpinnerConstants.isTreeCellEditor(c);

        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = new GraphicsContext(g);
        g.translate(x, y);

        int size = tableCellEditor ? 0 : borderSize;

        if (c instanceof JSpinner) {
            JSpinner spinner = (JSpinner) c;
            JComponent editor = spinner.getEditor();
            if (editor != null) {
                int off = spinner.getComponentOrientation().isLeftToRight()
                          ? editor.getBounds().x + editor.getWidth()
                          : editor.getBounds().x - 1 - borderSize;
                g.setColor(getBorderColor(spinner));
                if (!treeCellEditor) {
                    g.fillRect(off, size - 1, 1, height - 2 * size + 1);
                } else {
                    g.fillRect(off, 0, 1, height);
                }
            }
        }

        if (!tableCellEditor && !treeCellEditor && DarkUIUtil.hasFocus(c)) {
            DarkUIUtil.paintFocusBorder(g, width, height, arc, borderSize);
        }

        g.setColor(getBorderColor(c));
        if (!tableCellEditor && !treeCellEditor) {
            if (DarkUIUtil.hasFocus(c)) {
                g.setColor(focusBorderColor);
            }
            DarkUIUtil.paintLineBorder(g, size, size, width - 2 * size, height - 2 * size, arc);
        } else if (tableCellEditor && (c.getParent() instanceof JTable)) {
            JTable table = (JTable) c.getParent();
            if (!table.getShowHorizontalLines()) {
                g.fillRect(0, 0, width, 1);
                g.fillRect(0, height - 1, width, 1);
            }
            if (!table.getShowVerticalLines()) {
                g.fillRect(0, 0, 1, height);
                g.fillRect(width - 1, 0, 1, height);
            }
        } else {
            DarkUIUtil.drawRect(g, 0, 0, width, height, 1);
        }

        g.translate(-x, -y);
        config.restore();
    }

    protected Color getBorderColor(final Component c) {
        return c.isEnabled() ? borderColor : inactiveBorderColor;
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (SpinnerConstants.isTreeOrTableCellEditor(c)) {
            return new InsetsUIResource(cellInsets.top, cellInsets.left, cellInsets.bottom, cellInsets.right);
        }
        return new InsetsUIResource(insets.top, insets.left, insets.bottom, insets.right);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }
}
