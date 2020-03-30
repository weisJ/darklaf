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
package com.github.weisj.darklaf.ui.combobox;

import com.github.weisj.darklaf.ui.cell.CellUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;

public class DarkComboBoxBorder implements Border, UIResource {

    protected final DarkComboBoxUI ui;
    protected Insets boxPadding;
    protected Insets cellPadding;
    protected final int borderSize;
    protected final int arcSize;
    protected final Color focusBorderColor;
    protected final Color inactiveBackground;
    protected final Color arrowBackground;
    protected final Color background;
    protected final Color borderColor;
    protected final Color inactiveBorderColor;

    public DarkComboBoxBorder(final DarkComboBoxUI ui) {
        this.ui = ui;
        arcSize = UIManager.getInt("ComboBox.arc");
        boxPadding = UIManager.getInsets("ComboBox.insets");
        borderSize = UIManager.getInt("ComboBox.borderThickness");
        background = UIManager.getColor("ComboBox.activeBackground");
        inactiveBackground = UIManager.getColor("ComboBox.inactiveBackground");
        focusBorderColor = UIManager.getColor("ComboBox.focusBorderColor");
        borderColor = UIManager.getColor("ComboBox.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("ComboBox.inactiveBorderColor");
        arrowBackground = UIManager.getColor("ComboBox.arrowBackground");
        cellPadding = UIManager.getInsets("ComboBox.cellEditorInsets");
        if (boxPadding == null) boxPadding = new Insets(0, 0, 0, 0);
        if (cellPadding == null) cellPadding = new Insets(0, 0, 0, 0);
    }

    @Override
    public void paintBorder(final Component c, final Graphics g2, final int x, final int y, final int width,
                            final int height) {
        JComboBox<?> comboBox = ui.getComboBox();
        AbstractButton arrowButton = ui.getArrowButton();
        if (comboBox == null || arrowButton == null) {
            return;
        }

        final boolean isTableCellEditor = ComboBoxConstants.isTableCellEditor(comboBox);
        final boolean isTreeCellEditor = ComboBoxConstants.isTreeCellEditor(comboBox);
        final boolean isCellEditor = isTableCellEditor || isTreeCellEditor;
        int bSize = !isCellEditor ? borderSize : 0;

        ui.checkFocus();
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = new GraphicsContext(g);
        g.translate(x, y);

        Color borderColor = getBorderColor(c);

        if (comboBox.isEditable()) {
            paintArrowBackground(width, height, comboBox, arrowButton, isCellEditor,
                                 bSize, arcSize, g, borderColor);
        }

        if (!isCellEditor) {
            if (ui.getHasFocus()) {
                DarkUIUtil.paintFocusBorder(g, width, height, arcSize, borderSize);
                g.setColor(focusBorderColor);
            } else {
                g.setColor(borderColor);
            }
            DarkUIUtil.paintLineBorder(g, bSize, bSize, width - 2 * bSize,
                                       height - 2 * bSize, arcSize);
        } else {
            paintCellBorder(c, width, height, isTableCellEditor, g, borderColor);
        }

        g.translate(-x, -y);
        config.restore();
    }

    public void paintArrowBackground(final int width, final int height, final JComboBox<?> comboBox,
                                     final AbstractButton arrowButton, final boolean isCellEditor,
                                     final int bSize, final int arc,
                                     final Graphics2D g, final Color borderColor) {
        Rectangle arrowBounds = arrowButton.getBounds();
        boolean leftToRight = comboBox.getComponentOrientation().isLeftToRight();
        int off = leftToRight ? arrowBounds.x : arrowBounds.x + arrowBounds.width;
        Area rect;
        Area iconRect = new Area(new Rectangle(off, 0, width, height));
        if (!isCellEditor) {
            rect = new Area(new RoundRectangle2D.Double(bSize - 1, bSize - 1, width - 2 * bSize + 1,
                                                        height - 2 * bSize + 1, arc, arc));
        } else {
            rect = new Area(new Rectangle(0, 0, width, height));
        }
        if (leftToRight) {
            rect.intersect(iconRect);
        } else {
            rect.subtract(iconRect);
        }
        g.setPaint(getArrowBackground(comboBox));
        g.fill(rect);

        g.setColor(borderColor);
        g.fillRect(off, bSize - 1, 1, height - 2 * bSize + 1);
    }

    protected void paintCellBorder(final Component c, final int width, final int height,
                                   final boolean isTableCellEditor, final Graphics2D g, final Color borderColor) {
        g.setColor(borderColor);
        Component parent = c.getParent();
        if (isTableCellEditor && parent instanceof JTable) {
            JTable table = ((JTable) parent);
            CellUtil.paintTableEditorBorder(g, c, table, width, height);
        } else {
            DarkUIUtil.drawRect(g, 0, 0, width, height, 1);
        }
    }

    protected Color getArrowBackground(final JComboBox<?> c) {
        if (!c.isEnabled()) return inactiveBackground;
        if (c.isEditable()) return arrowBackground;
        return background;
    }

    protected Color getBorderColor(final Component c) {
        return c.isEnabled() ? borderColor : inactiveBorderColor;
    }


    @Override
    public Insets getBorderInsets(final Component c) {
        if (ComboBoxConstants.isTreeOrTableCellEditor(c)) {
            return CellUtil.adjustEditorInsets(new InsetsUIResource(cellPadding.top, cellPadding.left,
                                                                    cellPadding.bottom, cellPadding.right), c);
        }
        if (c.getComponentOrientation().isLeftToRight()) {
            return new InsetsUIResource(boxPadding.top, boxPadding.left, boxPadding.bottom, borderSize);
        } else {
            return new InsetsUIResource(boxPadding.top, borderSize, boxPadding.bottom, boxPadding.right);
        }
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
