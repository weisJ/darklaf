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
package com.github.weisj.darklaf.ui.combobox;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.DividedWidgetPainter;
import com.github.weisj.darklaf.ui.VisualPaddingProvider;
import com.github.weisj.darklaf.ui.cell.CellUtil;

public class DarkComboBoxBorder implements Border, UIResource, VisualPaddingProvider {

    protected final DarkComboBoxUI ui;
    protected final int borderSize;
    protected final int arcSize;
    protected final Color focusBorderColor;
    protected final Color borderColor;
    protected final Color inactiveBorderColor;

    public DarkComboBoxBorder(final DarkComboBoxUI ui) {
        this.ui = ui;
        arcSize = UIManager.getInt("ComboBox.arc");
        borderSize = UIManager.getInt("ComboBox.borderThickness");
        focusBorderColor = UIManager.getColor("ComboBox.focusBorderColor");
        borderColor = UIManager.getColor("ComboBox.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("ComboBox.inactiveBorderColor");
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

        ui.checkFocus();
        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);

        Color borderColor = getBorderColor(c);
        int dividerLocation = getDividerLocation(comboBox);
        DividedWidgetPainter.paintBorder(g, comboBox, width, height, arcSize, borderSize, dividerLocation,
                isTableCellEditor, isTreeCellEditor, ui.getHasFocus(), borderColor, focusBorderColor);

        g.translate(-x, -y);
    }

    private int getDividerLocation(final JComboBox<?> comboBox) {
        if (comboBox.isEditable()) {
            AbstractButton arrowButton = ui.getArrowButton();
            Rectangle arrowBounds = arrowButton.getBounds();
            boolean leftToRight = comboBox.getComponentOrientation().isLeftToRight();
            return leftToRight ? arrowBounds.x : arrowBounds.x + arrowBounds.width - 1;
        }
        return -1;
    }

    protected void paintCellBorder(final Component c, final int width, final int height,
            final boolean isTableCellEditor, final Graphics2D g, final Color borderColor) {
        g.setColor(borderColor);
        Component parent = c.getParent();
        if (isTableCellEditor && parent instanceof JTable) {
            JTable table = ((JTable) parent);
            CellUtil.paintTableEditorBorder(g, c, table, width, height);
        } else {
            PaintUtil.drawRect(g, 0, 0, width, height, 1);
        }
    }

    protected Color getBorderColor(final Component c) {
        return c.isEnabled() ? borderColor : inactiveBorderColor;
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (ComboBoxConstants.isTreeOrTableCellEditor(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        return new InsetsUIResource(borderSize, borderSize, borderSize, borderSize);
    }

    @Override
    public Insets getVisualPaddings(final Component component) {
        return getBorderInsets(component);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
