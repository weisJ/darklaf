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
package com.github.weisj.darklaf.ui.spinner;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.ui.DividedWidgetPainter;
import com.github.weisj.darklaf.ui.VisualPaddingProvider;
import com.github.weisj.darklaf.util.DarkUIUtil;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkSpinnerBorder implements Border, UIResource, VisualPaddingProvider {

    protected final Color focusBorderColor;
    protected final Color borderColor;
    protected final Color inactiveBorderColor;
    protected final int arc;
    protected final int borderSize;
    protected final Insets editorInsets;
    protected final Insets cellInsets;

    public DarkSpinnerBorder() {
        focusBorderColor = UIManager.getColor("Spinner.focusBorderColor");
        borderColor = UIManager.getColor("Spinner.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("Spinner.inactiveBorderColor");
        arc = UIManager.getInt("Spinner.arc");
        borderSize = UIManager.getInt("Spinner.borderThickness");
        editorInsets = UIManager.getInsets("Spinner.editorInsets");
        cellInsets = UIManager.getInsets("Spinner.cellEditorInsets");
    }

    @Override
    public void paintBorder(final Component c, final Graphics g2, final int x, final int y, final int width,
            final int height) {
        Graphics2D g = (Graphics2D) g2;
        GraphicsContext config = new GraphicsContext(g);
        g.translate(x, y);

        if (c instanceof JComponent) {
            boolean tableCellEditor = SpinnerConstants.isTableCellEditor(c);
            boolean treeCellEditor = !tableCellEditor && SpinnerConstants.isTreeCellEditor(c);
            int dividerLocation = getDividerLocation(c);
            DividedWidgetPainter.paintBorder(g, (JComponent) c, width, height, arc, borderSize, dividerLocation,
                    tableCellEditor, treeCellEditor, DarkUIUtil.hasFocus(c), borderColor, focusBorderColor);
        }

        g.translate(-x, -y);
        config.restore();
    }

    public int getDividerLocation(final Component c) {
        int dividerLocation = -1;
        if (c instanceof JSpinner) {
            JSpinner spinner = (JSpinner) c;
            JComponent editor = spinner.getEditor();
            if (editor != null) {
                boolean ltr = spinner.getComponentOrientation().isLeftToRight();
                dividerLocation = ltr ? editor.getBounds().x + editor.getWidth() + editorInsets.right
                        : editor.getBounds().x - editorInsets.left;
            }
        }
        return dividerLocation;
    }

    protected Color getBorderColor(final Component c) {
        return c.isEnabled() ? borderColor : inactiveBorderColor;
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (SpinnerConstants.isTreeOrTableCellEditor(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        return new InsetsUIResource(borderSize, borderSize, borderSize, borderSize);
    }

    @Override
    public boolean isBorderOpaque() {
        return true;
    }

    @Override
    public Insets getVisualPaddings(final Component component) {
        return getBorderInsets(component);
    }
}
