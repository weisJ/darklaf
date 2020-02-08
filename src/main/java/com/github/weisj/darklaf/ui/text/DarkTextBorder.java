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
package com.github.weisj.darklaf.ui.text;

import com.github.weisj.darklaf.ui.table.TextFieldTableCellEditorBorder;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import javax.swing.text.JTextComponent;
import java.awt.*;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTextBorder implements Border, UIResource {

    public final static int PADDING = 4;
    private static final Border editorBorder = new TextFieldTableCellEditorBorder();
    protected Color errorBorderColor;
    protected Color focusErrorBorderColor;
    protected Color focusBorderColor;
    protected Color borderColor;
    protected Color inactiveBorderColor;

    protected int borderSize;
    protected int arc;
    protected int minimumArc;
    protected int searchArc;

    public DarkTextBorder() {
        focusErrorBorderColor = UIManager.getColor("TextField.border.focusError");
        focusBorderColor = UIManager.getColor("TextField.border.focus");
        errorBorderColor = UIManager.getColor("TextField.border.error");
        borderColor = UIManager.getColor("TextField.border.enabled");
        inactiveBorderColor = UIManager.getColor("TextField.border.disabled");
        borderSize = UIManager.getInt("TextField.borderThickness");
        arc = UIManager.getInt("TextField.arc");
        searchArc = UIManager.getInt("TextField.searchArc");
        minimumArc = UIManager.getInt("TextField.minimumArc");
    }

    public void paintBorder(final Component c, final Graphics g2, final int x, final int y,
                            final int width, final int height) {
        if (isCellEditor(c)) {
            editorBorder.paintBorder(c, g2, x, y, width, height);
            return;
        }
        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        int arcSize = getArcSize(c);
        int focusArcSize = getFocusArcSize(c);
        if (!DarkTextFieldUI.isSearchField(c)) {
            if (hasError(c)) {
                DarkUIUtil.paintOutlineBorder(g, width, height, focusArcSize, borderSize,
                                              c.hasFocus(), DarkUIUtil.Outline.error);
            } else if (c.hasFocus()) {
                DarkUIUtil.paintFocusBorder(g, width, height, focusArcSize, borderSize);
            }
            g.setColor(getBorderColor(c));

            if (DarkTextFieldUI.chooseAlternativeArc(c)) {
                DarkUIUtil.paintLineBorder(g, borderSize, borderSize, width - 2 * borderSize,
                                           height - 2 * borderSize, arcSize, true);
            } else {
                DarkUIUtil.paintLineBorder(g, borderSize, borderSize, width - 2 * borderSize,
                                           height - 2 * borderSize, 0, true);
            }
        } else if (DarkTextFieldUI.isSearchField(c)) {
            g.setColor(getBorderColor(c));
            if (((JComponent) c).getClientProperty("JTextField.Search.noBorderRing") != Boolean.TRUE) {
                if (c.hasFocus()) {
                    DarkUIUtil.paintFocusBorder(g, width, height, arcSize, borderSize);
                }
                g.setColor(getBorderColor(c));
                DarkUIUtil.paintLineBorder(g, borderSize, borderSize, width - 2 * borderSize,
                                           height - 2 * borderSize, arcSize, true);
            }
        }
        g.translate(-x, -y);
        config.restore();
    }

    @Contract("null -> false")
    protected static boolean isCellEditor(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTextField.cellEditor"));
    }

    protected int getArcSize(final Component c) {
        boolean alt = DarkTextFieldUI.chooseAlternativeArc(c);
        if (!alt && !DarkTextFieldUI.isSearchField(c)) {
            return 0;
        }
        return DarkTextFieldUI.isSearchField(c) ? (alt ? arc : searchArc) : (alt ? searchArc : arc);
    }

    protected int getFocusArcSize(final Component c) {
        boolean alt = DarkTextFieldUI.chooseAlternativeArc(c);
        if (!alt && !DarkTextFieldUI.isSearchField(c)) {
            return minimumArc;
        }
        return DarkTextFieldUI.isSearchField(c) ? (alt ? arc : searchArc) : (alt ? searchArc : arc);
    }

    @Contract("null -> false")
    protected static boolean hasError(final Component c) {
        return c instanceof JComponent
                && Boolean.TRUE.equals(((JComponent) c).getClientProperty("JTextField.hasError"));
    }

    protected Color getBorderColor(@NotNull final Component c) {
        boolean editable = !(c instanceof JTextComponent) || ((JTextComponent) c).isEditable();
        boolean focus = DarkUIUtil.hasFocus(c);
        boolean error = hasError(c);
        return getBorderColor(focus, error, editable, c.isEnabled());
    }

    protected Color getBorderColor(final boolean focus, final boolean error,
                                   final boolean editable, final boolean enabled) {
        if (focus) {
            return error ? focusErrorBorderColor : focusBorderColor;
        } else if (error) {
            return errorBorderColor;
        } else {
            return enabled && editable ? borderColor : inactiveBorderColor;
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isCellEditor(c)) {
            return editorBorder.getBorderInsets(c);
        }
        Insets insets = new Insets(borderSize + PADDING, borderSize + PADDING,
                                   borderSize + PADDING, borderSize + PADDING);
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
