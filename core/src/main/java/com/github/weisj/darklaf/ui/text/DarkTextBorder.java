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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public class DarkTextBorder implements Border, UIResource {

    protected final Color errorBorderColor;
    protected final Color focusErrorBorderColor;
    protected final Color focusBorderColor;
    protected final Color borderColor;
    protected final Color inactiveBorderColor;
    protected final Color inactiveFocusBorderColor;

    protected final int borderSize;
    protected final int arc;
    protected final int searchArc;
    protected final int focusArc;
    protected final int searchFocusArc;
    protected Insets padding;
    protected final Icon showIcon;

    public DarkTextBorder() {
        focusErrorBorderColor = UIManager.getColor("TextField.border.focusError");
        focusBorderColor = UIManager.getColor("TextField.border.focus");
        errorBorderColor = UIManager.getColor("TextField.border.error");
        borderColor = UIManager.getColor("TextField.border.enabled");
        inactiveBorderColor = UIManager.getColor("TextField.border.disabled");
        inactiveFocusBorderColor = UIManager.getColor("TextField.border.disabled.focus");
        borderSize = UIManager.getInt("TextField.borderThickness");
        arc = UIManager.getInt("TextField.arc");
        focusArc = UIManager.getInt("TextField.focusArc");
        searchArc = UIManager.getInt("TextField.searchArc");
        searchFocusArc = UIManager.getInt("TextField.searchFocusArc");
        padding = UIManager.getInsets("TextField.insets");
        showIcon = UIManager.getIcon("PasswordField.show.icon");
        if (padding == null) padding = new Insets(0, 0, 0, 0);
    }

    protected static boolean hasError(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkTextUI.KEY_HAS_ERROR);
    }

    protected int getArcSize(final Component c) {
        return DarkTextFieldUI.isSearchField(c) ? searchArc : arc;
    }

    protected int getFocusArcSize(final Component c) {
        return DarkTextFieldUI.isSearchField(c) ? searchFocusArc : focusArc;
    }

    public int getBorderSize() {
        return borderSize;
    }

    public void paintBorder(final Component c, final Graphics g2, final int x, final int y,
                            final int width, final int height) {
        boolean editable = !(c instanceof JTextComponent) || ((JTextComponent) c).isEditable();
        boolean focus = DarkUIUtil.hasFocus(c);
        boolean error = hasError(c);

        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        int arcSize = getArcSize(c);
        int focusArcSize = getFocusArcSize(c);
        if (error) {
            PaintUtil.paintOutlineBorder(g, width, height, focusArcSize, borderSize,
                                         c.hasFocus(), PaintUtil.Outline.error);
        } else if (c.hasFocus()) {
            PaintUtil.paintFocusBorder(g, width, height, focusArcSize, borderSize, editable);
        }
        g.setColor(getBorderColor(focus, error, editable, c.isEnabled()));
        PaintUtil.paintLineBorder(g, borderSize, borderSize, width - 2 * borderSize,
                                  height - 2 * borderSize, arcSize);
        g.translate(-x, -y);
        config.restore();
    }

    protected Color getBorderColor(final boolean focus, final boolean error,
                                   final boolean editable, final boolean enabled) {
        if (focus) {
            return error ? focusErrorBorderColor
                    : enabled && editable ? focusBorderColor : inactiveFocusBorderColor;
        } else if (error) {
            return errorBorderColor;
        } else {
            return enabled && editable ? borderColor : inactiveBorderColor;
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        Insets insets = new Insets(borderSize + padding.top, borderSize + padding.left,
                                   borderSize + padding.bottom, borderSize + padding.right);
        if (DarkTextFieldUI.isSearchField(c)) {
            int searchWidth = DarkTextFieldUI.getSearchIcon(c).getIconWidth();
            int clearWidth = DarkTextFieldUI.getClearIcon(false).getIconWidth();
            insets.left += padding.left + searchWidth;
            insets.right += padding.right + clearWidth;
        } else if (DarkPasswordFieldUI.hasShowIcon(c)) {
            int eyeWidth = showIcon.getIconWidth();
            if (c.getComponentOrientation().isLeftToRight()) {
                insets.right += padding.right + eyeWidth;
            } else {
                insets.left += padding.left + eyeWidth;
            }
        }
        return insets;
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }
}
