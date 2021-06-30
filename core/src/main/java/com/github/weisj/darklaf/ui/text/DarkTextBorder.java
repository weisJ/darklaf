/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.text;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.graphics.Outline;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

/** @author Jannis Weis */
public class DarkTextBorder implements Border, UIResource, VisualPaddingProvider {

    protected final Color errorBorderColor;
    protected final Color focusErrorBorderColor;
    protected final Color warningBorderColor;
    protected final Color focusWarningBorderColor;
    protected final Color focusBorderColor;
    protected final Color borderColor;
    protected final Color inactiveBorderColor;
    protected final Color inactiveFocusBorderColor;

    protected final int borderSize;
    protected final int arc;
    protected final int searchArc;
    protected final int focusArc;
    protected final int searchFocusArc;

    public DarkTextBorder() {
        focusErrorBorderColor = UIManager.getColor("TextField.border.focusError");
        focusWarningBorderColor = UIManager.getColor("TextField.border.focusWarning");
        focusBorderColor = UIManager.getColor("TextField.border.focus");
        errorBorderColor = UIManager.getColor("TextField.border.error");
        warningBorderColor = UIManager.getColor("TextField.border.warning");
        borderColor = UIManager.getColor("TextField.border.enabled");
        inactiveBorderColor = UIManager.getColor("TextField.border.disabled");
        inactiveFocusBorderColor = UIManager.getColor("TextField.border.disabled.focus");
        borderSize = UIManager.getInt("TextField.borderThickness");
        arc = UIManager.getInt("TextField.arc");
        focusArc = UIManager.getInt("TextField.focusArc");
        searchArc = UIManager.getInt("TextField.searchArc");
        searchFocusArc = UIManager.getInt("TextField.searchFocusArc");
    }

    protected static boolean hasError(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkTextUI.KEY_HAS_ERROR);
    }

    protected static boolean hasWarning(final Component c) {
        return PropertyUtil.getBooleanProperty(c, DarkTextUI.KEY_HAS_WARNING);
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

    public void paintBorder(final Component c, final Graphics g2, final int x, final int y, final int width,
            final int height) {
        boolean editable = !(c instanceof JTextComponent) || ((JTextComponent) c).isEditable();
        boolean focus = DarkUIUtil.hasFocus(c);
        boolean error = hasError(c);
        boolean warning = !error && hasWarning(c);

        Graphics2D g = (Graphics2D) g2;
        g.translate(x, y);
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        int arcSize = getArcSize(c);
        int focusArcSize = getFocusArcSize(c);
        paintFocus(g, c, width, height, editable, error, warning, focusArcSize);

        g.setColor(getBorderColor(focus, error, warning, editable, c.isEnabled()));
        PaintUtil.paintLineBorder(g, borderSize, borderSize, width - 2 * borderSize, height - 2 * borderSize, arcSize);
        g.translate(-x, -y);
        config.restore();
    }

    public void paintFocus(final Graphics2D g, final Component c, final int width, final int height,
            final boolean editable, final boolean error, final boolean warning, final int focusArcSize) {
        if (error) {
            PaintUtil.paintOutlineBorder(g, width, height, focusArcSize, borderSize, c.hasFocus(), Outline.error);
        } else if (warning) {
            PaintUtil.paintOutlineBorder(g, width, height, focusArcSize, borderSize, c.hasFocus(), Outline.warning);
        } else if (c.hasFocus()) {
            PaintUtil.paintFocusBorder(g, width, height, focusArcSize, borderSize, editable);
        }
    }

    protected Color getBorderColor(final boolean focus, final boolean error, final boolean warning,
            final boolean editable, final boolean enabled) {
        if (focus) {
            if (error) {
                return focusErrorBorderColor;
            } else if (warning) {
                return focusWarningBorderColor;
            } else {
                return enabled && editable ? focusBorderColor : inactiveFocusBorderColor;
            }
        } else {
            if (error) {
                return errorBorderColor;
            } else if (warning) {
                return warningBorderColor;
            } else {
                return enabled && editable ? borderColor : inactiveBorderColor;
            }
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        return new InsetsUIResource(borderSize, borderSize, borderSize, borderSize);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    @Override
    public Insets getVisualPaddings(final Component component) {
        return getBorderInsets(component);
    }
}
