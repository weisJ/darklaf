/*
 * MIT License
 *
 * Copyright (c) 2019-2022 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.button;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;
import org.jetbrains.annotations.NotNull;

/** @author Jannis Weis */
public class DarkButtonBorder implements Border, UIResource, VisualPaddingProvider {

    private final Color focusBorderColor;
    private final Color defaultBorderColor;
    private final Color borderColor;
    private final Color inactiveBorderColor;

    private final int arc;
    private final int focusArc;
    private final int squareFocusArc;
    private final int altArc;
    private final int minimumArc;
    private final int borderSize;
    private final int shadowSize;

    public DarkButtonBorder() {
        focusBorderColor = UIManager.getColor("Button.focusBorderColor");
        defaultBorderColor = UIManager.getColor("Button.defaultBorderColor");
        borderColor = UIManager.getColor("Button.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("Button.inactiveBorderColor");
        arc = UIManager.getInt("Button.arc");
        focusArc = UIManager.getInt("Button.focusArc");
        squareFocusArc = UIManager.getInt("Button.squareFocusArc");
        altArc = UIManager.getInt("Button.altArc");
        minimumArc = UIManager.getInt("Button.minimumArc");
        borderSize = UIManager.getInt("Button.borderThickness");
        shadowSize = UIManager.getInt("Button.shadowHeight");
    }

    public static boolean showDropShadow(final JComponent c) {
        return showDropShadow(getCornerFlag(c)) && !ButtonConstants.isBorderlessVariant(c);
    }

    public static boolean showDropShadow(final AlignmentExt a) {
        return a == null || a == AlignmentExt.SOUTH || a == AlignmentExt.SOUTH_EAST || a == AlignmentExt.SOUTH_WEST
                || a == AlignmentExt.LEFT || a == AlignmentExt.RIGHT || a == AlignmentExt.BOTTOM
                || a == AlignmentExt.MIDDLE_HORIZONTAL;
    }

    protected int getArc(final Component c) {
        return ButtonConstants.chooseArcWithBorder(c, arc, 0, altArc, getBorderSize());
    }

    protected int getFocusArc(final Component c) {
        return ButtonConstants.chooseArcWithBorder(c, focusArc, minimumArc, squareFocusArc, getBorderSize());
    }

    public static AlignmentExt getCornerFlag(final Component component) {
        return PropertyUtil.getObject(component, ButtonConstants.KEY_CORNER, AlignmentExt.class, null);
    }

    protected int getShadowSize(final JComponent c) {
        return showDropShadow(c) ? getShadowSize() : 0;
    }

    protected int getShadowSize() {
        return shadowSize;
    }

    protected int getBorderSize() {
        return borderSize;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        if (ButtonConstants.isBorderlessVariant(c)) {
            paintBorderlessBorder(c, g, x, y, width, height);
            return;
        }
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(x, y);

        int arcSize = getArc(c);
        int focusArcSize = getFocusArc(c);
        GraphicsContext config = new GraphicsContext(g);
        AlignmentExt corner = getCornerFlag(c);

        boolean paintShadow = showDropShadow(corner);
        boolean focus = paintFocus(c);
        int shadowHeight = paintShadow ? getShadowSize() : 0;
        int bs = getBorderSize();

        Insets insetMask = new Insets(bs, bs, Math.max(bs, shadowHeight), bs);
        Insets focusIns = new Insets(0, 0, 0, 0);
        if (corner != null) {
            focusIns = corner.maskInsets(focusIns, -bs - focusArcSize);
            insetMask = corner.maskInsets(insetMask, -arcSize);
        }

        int bx = insetMask.left;
        int by = insetMask.top;
        int bw = width - insetMask.left - insetMask.right;
        int bh = height - insetMask.top - insetMask.bottom;
        int fx = focusIns.left;
        int fy = focusIns.top;
        int fw = width - focusIns.left - focusIns.right;
        int fh = by + bh + bs - focusIns.top - focusIns.bottom;

        if (paintFocus(c)) {
            paintFocusBorder(g2, focusArcSize, bs, fx, fy, fw, fh);
        }

        paintLineBorder(c, g2, arcSize, focus, bx, by, bw, bh);

        if (corner != null) {
            paintNeighbourFocus(g2, c, width, height);
        }
        config.restore();
    }

    protected void paintBorderlessBorder(final Component c, final Graphics g, final int x, final int y, final int width,
            final int height) {
        if (paintFocus(c)) {
            AbstractButton b = (AbstractButton) c;
            DarkButtonUI ui = DarkUIUtil.getUIOfType(b.getUI(), DarkButtonUI.class);
            if (ui == null) return;
            Insets margin = b.getMargin();
            if (margin instanceof UIResource) {
                margin = null;
            }
            Rectangle r = ui.backgroundContentRect(b, width, height, margin);

            GraphicsContext context = GraphicsUtil.setupStrokePainting(g);
            g.translate(x, y);
            g.setColor(getBorderColor(c, true));
            PaintUtil.paintLineBorder((Graphics2D) g, r.x, r.y, r.width, r.height, getArc(c));
            context.restore();
        }
    }

    protected void paintLineBorder(final Component c, final Graphics2D g2, final int arc, final boolean focus,
            final int bx, final int by, final int bw, final int bh) {
        g2.setColor(getBorderColor(c, focus));
        PaintUtil.paintLineBorder(g2, bx, by, bw, bh, arc);
    }

    protected void paintFocusBorder(final Graphics2D g2, final int focusArc, final int borderSize, final int fx,
            final int fy, final int fw, final int fh) {
        g2.translate(fx, fy);
        PaintUtil.paintFocusBorder(g2, fw, fh, focusArc, borderSize);
        g2.translate(-fx, -fy);
    }

    protected void paintNeighbourFocus(final Graphics2D g2, final Component c, final int width, final int height) {
        JComponent left = ButtonConstants.getNeighbour(ButtonConstants.KEY_LEFT_NEIGHBOUR, c);
        boolean paintLeft = DarkUIUtil.hasFocus(left);
        if (paintLeft) {
            AlignmentExt corner = getCornerFlag(left);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            int h = height - Math.max(0, getShadowSize(left) - borderSize);
            paintFocusBorder(g2, getFocusArc(left), borderSize, -3 * borderSize + 1, -ins.top, 4 * borderSize,
                    h + ins.top + ins.bottom);
        }
        JComponent right = ButtonConstants.getNeighbour(ButtonConstants.KEY_RIGHT_NEIGHBOUR, c);
        boolean paintRight = DarkUIUtil.hasFocus(right);
        if (paintRight) {
            AlignmentExt corner = getCornerFlag(right);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            int h = height - Math.max(0, getShadowSize(right) - borderSize);
            paintFocusBorder(g2, getFocusArc(right), borderSize, width - borderSize - 1, -ins.top, 4 * borderSize,
                    h + ins.top + ins.bottom);
        }

        JComponent top = ButtonConstants.getNeighbour(ButtonConstants.KEY_TOP_NEIGHBOUR, c);
        boolean paintTop = DarkUIUtil.hasFocus(top);
        if (paintTop) {
            AlignmentExt corner = getCornerFlag(top);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            paintFocusBorder(g2, getFocusArc(top), borderSize, -ins.left, -3 * borderSize + 1,
                    width + ins.right + ins.left, 4 * borderSize);
        }

        JComponent topLeft = ButtonConstants.getNeighbour(ButtonConstants.KEY_TOP_LEFT_NEIGHBOUR, c);
        boolean paintTopLeft = DarkUIUtil.hasFocus(topLeft);
        if (paintTopLeft) {
            paintFocusBorder(g2, getFocusArc(topLeft), borderSize, -3 * borderSize + 1, -3 * borderSize + 1,
                    4 * borderSize, 4 * borderSize);
        }

        JComponent topRight = ButtonConstants.getNeighbour(ButtonConstants.KEY_TOP_RIGHT_NEIGHBOUR, c);
        boolean paintTopRight = DarkUIUtil.hasFocus(topRight);
        if (paintTopRight) {
            paintFocusBorder(g2, getFocusArc(topRight), borderSize, width - borderSize - 1, -3 * borderSize + 1,
                    4 * borderSize, 4 * borderSize);
        }

        JComponent bottom = ButtonConstants.getNeighbour(ButtonConstants.KEY_BOTTOM_NEIGHBOUR, c);
        boolean paintBottom = DarkUIUtil.hasFocus(bottom);
        if (paintBottom) {
            AlignmentExt corner = getCornerFlag(bottom);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            paintFocusBorder(g2, getFocusArc(bottom), borderSize, -ins.left, height - borderSize - 1,
                    width + ins.left + ins.right, 4 * borderSize);
        }

        JComponent bottomLeft = ButtonConstants.getNeighbour(ButtonConstants.KEY_BOTTOM_LEFT_NEIGHBOUR, c);
        boolean paintBottomLeft = DarkUIUtil.hasFocus(bottomLeft);
        if (paintBottomLeft) {
            paintFocusBorder(g2, getFocusArc(bottomLeft), borderSize, -3 * borderSize + 1, height - borderSize - 1,
                    4 * borderSize, 4 * borderSize);
        }

        JComponent bottomRight = ButtonConstants.getNeighbour(ButtonConstants.KEY_BOTTOM_RIGHT_NEIGHBOUR, c);
        boolean paintBottomRight = DarkUIUtil.hasFocus(bottomRight);
        if (paintBottomRight) {
            paintFocusBorder(g2, getFocusArc(bottomRight), borderSize, width - borderSize - 1, height - borderSize - 1,
                    4 * borderSize, 4 * borderSize);
        }
    }

    protected boolean paintFocus(final Component c) {
        if (c instanceof AbstractButton) {
            return ((AbstractButton) c).isFocusPainted() && c.hasFocus();
        }
        return c.hasFocus();
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    protected Color getBorderColor(final Component c, final boolean focus) {
        if (focus) {
            return focusBorderColor;
        } else if (c instanceof JComponent && ButtonConstants.isDefaultButton((JComponent) c) && c.isEnabled()) {
            return defaultBorderColor;
        } else if (c.isEnabled()) {
            return borderColor;
        } else {
            return inactiveBorderColor;
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (ButtonConstants.isBorderlessVariant(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        boolean shadowVariant = ButtonConstants.isBorderless(c);
        int shadow = shadowVariant ? 0 : getShadowSize();
        return maskInsets(new InsetsUIResource(borderSize, borderSize, Math.max(borderSize, shadow), borderSize), c,
                shadow);
    }

    protected Insets maskInsets(final Insets ins, final Component c, final int shadow) {
        AlignmentExt alignment = getCornerFlag(c);
        if (alignment == null) return ins;
        Insets insetMask = new Insets(borderSize, borderSize, Math.max(borderSize, shadow), borderSize);
        insetMask = alignment.maskInsetsInverted(insetMask);
        ins.top -= insetMask.top;
        ins.bottom -= insetMask.bottom;
        ins.left -= insetMask.left;
        ins.right -= insetMask.right;
        return ins;
    }

    @Override
    public @NotNull Insets getVisualPaddings(@NotNull final Component c) {
        return getBorderInsets(c);
    }
}
