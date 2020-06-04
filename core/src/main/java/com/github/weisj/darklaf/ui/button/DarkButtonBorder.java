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
package com.github.weisj.darklaf.ui.button;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public class DarkButtonBorder implements Border, UIResource {

    private final Color focusBorderColor;
    private final Color defaultBorderColor;
    private final Color borderColor;
    private final Color inactiveBorderColor;

    private final int arc;
    private final int focusArc;
    private final int squareFocusArc;
    private final int squareArc;
    private final int minimumArc;
    private final int borderSize;
    private final int shadowSize;
    private Insets insets;
    private Insets thinInsets;
    private Insets squareInsets;
    private Insets squareThinInsets;
    private Insets labelInsets;
    private Insets borderlessRectangularInsets;

    public DarkButtonBorder() {
        focusBorderColor = UIManager.getColor("Button.focusBorderColor");
        defaultBorderColor = UIManager.getColor("Button.defaultBorderColor");
        borderColor = UIManager.getColor("Button.activeBorderColor");
        inactiveBorderColor = UIManager.getColor("Button.inactiveBorderColor");
        arc = UIManager.getInt("Button.arc");
        focusArc = UIManager.getInt("Button.focusArc");
        squareFocusArc = UIManager.getInt("Button.squareFocusArc");
        squareArc = UIManager.getInt("Button.squareArc");
        minimumArc = UIManager.getInt("Button.minimumArc");
        borderSize = UIManager.getInt("Button.borderThickness");
        shadowSize = UIManager.getInt("Button.shadowHeight");
        insets = UIManager.getInsets("Button.borderInsets");
        thinInsets = UIManager.getInsets("Button.thinBorderInsets");
        squareInsets = UIManager.getInsets("Button.squareBorderInsets");
        squareThinInsets = UIManager.getInsets("Button.squareThinBorderInsets");
        labelInsets = UIManager.getInsets("Button.onlyLabelInsets");
        borderlessRectangularInsets = UIManager.getInsets("Button.borderlessRectangularInsets");
        if (insets == null) insets = new Insets(0, 0, 0, 0);
        if (thinInsets == null) thinInsets = new Insets(0, 0, 0, 0);
        if (squareThinInsets == null) squareThinInsets = new Insets(0, 0, 0, 0);
        if (squareInsets == null) squareInsets = new Insets(0, 0, 0, 0);
        if (borderlessRectangularInsets == null) borderlessRectangularInsets = new Insets(0, 0, 0, 0);
        if (labelInsets == null) labelInsets = new Insets(0, 0, 0, 0);
    }

    public static boolean showDropShadow(final JComponent c) {
        return showDropShadow(getCornerFlag(c));
    }

    public static boolean showDropShadow(final AlignmentExt a) {
        return a == null
               || a == AlignmentExt.SOUTH
               || a == AlignmentExt.SOUTH_EAST
               || a == AlignmentExt.SOUTH_WEST
               || a == AlignmentExt.LEFT
               || a == AlignmentExt.RIGHT
               || a == AlignmentExt.BOTTOM
               || a == AlignmentExt.MIDDLE_HORIZONTAL;
    }

    protected int getArc(final Component c) {
        return ButtonConstants.chooseArcWithBorder(c, arc, 0, squareArc, getBorderSize());
    }

    protected int getFocusArc(final Component c) {
        return ButtonConstants.chooseArcWithBorder(c, focusArc, minimumArc, squareFocusArc, getBorderSize());
    }

    public static AlignmentExt getCornerFlag(final Component component) {
        return PropertyUtil.getObject(component, DarkButtonUI.KEY_CORNER, AlignmentExt.class, null);
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
    public void paintBorder(final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        if (ButtonConstants.isBorderlessVariant(c) || ButtonConstants.isLabelButton(c)) {
            return;
        }
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(x, y);

        int arc = getArc(c);
        int focusArc = getFocusArc(c);
        GraphicsContext config = new GraphicsContext(g);
        AlignmentExt corner = getCornerFlag(c);

        boolean paintShadow = showDropShadow(corner);
        boolean focus = paintFocus(c);
        int shadowHeight = paintShadow ? getShadowSize() : 0;
        int borderSize = getBorderSize();

        Insets insetMask = new Insets(borderSize, borderSize, Math.max(borderSize, shadowHeight), borderSize);
        Insets focusIns = new Insets(0, 0, 0, 0);
        if (corner != null) {
            focusIns = corner.maskInsets(focusIns, -borderSize - focusArc);
            insetMask = corner.maskInsets(insetMask, -arc);
        }

        int bx = insetMask.left;
        int by = insetMask.top;
        int bw = width - insetMask.left - insetMask.right;
        int bh = height - insetMask.top - insetMask.bottom;
        int fx = focusIns.left;
        int fy = focusIns.top;
        int fw = width - focusIns.left - focusIns.right;
        int fh = by + bh + borderSize - focusIns.top - focusIns.bottom;

        if (paintFocus(c)) {
            g.translate(fx, fy);
            PaintUtil.paintFocusBorder(g2, fw, fh, focusArc, borderSize);
            g.translate(-fx, -fy);
        }

        g2.setColor(getBorderColor(c, focus));
        PaintUtil.paintLineBorder(g2, bx, by, bw, bh, arc);
        if (corner != null) {
            paintNeighbourFocus(g2, c, width, height);
        }
        config.restore();
    }

    protected void paintNeighbourFocus(final Graphics2D g2, final Component c,
                                       final int width, final int height) {
        JComponent left = ButtonConstants.getNeighbour(DarkButtonUI.KEY_LEFT_NEIGHBOUR, c);
        boolean paintLeft = DarkUIUtil.hasFocus(left);
        if (paintLeft) {
            AlignmentExt corner = getCornerFlag(left);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            int h = height - Math.max(0, getShadowSize(left) - borderSize);
            g2.translate(-3 * borderSize + 1, -ins.top);
            PaintUtil.paintFocusBorder(g2, 4 * borderSize, h + ins.top + ins.bottom,
                                       getFocusArc(left), borderSize);
            g2.translate(-(-3 * borderSize + 1), ins.bottom);
        }
        JComponent right = ButtonConstants.getNeighbour(DarkButtonUI.KEY_RIGHT_NEIGHBOUR, c);
        boolean paintRight = DarkUIUtil.hasFocus(right);
        if (paintRight) {
            AlignmentExt corner = getCornerFlag(right);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            int h = height - Math.max(0, getShadowSize(right) - borderSize);
            g2.translate(width - borderSize - 1, -ins.top);
            PaintUtil.paintFocusBorder(g2, 4 * borderSize, h + ins.top + ins.bottom,
                                       getFocusArc(right), borderSize);
            g2.translate(-(width - borderSize - 1), ins.top);
        }

        JComponent top = ButtonConstants.getNeighbour(DarkButtonUI.KEY_TOP_NEIGHBOUR, c);
        boolean paintTop = DarkUIUtil.hasFocus(top);
        if (paintTop) {
            AlignmentExt corner = getCornerFlag(top);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            g2.translate(-ins.left, -3 * borderSize + 1);
            PaintUtil.paintFocusBorder(g2, width + ins.right + ins.left,
                                       4 * borderSize, getFocusArc(top), borderSize);
            g2.translate(ins.left, -(-3 * borderSize + 1));
        }

        JComponent topLeft = ButtonConstants.getNeighbour(DarkButtonUI.KEY_TOP_LEFT_NEIGHBOUR, c);
        boolean paintTopLeft = DarkUIUtil.hasFocus(topLeft);
        if (paintTopLeft) {
            g2.translate(-3 * borderSize + 1, -3 * borderSize + 1);
            PaintUtil.paintFocusBorder(g2, 4 * borderSize, 4 * borderSize,
                                       getFocusArc(topLeft), borderSize);
            g2.translate(-(-3 * borderSize + 1), -(-3 * borderSize + 1));
        }

        JComponent topRight = ButtonConstants.getNeighbour(DarkButtonUI.KEY_TOP_RIGHT_NEIGHBOUR, c);
        boolean paintTopRight = DarkUIUtil.hasFocus(topRight);
        if (paintTopRight) {
            g2.translate(width - borderSize - 1, -3 * borderSize + 1);
            PaintUtil.paintFocusBorder(g2, 4 * borderSize, 4 * borderSize,
                                       getFocusArc(topRight), borderSize);
            g2.translate(-(width - borderSize - 1), -(-3 * borderSize + 1));
        }

        JComponent bottom = ButtonConstants.getNeighbour(DarkButtonUI.KEY_BOTTOM_NEIGHBOUR, c);
        boolean paintBottom = DarkUIUtil.hasFocus(bottom);
        if (paintBottom) {
            AlignmentExt corner = getCornerFlag(bottom);
            Insets ins = new Insets(0, 0, 0, 0);
            if (corner != null) ins = corner.maskInsets(ins, borderSize);

            g2.translate(-ins.left, height - borderSize - 1);
            PaintUtil.paintFocusBorder(g2, width + ins.left + ins.right, 4 * borderSize,
                                       getFocusArc(bottom), borderSize);
            g2.translate(ins.left, -(height - borderSize - 1));
        }

        JComponent bottomLeft = ButtonConstants.getNeighbour(DarkButtonUI.KEY_BOTTOM_LEFT_NEIGHBOUR, c);
        boolean paintBottomLeft = DarkUIUtil.hasFocus(bottomLeft);
        if (paintBottomLeft) {
            g2.translate(-3 * borderSize + 1, height - borderSize - 1);
            PaintUtil.paintFocusBorder(g2, 4 * borderSize, 4 * borderSize,
                                       getFocusArc(bottomLeft), borderSize);
            g2.translate(-(-3 * borderSize + 1), -(height - borderSize - 1));
        }

        JComponent bottomRight = ButtonConstants.getNeighbour(DarkButtonUI.KEY_BOTTOM_RIGHT_NEIGHBOUR, c);
        boolean paintBottomRight = DarkUIUtil.hasFocus(bottomRight);
        if (paintBottomRight) {
            g2.translate(width - borderSize - 1, height - borderSize - 1);
            PaintUtil.paintFocusBorder(g2, 4 * borderSize, 4 * borderSize,
                                       getFocusArc(bottomRight), borderSize);
            g2.translate(-(width - borderSize - 1), -(height - borderSize - 1));
        }
    }

    protected boolean paintFocus(final Component c) {
        if (c instanceof AbstractButton) {
            return ((AbstractButton) c).isFocusPainted() && c.hasFocus();
        }
        return c.hasFocus();
    }

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

    public Insets getBorderInsets(final Component c) {
        Insets margins = c instanceof AbstractButton ? ((AbstractButton) c).getMargin() : null;

        if (ButtonConstants.isBorderlessRectangular(c)) {
            Insets ins = new InsetsUIResource(borderlessRectangularInsets.top, borderlessRectangularInsets.left,
                                              borderlessRectangularInsets.bottom, borderlessRectangularInsets.right);
            return getInsets(ins, margins);
        }
        if (ButtonConstants.isLabelButton(c)) {
            Insets ins = new InsetsUIResource(labelInsets.top, labelInsets.left, labelInsets.bottom, labelInsets.right);
            return getInsets(ins, margins);
        }
        boolean shadowVariant = ButtonConstants.isBorderlessVariant(c);
        int shadow = shadowVariant ? 0 : getShadowSize();
        boolean square = ButtonConstants.isSquare(c);
        Insets pad = ButtonConstants.isThin(c) ? square ? squareThinInsets
                : thinInsets
                : square ? squareInsets
                : insets;
        pad = getInsets(pad, margins);
        return maskInsets(new InsetsUIResource(pad.top, pad.left, pad.bottom, pad.right), c, shadow);
    }

    protected Insets getInsets(final Insets ins, final Insets margin) {
        if (margin != null && !(margin instanceof UIResource)) {
            return margin;
        }
        return ins;
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
}
