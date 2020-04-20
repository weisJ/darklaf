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
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkButtonBorder implements Border, UIResource {

    private final Color shadowColor;
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
        shadowColor = UIManager.getColor("Button.shadow");
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
        if (ButtonConstants.isNoArc(c)) return 0;
        boolean square = ButtonConstants.isSquare(c);
        boolean alt = ButtonConstants.chooseAlternativeArc(c);
        return square ? alt ? arc : squareArc : alt ? squareArc : arc;
    }

    protected int getFocusArc(final Component c) {
        if (ButtonConstants.isNoArc(c)) return minimumArc;
        boolean square = ButtonConstants.isSquare(c);
        boolean alt = ButtonConstants.chooseAlternativeArc(c);
        return square ? alt ? focusArc : squareFocusArc : alt ? squareFocusArc : focusArc;
    }

    public static AlignmentExt getCornerFlag(final Component component) {
        if (component instanceof JComponent) {
            Object align = ((JComponent) component).getClientProperty(DarkButtonUI.KEY_CORNER);
            return align instanceof AlignmentExt ? (AlignmentExt) align : null;
        }
        return null;
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
        int fh = (by + bh + borderSize - fy) - focusIns.top - focusIns.bottom;

        if (c.isEnabled() && paintShadow && DarkUIUtil.getShadowComposite().getAlpha() != 0) {
            paintShadow((Graphics2D) g, bx, by, bw, bh, arc);
        }

        if (paintFocus(c)) {
            g.translate(fx, fy);
            DarkUIUtil.paintFocusBorder(g2, fw, fh, focusArc, borderSize);
            g.translate(-fx, -fy);
        }

        g2.setColor(getBorderColor(c, focus));
        DarkUIUtil.paintLineBorder(g2, bx, by, bw, bh, arc);
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
            int h = height - Math.max(0, getShadowSize(left) - borderSize);
            int arc = getFocusArc(left);
            g2.translate(-2 * borderSize, 0);
            DarkUIUtil.paintLineBorder(g2, -borderSize, borderSize, 3 * borderSize, h, arc);
            DarkUIUtil.paintFocusBorder(g2, 3 * borderSize, h, arc, borderSize);
            g2.translate(2 * borderSize, 0);
        }
        JComponent right = ButtonConstants.getNeighbour(DarkButtonUI.KEY_RIGHT_NEIGHBOUR, c);
        boolean paintRight = DarkUIUtil.hasFocus(right);
        g2.setColor(focusBorderColor);
        if (paintRight) {
            int h = height - Math.max(0, getShadowSize(right) - borderSize);
            g2.translate(width - borderSize, 0);
            DarkUIUtil.paintFocusBorder(g2, 3 * borderSize, h, getFocusArc(right), borderSize);
            g2.translate(borderSize - width, 0);
        }
        JComponent top = ButtonConstants.getNeighbour(DarkButtonUI.KEY_TOP_NEIGHBOUR, c);
        boolean paintTop = DarkUIUtil.hasFocus(top);
        if (paintTop) {
            g2.translate(0, -2 * borderSize);
            DarkUIUtil.paintFocusBorder(g2, width, 3 * borderSize,
                                        getFocusArc(top), borderSize);
            g2.translate(0, 2 * borderSize);
        }
        JComponent bottom = ButtonConstants.getNeighbour(DarkButtonUI.KEY_TOP_NEIGHBOUR, c);
        boolean paintBottom = DarkUIUtil.hasFocus(bottom);
        if (paintBottom) {
            g2.translate(0, height - borderSize);
            DarkUIUtil.paintFocusBorder(g2, width, 3 * borderSize,
                                        getFocusArc(bottom), borderSize);
            g2.translate(0, borderSize - height);
        }
    }

    protected boolean paintFocus(final Component c) {
        if (c instanceof AbstractButton) {
            return ((AbstractButton) c).isFocusPainted() && c.hasFocus();
        }
        return c.hasFocus();
    }

    private void paintShadow(final Graphics2D g2, final int x, final int y,
                             final int width, final int height, final int arc) {
        GraphicsContext context = new GraphicsContext(g2);
        int shadowSize = getShadowSize();
        Area shadowShape = new Area(new RoundRectangle2D.Double(x, y, width, height + shadowSize, arc, arc));
        Area innerArea = new Area(new RoundRectangle2D.Double(x, y, width, height, arc, arc));
        shadowShape.subtract(innerArea);
        g2.setComposite(DarkUIUtil.getShadowComposite());
        g2.setColor(shadowColor);
        g2.fill(shadowShape);
        context.restore();
    }

    public boolean isBorderOpaque() {
        return false;
    }

    protected Color getBorderColor(final Component c, final boolean focus) {
        if (focus) {
            return focusBorderColor;
        } else if (c instanceof JButton && ((JButton) c).isDefaultButton() && c.isEnabled()) {
            return defaultBorderColor;
        } else if (c.isEnabled()) {
            return borderColor;
        } else {
            return inactiveBorderColor;
        }
    }

    public Insets getBorderInsets(final Component c) {
        if (ButtonConstants.isBorderlessRectangular(c)) {
            return new InsetsUIResource(borderlessRectangularInsets.top, borderlessRectangularInsets.left,
                                        borderlessRectangularInsets.bottom, borderlessRectangularInsets.right);
        }
        if (ButtonConstants.isLabelButton(c)) {
            return new InsetsUIResource(labelInsets.top, labelInsets.left, labelInsets.bottom, labelInsets.right);
        }
        boolean shadowVariant = ButtonConstants.isBorderlessVariant(c);
        int shadow = shadowVariant ? 0 : getShadowSize();
        boolean square = ButtonConstants.isSquare(c);
        Insets pad = ButtonConstants.isThin(c) ? square ? squareThinInsets
                : thinInsets
                : square ? squareInsets
                : insets;
        return maskInsets(new InsetsUIResource(pad.top, pad.left, pad.bottom, pad.right), c, shadow);
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
