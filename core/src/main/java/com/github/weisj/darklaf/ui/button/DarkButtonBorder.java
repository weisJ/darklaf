/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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

import org.jetbrains.annotations.NotNull;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.util.AlignmentExt;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;
import com.github.weisj.swingdsl.visualpadding.VisualPaddingProvider;

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
        shadowSize = UIManager.getInt("Button.shadowHeight") + 20;
    }

    public static boolean showDropShadow(final Component c) {
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

    protected int getShadowSize(final Component c) {
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

        boolean focus = paintFocus(c);
        int bs = getBorderSize();

        Rectangle bgRect = getEffectiveRect(c, width, height, arcSize, corner);
        Rectangle focusRect = getFocusRect(c, width, height, arcSize, corner);

        if (paintFocus(c)) {
            paintFocusBorder(g2, focusArcSize, bs, focusRect);
        }

        paintLineBorder(c, g2, arcSize, focus, bgRect.x, bgRect.y, bgRect.width, bgRect.height);

        if (corner != null) {
            paintNeighbourFocus(g2, c, width, height);
        }
        config.restore();
    }

    public Rectangle getFocusRect(final Component c, final int width, final int height, int arcSize,
            final AlignmentExt corner) {
        Insets focusIns = new Insets(0, 0, Math.max(shadowSize - getBorderSize(), 0), 0);
        if (corner != null) {
            focusIns = corner.maskInsets(focusIns, -getBorderSize());
        }
        int fx = focusIns.left;
        int fy = focusIns.top;
        int fw = width - focusIns.left - focusIns.right;
        int fh = height - focusIns.top - focusIns.bottom;
        return new Rectangle(fx, fy, fw, fh);
    }

    public Rectangle getEffectiveRect(final Component c, final int width, final int height, int arcSize,
            final AlignmentExt corner) {
        int bs = getBorderSize();
        int shadowHeight = getShadowSize(c);
        Insets insetMask = new Insets(bs, bs, Math.max(bs, shadowHeight), bs);
        if (corner != null) {
            insetMask = corner.maskInsets(insetMask, -arcSize);
        }
        int bx = insetMask.left;
        int by = insetMask.top;
        int bw = width - insetMask.left - insetMask.right;
        int bh = height - insetMask.top - insetMask.bottom;
        return new Rectangle(bx, by, bw, bh);
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

    protected void paintFocusBorder(final Graphics2D g2, final int focusArc, final int borderSize,
            final Rectangle focusRect) {
        g2.translate(focusRect.x, focusRect.y);
        PaintUtil.paintFocusBorder(g2, focusRect.width, focusRect.height, focusArc, borderSize);
        g2.translate(-focusRect.x, -focusRect.y);
    }

    private void paintNeighbourBorder(final Graphics2D g2, final Component c, final JComponent neighbour) {
        if (neighbour == null || !DarkUIUtil.hasFocus(neighbour)) return;
        Graphics2D g = (Graphics2D) g2.create();
        g.translate(neighbour.getX() - c.getX(), neighbour.getY() - c.getY());
        AlignmentExt corner = getCornerFlag(neighbour);
        DarkButtonBorder b = (DarkButtonBorder) neighbour.getBorder();
        int fa = getFocusArc(neighbour);
        Rectangle focusRect = b.getFocusRect(neighbour, neighbour.getWidth(), neighbour.getHeight(), fa, corner);
        b.paintFocusBorder(g, fa, getBorderSize(), focusRect);
        g.dispose();
    }

    protected void paintNeighbourFocus(final Graphics2D g2, final Component c, final int width, final int height) {
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_LEFT_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_RIGHT_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_TOP_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_TOP_LEFT_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_TOP_RIGHT_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_BOTTOM_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_BOTTOM_LEFT_NEIGHBOUR, c));
        paintNeighbourBorder(g2, c, ButtonConstants.getNeighbour(ButtonConstants.KEY_BOTTOM_RIGHT_NEIGHBOUR, c));
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
        int shadow = getShadowSize(c);
        return new InsetsUIResource(borderSize, borderSize, Math.max(borderSize, shadow), borderSize);
    }

    @Override
    public @NotNull Insets getVisualPaddings(@NotNull final Component c) {
        return getBorderInsets(c);
    }

}
