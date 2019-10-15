/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.button;

import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.defaults.DarkDefaults;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.InsetsUIResource;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.RoundRectangle2D;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkButtonBorder implements Border, UIResource {

    @Override
    public void paintBorder(final Component c, @NotNull final Graphics g,
                            final int x, final int y, final int width, final int height) {
        if (DarkButtonUI.isShadowVariant(c) || DarkButtonUI.isLabelButton(c)) {
            return;
        }
        Graphics2D g2 = (Graphics2D) g;
        g2.translate(x, y);

        int arc = getArc(c);
        int focusArc = getFocusArc(c);
        GraphicsContext config = new GraphicsContext(g);

        if (c.isEnabled()) {
            paintShadow(g2, width, height, arc);
        }

        int shadowHeight = getShadowSize();
        int borderSize = getBorderSize();


        if (c.hasFocus()) {
            DarkUIUtil.paintFocusBorder(g2, width, height - shadowHeight, focusArc, borderSize);
        }

        g2.setColor(getBorderColor(c));
        DarkUIUtil.paintLineBorder(g2, borderSize, borderSize, width - 2 * borderSize,
                                   height - 2 * borderSize - shadowHeight, arc, true);
        config.restore();
    }

    public static int getArc(final Component c) {
        if (DarkButtonUI.isNoArc(c)) return 0;
        boolean square = DarkButtonUI.isSquare(c);
        boolean alt = DarkButtonUI.chooseAlternativeArc(c);
        return square ? alt ? getArcSize()
                            : getSquareArcSize()
                      : alt ? getSquareArcSize() : getArcSize();
    }

    public static int getSquareArcSize() {
        return DarkDefaults.get().getButtonSquareArc();
    }

    public static int getFocusArc(final Component c) {
        if (DarkButtonUI.isNoArc(c)) return getMinimumArc();
        boolean square = DarkButtonUI.isSquare(c);
        boolean alt = DarkButtonUI.chooseAlternativeArc(c);
        return square ? alt ? getFocusArcSize()
                            : getSquareFocusArcSize()
                      : alt ? getSquareFocusArcSize() : getFocusArcSize();
    }

    private void paintShadow(@NotNull final Graphics2D g2, final int width, final int height, final int arc) {
        GraphicsContext context = new GraphicsContext(g2);
        int borderSize = getBorderSize();
        int shadowSize = getShadowSize();
        Area shadowShape = new Area(new RoundRectangle2D.Double(borderSize, borderSize,
                                                                width - 2 * borderSize, height - 2 * borderSize,
                                                                arc, arc));
        Area innerArea = new Area(new RoundRectangle2D.Double(borderSize, borderSize,
                                                              width - 2 * borderSize,
                                                              height - 2 * borderSize - shadowSize,
                                                              arc, arc));
        shadowShape.subtract(innerArea);
        g2.setComposite(DarkUIUtil.SHADOW_COMPOSITE);
        g2.setColor(DarkColors.get().getButtonShadowColor());
        g2.fill(shadowShape);
        context.restore();
    }

    public static int getShadowSize() {
        return DarkDefaults.get().getButtonShadowSize();
    }

    public static int getBorderSize() {
        return DarkDefaults.get().getButtonBorderSize();
    }

    private Color getBorderColor(@NotNull final Component c) {
        if (c.hasFocus()) {
            return DarkColors.get().getButtonFocusBorderColor();
        } else if (c instanceof JButton && ((JButton) c).isDefaultButton() && c.isEnabled()) {
            return DarkColors.get().getButtonDefaultBorderColor();
        } else if (c.isEnabled()) {
            return DarkColors.get().getButtonBorderColor();
        } else {
            return DarkColors.get().getButtonInactiveBorderColor();
        }
    }

    public static int getArcSize() {
        return DarkDefaults.get().getButtonArc();
    }

    public static int getMinimumArc() {
        return DarkDefaults.get().getButtonMinimumArc();
    }

    public static int getSquareFocusArcSize() {
        return DarkDefaults.get().getButtonSquareFocusArc();
    }

    public static int getFocusArcSize() {
        return DarkDefaults.get().getButtonFocusArc();
    }

    public Insets getBorderInsets(final Component c) {
        if (DarkButtonUI.isFullShadow(c) || DarkButtonUI.isLabelButton(c)) {
            return new InsetsUIResource(0, 0, 0, 0);
        }
        int shadow = DarkButtonUI.isShadowVariant(c) ? 0 : getShadowSize();
        int pad = isThin(c) ? 4 : 8;
        if (DarkButtonUI.isSquare(c)) {
            return new InsetsUIResource(pad, pad, pad + shadow, pad);
        } else {
            return new InsetsUIResource(pad, 2 * pad, pad + shadow, 2 * pad);
        }
    }

    public boolean isBorderOpaque() {
        return false;
    }

    @Contract("null -> false")
    public static boolean isThin(final Component c) {
        return c instanceof JButton
                && Boolean.TRUE.equals(((JButton) c).getClientProperty("JButton.thin"));
    }
}
