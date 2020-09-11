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
package com.github.weisj.darklaf.ui.tooltip;

import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.components.border.BubbleBorder;
import com.github.weisj.darklaf.components.border.DropShadowBorder;
import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * @author Jannis Weis
 */
public class DarkTooltipBorder implements Border, AlignableTooltipBorder {

    private final DropShadowBorder shadowBorder;
    private final BubbleBorder bubbleBorder;
    private final boolean paintShadow;
    private boolean skipShadow;
    private Insets margin;
    private Alignment alignment;
    private boolean showPointer;

    public DarkTooltipBorder() {
        margin = UIManager.getInsets("ToolTip.borderInsets");
        if (margin == null) margin = new Insets(0, 0, 0, 0);
        bubbleBorder = new BubbleBorder(UIManager.getColor("ToolTip.borderColor"));
        bubbleBorder.setThickness(1);
        bubbleBorder.setPointerSize(8);
        bubbleBorder.setPointerWidth(12);
        bubbleBorder.setPointerSide(Alignment.CENTER);
        int shadowSize = UIManager.getInt("ToolTip.shadowSize");
        float opacity = UIManager.getInt("ToolTip.shadowOpacity") / 100.0f;
        shadowBorder = new DropShadowBorder(
            UIManager.getColor("ToolTip.borderShadowColor"), shadowSize, opacity, 2 * shadowSize, false, true, true, true
        );
        paintShadow = UIManager.getBoolean("ToolTip.paintShadow");
    }

    public Area getBackgroundArea(final Component c, final int width, final int height) {
        if (isPlain(c)) {
            return new Area(new Rectangle(0, 0, width, height));
        }
        Insets ins = shadowBorder.getBorderInsets(null);
        adjustInsets(ins);
        return bubbleBorder.getBubbleArea(
            ins.left, ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom,
            bubbleBorder.getThickness() / 2f
        );
    }

    @Override
    public int getPointerOffset(final Component c, final Dimension dimension, final int thicknessFactor) {
        if (!showPointer || isPlain(c)) return 0;
        int offset = (int) bubbleBorder.getOffset(dimension.width - 2 * shadowBorder.getShadowSize(), dimension.height)
            + shadowBorder.getShadowSize();
        int thickness = bubbleBorder.getThickness();
        Alignment align = bubbleBorder.getPointerSide();
        if (align.isWest(false)) offset += thicknessFactor * thickness;
        return offset;
    }

    @Override
    public void adjustContentSize(final JToolTip toolTip, final Dimension dim, final Alignment align) {
        if (align == Alignment.EAST || align == Alignment.WEST) {
            dim.height -= getShadowSize(toolTip);
        }
    }

    private void adjustInsets(final Insets si) {
        Alignment align = bubbleBorder.getPointerSide();
        int pointerSize = bubbleBorder.getPointerSize();
        if (align.isSouth()) {
            si.bottom -= pointerSize;
        } else if (align == Alignment.EAST) {
            si.right -= pointerSize;
        } else if (align == Alignment.WEST) {
            si.left -= pointerSize;
        } else if (align.isNorth()) {
            si.top = 0;
        }
    }

    @Override
    public void paintBorder(
            final Component c, final Graphics g, final int x, final int y, final int width, final int height
    ) {
        if (c instanceof JToolTip && ((JToolTip) c).getTipText() == null) return;
        GraphicsContext context = new GraphicsContext(g);
        if (isPlain(c)) {
            g.setColor(bubbleBorder.getColor());
            PaintUtil.drawRect(g, x, y, width, height, 1);
            return;
        }
        Insets ins = shadowBorder.getBorderInsets(c);
        adjustInsets(ins);
        Area innerArea = bubbleBorder.getBubbleArea(
            x + ins.left, y + ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom,
            bubbleBorder.getThickness()
        );
        if (!skipShadow && paintShadow) {
            paintShadow(c, g, x, y, width, height, innerArea);
        }
        Area outerArea = bubbleBorder
            .getBubbleArea(x + ins.left, y + ins.top, width - ins.left - ins.right, height - ins.top - ins.bottom, 0);
        outerArea.subtract(innerArea);
        bubbleBorder.paintBorder(g, outerArea);
        context.restore();
    }

    public void paintShadow(
            final Component c, final Graphics g, final int x, final int y, final int width, final int height,
            final Area bubbleArea
    ) {
        Shape oldClip = g.getClip();
        Area clip = new Area(new Rectangle2D.Double(x, y, width, height));
        clip.subtract(bubbleArea);
        g.setClip(clip);
        int bw = 1 + bubbleBorder.getThickness();
        shadowBorder.paintBorder(c, g, x + bw, y + bw, width - 2 * bw, height - 2 * bw);
        g.setClip(oldClip);
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        Insets uIns = getUserInsets(c);
        if (isPlain(c)) {
            return new Insets(1 + uIns.top, 1 + uIns.left, 1 + uIns.bottom, 1 + uIns.right);
        }
        Insets ins = new Insets(0, 0, 0, 0);
        Insets bi = bubbleBorder.getBorderInsets(c);
        Insets si = shadowBorder.getBorderInsets(c);
        ins.bottom = Math.max(bi.bottom, si.bottom);
        ins.left = Math.max(bi.left, si.left);
        ins.right = Math.max(bi.right, si.right);
        ins.top = Math.max(bi.top, si.top);
        ins.left += uIns.left;
        ins.top += uIns.top;
        ins.right += uIns.right;
        ins.bottom += uIns.bottom;
        return ins;
    }

    protected Insets getUserInsets(final Component c) {
        return PropertyUtil.getObject(c, DarkToolTipUI.KEY_INSETS, Insets.class, margin);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    @Override
    public void setPointerLocation(final Alignment side, final boolean showPointer) {
        if (showPointer) {
            bubbleBorder.setPointerSide(side);
        }
        this.showPointer = showPointer;
        alignment = side;
    }

    public void setPointerWidth(final int width) {
        bubbleBorder.setPointerWidth(width);
    }

    public void setPointerHeight(final int height) {
        bubbleBorder.setPointerSize(height);
    }

    protected boolean isPlain(final Component c) {
        if (!(c instanceof JComponent)) return false;
        Object prop = ((JComponent) c).getClientProperty(DarkToolTipUI.KEY_STYLE);
        return prop == ToolTipStyle.PLAIN || DarkToolTipUI.VARIANT_PLAIN.equals(prop);
    }

    public int getShadowSize(final Component c) {
        if (isPlain(c)) return 0;
        return shadowBorder.getShadowSize();
    }

    public void setSkipShadow(final boolean skip) {
        this.skipShadow = skip;
    }

    @Override
    public int getDistanceToPointer() {
        switch (alignment) {
            case WEST:
            case SOUTH:
            case SOUTH_EAST:
            case SOUTH_WEST:
            case EAST:
                return Math.max(0, shadowBorder.getShadowSize() - bubbleBorder.getPointerSize())
                    + bubbleBorder.getThickness();
            case NORTH:
            case NORTH_EAST:
            case NORTH_WEST:
            case CENTER:
            default:
                return bubbleBorder.getThickness();
        }
    }
}
