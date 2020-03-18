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
package com.github.weisj.darklaf.ui.tooltip;

import com.github.weisj.darklaf.components.border.BubbleBorder;
import com.github.weisj.darklaf.components.border.DropShadowBorder;
import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

/**
 * @author Jannis Weis
 */
public class DarkTooltipBorder implements Border {

    private final DropShadowBorder shadowBorder = new DropShadowBorder(Color.BLACK,
                                                                       10, 0.2f, 10,
                                                                       false, true,
                                                                       true, true);
    private final BubbleBorder bubbleBorder;
    private static final AlphaComposite COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER);

    public DarkTooltipBorder() {
        bubbleBorder = new BubbleBorder(UIManager.getColor("ToolTip.borderColor"));
        bubbleBorder.setThickness(1);
        bubbleBorder.setPointerSize(8);
        bubbleBorder.setPointerWidth(12);
        bubbleBorder.setPointerSide(Alignment.CENTER);
        shadowBorder.setShadowColor(UIManager.getColor("ToolTip.borderShadowColor"));
    }


    public Area getBackgroundArea(final Component c, final int width, final int height) {
        if (isPlain(c)) {
            return new Area(new Rectangle(0, 0, width, height));
        }
        Insets ins = shadowBorder.getBorderInsets(null);
        adjustInsets(ins);
        return bubbleBorder.getInnerArea(ins.left, ins.top,
                                         width - ins.left - ins.right,
                                         height - ins.top - ins.bottom);
    }

    public int getPointerOffset(final Component c, final Dimension dimension) {
        if (isPlain(c)) return 0;
        return bubbleBorder.getOffset(dimension.width - 2 * shadowBorder.getShadowSize(), dimension.height)
                + shadowBorder.getShadowSize();
    }

    private void adjustInsets(final Insets si) {
        Alignment align = bubbleBorder.getPointerSide();
        if (align == Alignment.SOUTH || align == Alignment.SOUTH_EAST || align == Alignment.SOUTH_WEST) {
            si.bottom = 0;
        } else if (align == Alignment.EAST) {
            si.right = 0;
        } else if (align == Alignment.WEST) {
            si.left = 0;
        } else if (align == Alignment.NORTH_EAST || align == Alignment.NORTH || align == Alignment.NORTH_WEST) {
            si.top = 0;
        }
    }

    protected float getAlpha(final Component c) {
        if (c instanceof JComponent) {
            Object alpha = ((JComponent) c).getClientProperty(DarkTooltipUI.KEY_PAINT_ALPHA);
            if (alpha instanceof Float) {
                return (float) alpha;
            }
        }
        return DarkTooltipUI.MAX_ALPHA;
    }

    @Override
    public void paintBorder(final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        float alpha = getAlpha(c);
        if (alpha == 0) return;
        GraphicsContext context = new GraphicsContext(g);
        if (alpha != DarkTooltipUI.MAX_ALPHA) {
            ((Graphics2D) g).setComposite(COMPOSITE.derive(alpha));
        }
        if (isPlain(c)) {
            g.setColor(bubbleBorder.getColor());
            DarkUIUtil.drawRect(g, x, y, width, height, 1);
            return;
        }
        if (c instanceof JToolTip && ((JToolTip) c).getTipText() == null) return;
        Insets ins = shadowBorder.getBorderInsets(c);
        adjustInsets(ins);
        Area bubbleArea = bubbleBorder.getInnerArea(x + ins.left, y + ins.top,
                                                    width - ins.left - ins.right,
                                                    height - ins.top - ins.bottom);
        Shape oldClip = g.getClip();
        Area clip = new Area(new Rectangle2D.Double(x, y, width, height));
        clip.subtract(bubbleArea);
        g.setClip(clip);
        int bw = bubbleBorder.getThickness();
        int off = 0;
        Alignment pointerSide = bubbleBorder.getPointerSide();
        if (pointerSide == Alignment.NORTH
                || pointerSide == Alignment.NORTH_EAST
                || pointerSide == Alignment.NORTH_WEST) {
            off = bubbleBorder.getPointerSize();
        }
        shadowBorder.paintBorder(c, g, x + bw, y + bw + off, width - 2 * bw, height - 2 * bw - off);
        g.setClip(oldClip);
        bubbleBorder.paintBorder(g, bubbleArea);
        context.restore();
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
        ins.left += 5 + uIns.left;
        ins.top += 5 + uIns.top;
        ins.right += 5 + uIns.right;
        ins.bottom += 5 + uIns.bottom;
        return ins;
    }

    protected Insets getUserInsets(final Component c) {
        if (c instanceof JComponent) {
            Object obj = ((JComponent) c).getClientProperty(DarkTooltipUI.KEY_INSETS);
            if (obj instanceof Insets) {
                return (Insets) obj;
            }
        }
        return new Insets(0, 0, 0, 0);
    }

    @Override
    public boolean isBorderOpaque() {
        return false;
    }

    public void setPointerLocation(final Alignment side) {
        bubbleBorder.setPointerSide(side);
    }

    public void setPointerWidth(final int width) {
        bubbleBorder.setPointerWidth(width);
    }

    public void setPointerHeight(final int height) {
        bubbleBorder.setPointerSize(height);
    }

    protected boolean isPlain(final Component c) {
        if (!(c instanceof JComponent)) return false;
        Object prop = ((JComponent) c).getClientProperty(DarkTooltipUI.KEY_STYLE);
        return prop == ToolTipStyle.PLAIN || DarkTooltipUI.VARIANT_PLAIN.equals(prop);
    }

    public int getShadowSize(final Component c) {
        if (isPlain(c)) return 0;
        return shadowBorder.getShadowSize();
    }
}
