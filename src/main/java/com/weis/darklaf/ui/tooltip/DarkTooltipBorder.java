package com.weis.darklaf.ui.tooltip;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.border.BubbleBorder;
import com.weis.darklaf.components.border.DropShadowBorder;
import com.weis.darklaf.components.tooltip.ToolTipStyle;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

/**
 * @author Jannis Weis
 */
public class DarkTooltipBorder implements Border, UIResource {

    private final DropShadowBorder shadowBorder = new DropShadowBorder(Color.BLACK,
                                                                       10, 0.2f, 10,
                                                                       false, true,
                                                                       true, true);
    private final BubbleBorder bubbleBorder;

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
        var ins = shadowBorder.getBorderInsets(null);
        adjustInsets(ins);
        return bubbleBorder.getInnerArea(ins.left, ins.top,
                                         width - ins.left - ins.right,
                                         height - ins.top - ins.bottom);
    }

    @Override
    public void paintBorder(final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        if (isPlain(c)) {
            g.setColor(bubbleBorder.getColor());
            DarkUIUtil.drawRect(g, x, y, width, height, 1);
            return;
        }
        if (c instanceof JToolTip && ((JToolTip) c).getTipText() == null) return;
        g.setColor(Color.RED);
        g.drawRect(x, y, width, height);
        var ins = shadowBorder.getBorderInsets(c);
        adjustInsets(ins);
        var bubbleArea = bubbleBorder.getInnerArea(x + ins.left, y + ins.top,
                                                   width - ins.left - ins.right,
                                                   height - ins.top - ins.bottom);
        var oldClip = g.getClip();
        var clip = new Area(new Rectangle2D.Double(x, y, width, height));
        clip.subtract(bubbleArea);
        g.setClip(clip);
        int bw = bubbleBorder.getThickness();
        int off = 0;
        var pointerSide = bubbleBorder.getPointerSide();
        if (pointerSide == Alignment.NORTH
                || pointerSide == Alignment.NORTH_EAST
                || pointerSide == Alignment.NORTH_WEST) {
            off = bubbleBorder.getPointerSize();
        }
        shadowBorder.paintBorder(c, g, x + bw, y + bw + off, width - 2 * bw, height - 2 * bw - off);
        g.setClip(oldClip);
        bubbleBorder.paintBorder(g, bubbleArea);
    }

    private void adjustInsets(final Insets si) {
        var align = bubbleBorder.getPointerSide();
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

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isPlain(c)) {
            return new Insets(1, 1, 1, 1);
        }
        var ins = new Insets(0, 0, 0, 0);
        var bi = bubbleBorder.getBorderInsets(c);
        var si = shadowBorder.getBorderInsets(c);
        ins.bottom = Math.max(bi.bottom, si.bottom);
        ins.left = Math.max(bi.left, si.left);
        ins.right = Math.max(bi.right, si.right);
        ins.top = Math.max(bi.top, si.top);
        var uIns = getUserInsets(c);
        ins.left += 5 + uIns.left;
        ins.top += 2 + uIns.top;
        ins.right += 5 + uIns.right;
        ins.bottom += 2 + uIns.bottom;
        return ins;
    }

    protected boolean isPlain(@NotNull final Component c) {
        if (!(c instanceof JComponent)) return false;
        var prop = ((JComponent) c).getClientProperty("JToolTip.style");
        return prop == ToolTipStyle.PLAIN || "plain".equals(prop);
    }

    protected Insets getUserInsets(final Component c) {
        if (c instanceof JComponent) {
            var obj = ((JComponent) c).getClientProperty("JToolTip.insets");
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

    public int getPointerOffset(final Component c, @NotNull final Dimension dimension) {
        if (isPlain(c)) return 0;
        return bubbleBorder.getOffset(dimension.width - 2 * shadowBorder.getShadowSize(), dimension.height)
                + shadowBorder.getShadowSize();
    }

    public int getShadowSize(final Component c) {
        if (isPlain(c)) return 0;
        return shadowBorder.getShadowSize();
    }
}
