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
        if (bubbleBorder.getColor() == null) {
            bubbleBorder.setColor(UIManager.getColor("ToolTip.borderColor"));
        }
        if (isPlain(c)) {
            g.setColor(bubbleBorder.getColor());
            DarkUIUtil.drawRect(g, x, y, width, height, 1);
            return;
        }
        if (c instanceof JToolTip && ((JToolTip) c).getTipText() == null) return;
        var ins = shadowBorder.getBorderInsets(c);
        adjustInsets(ins);
        var bubbleArea = bubbleBorder.getInnerArea(x + ins.left, y + ins.top,
                                                   width - ins.left - ins.right,
                                                   height - ins.top - ins.bottom);
        var oldClip = g.getClip();
        var clip = new Area(new Rectangle2D.Double(x, y, width, height));
        clip.subtract(bubbleArea);
        g.setClip(clip);

        shadowBorder.setShadowColor(UIManager.getColor("ToolTip.borderShadowColor"));
        shadowBorder.paintBorder(c, g, x, y, width, height);

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
        }
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        if (isPlain(c)) {
            return new Insets(1, 1, 1, 1);
        }
        var ins = (Insets) bubbleBorder.getBorderInsets(c).clone();
        var si = shadowBorder.getBorderInsets(c);
        adjustInsets(si);
        var uIns = getUserInsets(c);
        ins.left += 5 + si.left + uIns.left;
        ins.top += 2 + si.top + uIns.top;
        ins.right += 5 + si.right + uIns.right;
        ins.bottom += 2 + si.bottom + uIns.bottom;
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
