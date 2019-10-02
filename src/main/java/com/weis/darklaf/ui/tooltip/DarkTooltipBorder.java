package com.weis.darklaf.ui.tooltip;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.border.DropShadowBorder;
import com.weis.darklaf.components.border.TextBubbleBorder;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

public class DarkTooltipBorder implements Border, UIResource {

    private final DropShadowBorder shadowBorder = new DropShadowBorder(Color.BLACK, 10, 0.4f, 10,
                                                                       false, true,
                                                                       true, true);
    private final TextBubbleBorder bubbleBorder;

    public DarkTooltipBorder() {
        bubbleBorder = new TextBubbleBorder(UIManager.getColor("Tooltip.borderColor"));
        bubbleBorder.setThickness(1);
        bubbleBorder.setPointerSize(8);
        bubbleBorder.setPointerWidth(12);
        bubbleBorder.setPointerSide(Alignment.EAST);
    }


    public Area getBackgroundArea(final int width, final int height) {
        var ins = shadowBorder.getBorderInsets(null);
        return bubbleBorder.getInnerArea(ins.left, ins.top,
                                         width - ins.left - ins.right,
                                         height - ins.top - ins.bottom);
    }

    @Override
    public void paintBorder(final Component c, final Graphics g,
                            final int x, final int y, final int width, final int height) {
        if (c instanceof JToolTip && ((JToolTip) c).getTipText() == null) return;
        var ins = shadowBorder.getBorderInsets(c);
        var bubbleArea = bubbleBorder.getInnerArea(x + ins.left, y + ins.top,
                                                   width - ins.left - ins.right,
                                                   height - ins.top - ins.bottom);
        var oldClip = g.getClip();
        var clip = new Area(new Rectangle2D.Double(x, y, width, height));
        clip.subtract(bubbleArea);
        g.setClip(clip);

        ins = bubbleBorder.getBorderInsets(c);
        shadowBorder.paintBorder(c, g, x + ins.left, y + ins.top,
                                 width - ins.left - ins.right, height - ins.top - ins.bottom);

        g.setClip(oldClip);
        bubbleBorder.paintBorder(g, bubbleArea);
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        var ins = (Insets) bubbleBorder.getBorderInsets(c).clone();
        var si = shadowBorder.getBorderInsets(c);
        var uIns = getUserInsets(c);
        ins.left += 5 + si.left + uIns.left;
        ins.top += 2 + si.top + uIns.top;
        ins.right += 5 + si.right + uIns.right;
        ins.bottom += 2 + si.bottom + uIns.bottom;
        return ins;
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
}
