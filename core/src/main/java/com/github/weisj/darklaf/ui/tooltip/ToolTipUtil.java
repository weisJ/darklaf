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

import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;

import javax.swing.*;
import java.awt.*;
import java.util.function.BiConsumer;

public class ToolTipUtil {

    public static void applyContext(final JToolTip toolTip) {
        JComponent target = toolTip.getComponent();
        if (target == null) return;

        ToolTipContext context = getToolTipContext(toolTip);
        if (context == null) return;

        context.setTarget(target);
        context.setToolTip(toolTip);

        Point p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, target);
        Point pos = getBestPositionMatch(context, p);
        if (pos != null) {
            moveToolTip(toolTip, pos.x, pos.y, target);
        }
    }

    protected static Point getBestPositionMatch(final ToolTipContext context, final Point p) {
        if (!context.isBestFit()) {
            return context.getToolTipLocation(p, null);
        }
        Rectangle screenBounds = DarkUIUtil.getScreenBounds(context.getTarget(), p);
        Rectangle windowBounds = DarkUIUtil.getWindow(context.getTarget()).getBounds();
        Rectangle tooltipBounds = new Rectangle();
        tooltipBounds.setSize(context.getToolTip().getPreferredSize());

        Alignment original = context.getAlignment();
        Alignment originalCenter = context.getCenterAlignment();
        boolean isCenter = original == Alignment.CENTER;
        Alignment[] alignments = getAlignments(isCenter ? originalCenter : original);
        Point pos = null;
        BiConsumer<ToolTipContext, Alignment> setter = isCenter ? ToolTipContext::setCenterAlignment
                                                                : ToolTipContext::setAlignment;
        // Check if a position keeps the tooltip inside the window.
        for (Alignment a : alignments) {
            pos = tryPosition(a, context, p, tooltipBounds, windowBounds, screenBounds, setter);
            if (pos != null) break;
        }
        if (pos == null) {
            //Try again with screen bounds instead.
            for (Alignment a : alignments) {
                pos = tryPosition(a, context, p, tooltipBounds, screenBounds, screenBounds, setter);
                if (pos != null) break;
            }
        }
        /*
         * At this point if the tooltip is still extending outside the screen boundary
         * we surrender and leave the tooltip as it was.
         */
        if (pos == null) {
            context.setAlignment(Alignment.CENTER);
            context.setCenterAlignment(Alignment.CENTER);
            pos = context.getFallBackPosition();
        }
        context.updateToolTip();
        context.setAlignment(original);
        context.setCenterAlignment(originalCenter);
        return pos;
    }

    protected static Alignment[] getAlignments(final Alignment start) {
        //Example with NORTH:
        return new Alignment[]{
            start, //NORTH
            start.opposite(), //SOUTH
            start.clockwise().clockwise(),//EAST
            start.anticlockwise().anticlockwise(), //WEST
            start.clockwise(), //NORTH_EAST
            start.clockwise().opposite(), //SOUTH_WEST
            start.anticlockwise(), //NORTH_WEST
            start.anticlockwise().opposite() //SOUTH_EAST
        };
    }

    protected static Point tryPosition(final Alignment a, final ToolTipContext context, final Point p,
                                       final Rectangle tooltipBounds, final Rectangle boundary,
                                       final Rectangle screenBoundary,
                                       final BiConsumer<ToolTipContext, Alignment> setter) {
        setter.accept(context, a);
        context.setCenterAlignment(a);
        context.updateToolTip();
        Point pos = context.getToolTipLocation(p, null);
        Point screenPos = new Point(pos.x, pos.y);
        SwingUtilities.convertPointToScreen(screenPos, context.getTarget());
        tooltipBounds.setLocation(screenPos);
        if (!fits(tooltipBounds, boundary, screenBoundary)) pos = null;
        return pos;
    }

    protected static boolean fits(final Rectangle toolTipBounds, final Rectangle boundary,
                                  final Rectangle screenBoundary) {
        if (boundary == screenBoundary) {
            return SwingUtilities.isRectangleContainingRectangle(boundary, toolTipBounds);
        }
        return SwingUtilities.isRectangleContainingRectangle(boundary, toolTipBounds)
               && SwingUtilities.isRectangleContainingRectangle(screenBoundary, toolTipBounds);
    }

    protected static ToolTipContext getToolTipContext(final JToolTip tooltip) {
        Object context = tooltip.getClientProperty(DarkTooltipUI.KEY_CONTEXT);
        if (context instanceof ToolTipContext) {
            return (ToolTipContext) context;
        }
        context = tooltip.getComponent().getClientProperty(DarkTooltipUI.KEY_CONTEXT);
        if (context instanceof ToolTipContext) {
            return (ToolTipContext) context;
        }
        Object style = tooltip.getComponent().getClientProperty(DarkTooltipUI.KEY_STYLE);
        if (ToolTipStyle.BALLOON.equals(DarkTooltipUI.getStyle(style))) {
            return ToolTipContext.getDefaultContext();
        }
        return null;
    }

    public static void moveToolTip(final JToolTip toolTip, final int x, final int y, final JComponent target) {
        Window window = DarkUIUtil.getWindow(toolTip);
        if (window == null) return;
        Point targetPos = target.getLocationOnScreen();
        window.setLocation(targetPos.x + x, targetPos.y + y);
    }
}
