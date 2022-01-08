/*
 * MIT License
 *
 * Copyright (c) 2020-2022 Jannis Weis
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
package com.github.weisj.darklaf.ui.tooltip;

import java.awt.*;
import java.util.Objects;
import java.util.function.BiConsumer;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;
import com.github.weisj.darklaf.ui.util.WindowUtil;
import com.github.weisj.darklaf.util.Alignment;

public final class ToolTipUtil {

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

    private static Point getBestPositionMatch(final ToolTipContext context, final Point p) {
        if (!context.isBestFit()) {
            return context.getToolTipLocation(p, null);
        }
        Alignment original = context.getAlignment();
        Alignment originalCenter = context.getCenterAlignment();

        LayoutConstraints layoutConstraints = calculateLayoutConstraints(context, p);

        boolean isCenter = original == Alignment.CENTER;
        Alignment targetAlignment = isCenter ? originalCenter : original;
        if (context.isChooseBestInitialAlignment()) {
            targetAlignment = probeAlignment(context, layoutConstraints);
        }

        boolean centerVertically = targetAlignment.isHorizontal();
        boolean centerHorizontally = targetAlignment.isVertical();

        Alignment[] alignments = getAlignments(targetAlignment);
        Point pos;
        BiConsumer<ToolTipContext, Alignment> setter = isCenter
                ? ToolTipContext::setCenterAlignment
                : ToolTipContext::setAlignment;
        // Check if a position keeps the tooltip inside the window.

        pos = tryAlignments(alignments, context, p, layoutConstraints, setter, centerHorizontally, centerVertically);
        if (pos == null) {
            // Try again with screen bounds instead.
            pos = tryAlignments(alignments, context, p, layoutConstraints, setter, centerHorizontally,
                    centerVertically);
        }

        /*
         * At this point if the tooltip is still extending outside the screen boundary we surrender and
         * leave the tooltip as it was.
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

    private static Alignment probeAlignment(final ToolTipContext context, final LayoutConstraints layoutConstraints) {
        JComponent target = context.getTarget();
        if (target == null) return Alignment.SOUTH;

        Rectangle targetBounds = target.getBounds();
        Point center = new Point(targetBounds.width / 2, targetBounds.height / 2);
        center = SwingUtilities.convertPoint(target, center, layoutConstraints.window);

        Rectangle windowBounds = layoutConstraints.windowBounds;

        if (center.y < windowBounds.height / 4) return Alignment.SOUTH;
        if (center.y > 3 * windowBounds.height / 4) return Alignment.NORTH;

        if (center.x - layoutConstraints.tooltipBounds.width / 2 < 0) return Alignment.EAST;
        if (center.x + layoutConstraints.tooltipBounds.width / 2 > windowBounds.width) return Alignment.WEST;

        return Alignment.SOUTH;
    }

    private static LayoutConstraints calculateLayoutConstraints(final ToolTipContext context, final Point p) {
        Window window = DarkUIUtil.getWindow(context.getTarget());
        Rectangle screenBounds = DarkUIUtil.getScreenBounds(context.getTarget(), p);
        Rectangle windowBounds = window.getBounds();

        JToolTip toolTip = context.getToolTip();
        Rectangle tooltipBounds = new Rectangle(toolTip.getPreferredSize());

        Border tooltipBorder = toolTip.getBorder();
        Insets layoutInsets = tooltipBorder instanceof AlignableTooltipBorder
                ? ((AlignableTooltipBorder) tooltipBorder).getAlignmentInsets(toolTip)
                : new Insets(0, 0, 0, 0);

        return new LayoutConstraints(tooltipBounds, windowBounds, window, screenBounds, layoutInsets);
    }

    private static Point tryAlignments(final Alignment[] alignments, final ToolTipContext context, final Point p,
            final LayoutConstraints layoutConstraints, final BiConsumer<ToolTipContext, Alignment> setter,
            final boolean centerHorizontally, final boolean centerVertically) {
        Point pos = null;
        for (Alignment a : alignments) {
            if ((centerHorizontally || centerVertically) && a.isDiagonal()) {
                pos = tryPosition(a, context, p, layoutConstraints, setter, centerHorizontally,
                        centerVertically);
                if (pos != null) break;
            }
            pos = tryPosition(a, context, p, layoutConstraints, setter, false, false);
            if (pos != null) break;
        }
        return pos;
    }

    private static Alignment[] getAlignments(final Alignment start) {
        /*
         * Example with start == NORTH: [NORTH, NORTH_WEST, NORTH_EAST, SOUTH, SOUTH_WEST, SOUTH_EAST, WEST,
         * EAST]
         */
        Alignment opposite = start.opposite();
        Alignment clockwise = start.clockwise();
        Alignment anticlockwise = start.anticlockwise();
        return new Alignment[] {
                start, anticlockwise, clockwise,
                opposite, opposite.clockwise(), opposite.anticlockwise(),
                anticlockwise.anticlockwise(), clockwise.clockwise(),
        };
    }

    private static Point tryPosition(final Alignment a, final ToolTipContext context, final Point p,
            final LayoutConstraints layoutConstraints, final BiConsumer<ToolTipContext, Alignment> setter,
            final boolean centerHorizontally, final boolean centerVertically) {
        setter.accept(context, a);
        context.setCenterAlignment(a);
        context.updateToolTip();
        Point pos = context.getToolTipLocation(p, null, centerHorizontally, centerVertically);
        Point screenPos = new Point(pos.x, pos.y);
        SwingUtilities.convertPointToScreen(screenPos, context.getTarget());
        layoutConstraints.tooltipBounds.setLocation(screenPos);

        if (!fits(layoutConstraints)) pos = null;
        return pos;
    }

    private static boolean fits(final LayoutConstraints layoutConstraints) {
        final Rectangle testRectangle = layoutConstraints.testRectangle();

        if (Objects.equals(layoutConstraints.windowBounds, layoutConstraints.screenBoundary)) {
            return SwingUtilities.isRectangleContainingRectangle(layoutConstraints.windowBounds, testRectangle);
        }
        return SwingUtilities.isRectangleContainingRectangle(layoutConstraints.windowBounds, testRectangle)
                && SwingUtilities.isRectangleContainingRectangle(layoutConstraints.screenBoundary, testRectangle);
    }

    private static ToolTipContext getToolTipContext(final JToolTip tooltip) {
        Object context = tooltip.getClientProperty(DarkToolTipUI.KEY_CONTEXT);
        if (context instanceof ToolTipContext) {
            return (ToolTipContext) context;
        }
        context = tooltip.getComponent().getClientProperty(DarkToolTipUI.KEY_CONTEXT);
        if (context instanceof ToolTipContext) {
            return (ToolTipContext) context;
        }
        Object style = tooltip.getComponent().getClientProperty(DarkToolTipUI.KEY_STYLE);
        if (ToolTipStyle.BALLOON.equals(ToolTipStyle.parse(style))) {
            return ToolTipContext.getDefaultContext();
        }
        return null;
    }

    public static void moveToolTip(final JToolTip toolTip, final int x, final int y, final JComponent target) {
        Window window = DarkUIUtil.getWindow(toolTip);
        if (window == null) return;
        Point p = new Point(x, y);
        SwingUtilities.convertPointToScreen(p, target);
        WindowUtil.moveWindow(window, toolTip, p.x, p.y);
    }

    private static final class LayoutConstraints {

        private final Rectangle tooltipBounds;
        private final Rectangle windowBounds;
        private final Window window;
        private final Rectangle screenBoundary;
        private final Insets layoutInsets;

        private LayoutConstraints(final Rectangle tooltipBounds, final Rectangle windowBounds,
                final Window window, final Rectangle screenBoundary, Insets layoutInsets) {
            this.tooltipBounds = tooltipBounds;
            this.windowBounds = windowBounds;
            this.window = window;
            this.screenBoundary = screenBoundary;
            this.layoutInsets = layoutInsets;
        }

        public Rectangle testRectangle() {
            return DarkUIUtil.applyInsets(new Rectangle(tooltipBounds), layoutInsets);
        }
    }
}
