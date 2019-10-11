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
package com.weis.darklaf.components.tooltip;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.alignment.AlignmentStrategy;
import com.weis.darklaf.ui.tooltip.DarkTooltipBorder;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;

public class ToolTipContext implements ToolTipListener {

    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseMoved(final MouseEvent e) {
            checkExit(e);
        }

        @Override
        public void mouseExited(final MouseEvent e) {
            checkExit(e);
        }

        private void checkExit(final MouseEvent e) {
            if (!hideOnExit) return;
            if (hotSpotArea != null) {
                if (!hotSpotArea.contains(e.getPoint())) {
                    ToolTipManager.sharedInstance().mousePressed(null);
                }
            } else {
                if (!c.contains(e.getPoint())) {
                    ToolTipManager.sharedInstance().mousePressed(null);
                }
            }
        }
    };
    private final JComponent c;
    private DarkToolTip toolTip;
    private Alignment alignment;
    private Alignment centerAlignment;
    private boolean alignInside;
    private boolean updatePosition;
    private AlignmentStrategy alignmentStrategy;
    private Function<MouseEvent, Rectangle> toolTipRectSupplier;
    private Point lastPos;
    private Rectangle lastRect;
    private boolean valid;
    private Area hotSpotArea;
    private boolean hideOnExit;
    private Insets insets;

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c the component which the tooltip belongs to.
     */
    @Contract("null -> fail")
    public ToolTipContext(final JComponent c) {
        this(c, null, null, null, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c         the component which the tooltip belongs to.
     * @param alignment {@link #setAlignment(Alignment)}
     */
    @Contract("null, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment) {
        this(c, alignment, null, null, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c               the component which the tooltip belongs to.
     * @param alignment       {@link #setAlignment(Alignment)}
     * @param centerAlignment {@link #setCenterAlignment(Alignment)}
     */
    @Contract("null, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final Alignment centerAlignment) {
        this(c, alignment, centerAlignment, null, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c           the component which the tooltip belongs to.
     * @param alignment   {@link #setAlignment(Alignment)}
     * @param alignInside {@link #setAlignInside(boolean)}
     */
    @Contract("null, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final boolean alignInside) {
        this(c, alignment, null, null, alignInside, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c               the component which the tooltip belongs to.
     * @param alignment       {@link #setAlignment(Alignment)}
     * @param centerAlignment {@link #setCenterAlignment(Alignment)}
     * @param alignInside     {@link #setAlignInside(boolean)}
     */
    @Contract("null, _, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final Alignment centerAlignment,
                          final boolean alignInside) {
        this(c, alignment, centerAlignment, null, alignInside, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c                 the component which the tooltip belongs to.
     * @param alignment         {@link #setAlignment(Alignment)}
     * @param alignmentStrategy {@link #setAlignmentStrategy(AlignmentStrategy)}
     */
    @Contract("null, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final AlignmentStrategy alignmentStrategy) {
        this(c, alignment, null, alignmentStrategy, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c                 the component which the tooltip belongs to.
     * @param alignment         {@link #setAlignment(Alignment)}
     * @param centerAlignment   {@link #setCenterAlignment(Alignment)}
     * @param alignmentStrategy {@link #setAlignmentStrategy(AlignmentStrategy)}
     */
    @Contract("null, _, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final Alignment centerAlignment,
                          final AlignmentStrategy alignmentStrategy) {
        this(c, alignment, centerAlignment, alignmentStrategy, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c                 the component which the tooltip belongs to.
     * @param alignment         {@link #setAlignment(Alignment)}
     * @param alignmentStrategy {@link #setAlignmentStrategy(AlignmentStrategy)}
     * @param alignInside       {@link #setAlignInside(boolean)}
     */
    @Contract("null, _, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final AlignmentStrategy alignmentStrategy,
                          final boolean alignInside) {
        this(c, alignment, null, alignmentStrategy, alignInside, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param c                   the component which the tooltip belongs to.
     * @param alignment           {@link #setAlignment(Alignment)}
     * @param centerAlignment     {@link #setCenterAlignment(Alignment)}
     * @param alignmentStrategy   {@link #setAlignmentStrategy(AlignmentStrategy)}
     * @param alignInside         {@link #setAlignInside(boolean)}
     * @param toolTipRectSupplier {@link #setToolTipRectSupplier(Function)}
     */
    @Contract("null, _, _, _, _, _ -> fail")
    public ToolTipContext(final JComponent c, final Alignment alignment, final Alignment centerAlignment,
                          final AlignmentStrategy alignmentStrategy,
                          final boolean alignInside, final Function<MouseEvent, Rectangle> toolTipRectSupplier) {
        if (c == null) {
            throw new IllegalArgumentException("Component is null");
        }
        this.c = c;
        valid = false;
        setUpdatePosition(false);
        setHideOnExit(false);
        setAlignInside(alignInside);
        setAlignment(alignment);
        setCenterAlignment(centerAlignment);
        setAlignmentStrategy(alignmentStrategy);
        setToolTipRectSupplier(toolTipRectSupplier);
    }

    /**
     * Sets the alignment with respect to the supplied alignment rectangle.
     * {@see {@link #setToolTipRectSupplier(Function)}}.
     * When using {@link Alignment#CENTER} one can additionally supply an alignment using
     * {@link #setCenterAlignment(Alignment)}.
     * The tooltip will either be aligned outside of the rectangle or inside depending on
     * {@link #setAlignInside(boolean)}.
     *
     * @param alignment the alignment.
     * @return this.
     */
    public ToolTipContext setAlignment(final Alignment alignment) {
        this.alignment = alignment;
        if (alignment == null) {
            this.alignment = Alignment.CENTER;
        }
        updateToolTip();
        return this;
    }

    /**
     * When {@link #setAlignment(Alignment)} is {@link Alignment#CENTER} this property will
     * define on what side relative to the center point the tooltip will appear.
     * <p>
     * Default is {@link Alignment#NORTH}.
     *
     * @param centerAlignment the center alignment.
     * @return this
     */
    public ToolTipContext setCenterAlignment(final Alignment centerAlignment) {
        this.centerAlignment = centerAlignment;
        if (centerAlignment == null) {
            this.centerAlignment = Alignment.NORTH;
        }
        updateToolTip();
        return this;
    }

    /**
     * Sets whether the component should be aligned inside or outside the supplied rectangle if
     * the curent alignment is not {@link Alignment#CENTER}.
     * <p>
     * {@see {@link #setAlignment(Alignment)}, {@link #setToolTipRectSupplier(Supplier)}}
     * Default is true.
     *
     * @param alignInside true if the tooltip should be aligned inside.
     * @return this.
     */
    public ToolTipContext setAlignInside(final boolean alignInside) {
        this.alignInside = alignInside;
        return this;
    }


    /**
     * Set the alignment strategy which determines how the current mouse position is considered
     * when calculating the tooltip location.
     * {@see {@link AlignmentStrategy}}
     * <p>
     * Default will be {@link AlignmentStrategy#COMPONENT_BOTH}.
     *
     * @param alignmentStrategy the alignment strategy.
     * @return this
     */
    public ToolTipContext setAlignmentStrategy(final AlignmentStrategy alignmentStrategy) {
        this.alignmentStrategy = alignmentStrategy;
        if (alignmentStrategy == null) {
            this.alignmentStrategy = AlignmentStrategy.COMPONENT_BOTH;
        }
        return this;
    }

    /**
     * Set the supplier for the rectangle which is used to calculate the location of the tooltip.
     * The coordinates should be relative to the components origin.
     * <p>
     * Default will be the component bounding rectangle.
     *
     * @param toolTipRectSupplier rectangle supplier method.
     * @return this
     */
    public ToolTipContext setToolTipRectSupplier(final Function<MouseEvent, Rectangle> toolTipRectSupplier) {
        this.toolTipRectSupplier = toolTipRectSupplier;
        if (toolTipRectSupplier == null) {
            this.toolTipRectSupplier = e -> new Rectangle(0, 0, c.getWidth(), c.getHeight());
        }
        return this;
    }

    /**
     * Sets whether the position should be recalculated each time after the popup has been shown.
     * This may be useful if the tooltip should follow the mouse.
     * <p>
     * Default is false.
     *
     * @param updatePosition true if it should be recalculated.
     * @return this
     */
    public ToolTipContext setUpdatePosition(final boolean updatePosition) {
        this.updatePosition = updatePosition;
        return this;
    }

    /**
     * Set the area to check if the tooltip should hide.
     * If the mouse is outside of the area the the tooltip hides if {@link #setHideOnExit(boolean)} is true.
     * <p>
     * Defaults to
     *
     * @param insideRect the area to check.
     * @return this.
     */
    public ToolTipContext setInsideArea(final Area insideRect) {
        this.hotSpotArea = insideRect;
        return this;
    }

    /**
     * {@see {@link #setInsideArea(Area)}}.
     *
     * @param insideRect the rectangle to check.
     * @return this
     */
    public ToolTipContext setInsideArea(final Rectangle insideRect) {
        this.hotSpotArea = new Area(insideRect);
        return this;
    }

    /**
     * Sets whether the tooltip should be closed if the mouse has left the area set by
     * {@link #setInsideArea(Area).}
     * <p>
     * Default is false.
     *
     * @param hideOnExit true if tooltip should hide.
     * @return this.
     */
    public ToolTipContext setHideOnExit(final boolean hideOnExit) {
        this.hideOnExit = hideOnExit;
        if (hideOnExit) {
            c.addMouseListener(mouseListener);
        } else {
            c.removeMouseListener(mouseListener);
        }
        return this;
    }

    /**
     * Sets insets for the tooltip.
     *
     * @param insets the insets to set.
     * @return this.
     */
    public ToolTipContext setInsets(final Insets insets) {
        this.insets = insets;
        updateToolTip();
        return this;
    }

    private void updateToolTip() {
        if (toolTip != null) {
            toolTip.setAlignment(alignment == Alignment.CENTER
                                 ? centerAlignment.opposite()
                                 : alignInside ? alignment : alignment.opposite());
            toolTip.setInsets(insets);
        }
    }

    /**
     * Calculates the tooltip location.
     *
     * @param event the mouse event.
     * @return the tooltip location.
     * {@see {@link JComponent#getToolTipLocation(MouseEvent)}}
     */
    public Point getToolTipLocation(@NotNull final MouseEvent event) {
        var rect = toolTipRectSupplier.apply(event);
        if (valid && !updatePosition
                && lastPos != null
                && !Objects.equals(rect, lastRect)) {
            return lastPos;
        }
        getToolTip().setTipText(c.getToolTipText(event));
        var dim = getContentSize();
        var mp = SwingUtilities.convertPoint((Component) event.getSource(), event.getPoint(), c);
        var mRect = new Rectangle(mp.x, mp.y, 1, 1);
        Point compPoint;
        Point mousePoint;
        if (alignment == Alignment.CENTER) {
            compPoint = alignCenter(dim, rect);
            mousePoint = alignCenter(dim, mRect);
        } else {
            compPoint = alignInside ? alignInside(dim, rect)
                                    : alignOutside(dim, rect);
            mousePoint = alignInside ? alignInside(dim, mRect)
                                     : alignOutside(dim, mRect);
        }

        lastPos = alignmentStrategy.align(compPoint, mousePoint);
        lastRect = rect;
        valid = true;
        return lastPos;
    }

    private Dimension getContentSize() {
        var dim = toolTip.getPreferredSize();
        var align = alignment == Alignment.CENTER ? centerAlignment : alignment;
        if (align == Alignment.EAST || align == Alignment.WEST) {
            dim.height -= ((DarkTooltipBorder) toolTip.getBorder()).getShadowSize();
        }
        return dim;
    }

    /**
     * Get the tooltip.
     *
     * @return the tooltip.
     * {@see {@link JComponent#createToolTip()}}
     */
    public JToolTip getToolTip() {
        if (toolTip == null) {
            toolTip = new DarkToolTip(alignment);
            toolTip.addToolTipListener(this);
            toolTip.setComponent(this.c);
            updateToolTip();
        }
        return toolTip;
    }

    public void removeToolTip() {
        setHideOnExit(false);
    }

    @Nullable
    private Point alignCenter(final Dimension dim, @NotNull final Rectangle rect) {
        rect.x += rect.width / 2;
        rect.y += rect.height / 2;
        rect.width = 1;
        rect.height = 1;
        Point p = centerAlignment.alignOutside(dim, rect);
        adjustPoint(p, centerAlignment, dim, true);
        return p;
    }

    @Contract("_, _, _, _ -> param1")
    private Point adjustPoint(final Point p, final Alignment align, final Dimension dim, final boolean outside) {
        int factor = outside ? 1 : -1;
        if (align == Alignment.NORTH_EAST || align == Alignment.SOUTH_EAST) {
            p.x -= factor * ((DarkTooltipBorder) toolTip.getBorder()).getPointerOffset(dim);
        } else if (align == Alignment.NORTH_WEST || align == Alignment.SOUTH_WEST) {
            p.x += factor * ((DarkTooltipBorder) toolTip.getBorder()).getPointerOffset(dim);
        }
        return p;
    }

    private Point alignInside(final Dimension dim, final Rectangle rect) {
        Point p = alignment.alignInside(dim, rect);
        return adjustPoint(p, alignment, dim, false);
    }

    private Point alignOutside(final Dimension dim, final Rectangle rect) {
        Point p = alignment.alignOutside(dim, rect);
        return adjustPoint(p, alignment, dim, true);
    }

    @Override
    public void toolTipShown(final JToolTip toolTip) {
    }

    @Override
    public void toolTipHidden(final JToolTip toolTip) {
        if (toolTip == this.toolTip) {
            valid = false;
        }
    }

    @Override
    public void textChanged(final JToolTip toolTip) {
        if (toolTip == this.toolTip) {
            valid = false;
        }
    }
}
