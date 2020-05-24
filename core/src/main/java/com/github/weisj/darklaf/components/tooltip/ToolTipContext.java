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
 *
 */
package com.github.weisj.darklaf.components.tooltip;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.util.function.Function;

import javax.swing.*;
import javax.swing.border.Border;

import com.github.weisj.darklaf.components.alignment.AlignmentStrategy;
import com.github.weisj.darklaf.ui.tooltip.DarkTooltipBorder;
import com.github.weisj.darklaf.ui.tooltip.DarkToolTipUI;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class ToolTipContext {

    private static ToolTipContext defaultContext;

    public static ToolTipContext getDefaultContext() {
        if (defaultContext == null) defaultContext = createDefaultContext();
        return defaultContext;
    }

    private static ToolTipContext createDefaultContext() {
        return new ToolTipContext().setAlignment(Alignment.SOUTH)
                                   .setCenterAlignment(Alignment.SOUTH)
                                   .setAlignInside(false)
                                   .setIgnoreBorder(true)
                                   .setUseBestFit(true);
    }

    public static void setDefaultContext(final ToolTipContext defaultContext) {
        ToolTipContext.defaultContext = defaultContext;
    }

    private final Insets calcInsets = new Insets(0, 0, 0, 0);
    private JComponent target;
    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseExited(final MouseEvent e) {
            checkExit(e);
        }

        @Override
        public void mouseMoved(final MouseEvent e) {
            if (hotSpotArea == null) return;
            checkExit(e);
        }

        private void checkExit(final MouseEvent e) {
            if (!hideOnExit) return;
            if (hotSpotArea != null) {
                if (!hotSpotArea.contains(e.getPoint())) {
                    ToolTipManager.sharedInstance().mousePressed(null);
                }
            } else {
                if (target != null && !target.contains(e.getPoint())) {
                    ToolTipManager.sharedInstance().mousePressed(null);
                }
            }
        }
    };
    private Alignment alignment;
    private Alignment centerAlignment;
    private boolean alignInside;
    private AlignmentStrategy alignmentStrategy;
    private Function<MouseEvent, Rectangle> toolTipRectSupplier;
    private boolean applyInsetsToRect;
    private Area hotSpotArea;
    private boolean hideOnExit;
    private JToolTip toolTip;
    private Insets insets;
    private boolean ignoreBorder;
    private boolean bestFit;
    private Function<ToolTipContext, Point> fallBackPositionProvider;

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     */
    public ToolTipContext() {
        this(null, null, null, null, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target the component which the tooltip belongs to.
     */
    public ToolTipContext(final JComponent target) {
        this(target, null, null, null, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target              the component which the tooltip belongs to.
     * @param alignment           {@link #setAlignment(Alignment)}
     * @param centerAlignment     {@link #setCenterAlignment(Alignment)}
     * @param alignmentStrategy   {@link #setAlignmentStrategy(AlignmentStrategy)}
     * @param alignInside         {@link #setAlignInside(boolean)}
     * @param toolTipRectSupplier {@link #setToolTipRectSupplier(Function)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment, final Alignment centerAlignment,
                          final AlignmentStrategy alignmentStrategy,
                          final boolean alignInside, final Function<MouseEvent, Rectangle> toolTipRectSupplier) {
        this.target = target;
        setUpdatePosition(false);
        setHideOnExit(false);
        setFallBackPositionProvider(null);
        setAlignInside(alignInside);
        setAlignment(alignment);
        setCenterAlignment(centerAlignment);
        setAlignmentStrategy(alignmentStrategy);
        setToolTipRectSupplier(toolTipRectSupplier);
    }

    /**
     * Sets whether the position should be recalculated each time after the popup has been shown. This may be useful if
     * the tooltip should follow the mouse.
     * <p>
     * Default is false.
     *
     * @param  updatePosition true if it should be recalculated.
     * @return                this
     */
    public ToolTipContext setUpdatePosition(final boolean updatePosition) {
        return this;
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target    the component which the tooltip belongs to.
     * @param alignment {@link #setAlignment(Alignment)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment) {
        this(target, alignment, null, null, true, null);
    }

    /**
     * Sets whether the component should be aligned inside or outside the supplied rectangle if the current alignment is
     * not {@link Alignment#CENTER}. Default is true
     * <p>
     *
     * @param  alignInside true if the tooltip should be aligned inside.
     * @return             this.
     * @see                #setAlignment(Alignment)
     * @see                #setToolTipRectSupplier(Function)
     */
    public ToolTipContext setAlignInside(final boolean alignInside) {
        this.alignInside = alignInside;
        return this;
    }

    public boolean isIgnoreBorder() {
        return ignoreBorder;
    }

    public boolean isBestFit() {
        return bestFit;
    }

    /**
     * Sets the alignment with respect to the supplied alignment rectangle. When using {@link Alignment#CENTER} one can
     * additionally supply an alignment using {@link #setCenterAlignment(Alignment)}. The tooltip will either be aligned
     * outside of the rectangle or inside depending on {@link #setAlignInside(boolean)}.
     *
     * @param  alignment the alignment.
     * @return           this.
     * @see              #setToolTipRectSupplier(Function)
     */
    public ToolTipContext setAlignment(final Alignment alignment) {
        this.alignment = alignment;
        if (alignment == null) {
            this.alignment = Alignment.CENTER;
        }
        return this;
    }

    /**
     * When {@link #setAlignment(Alignment)} is {@link Alignment#CENTER} this property will define on what side relative
     * to the center point the tooltip will appear.
     * <p>
     * Default is {@link Alignment#NORTH}.
     *
     * @param  centerAlignment the center alignment.
     * @return                 this
     */
    public ToolTipContext setCenterAlignment(final Alignment centerAlignment) {
        this.centerAlignment = centerAlignment;
        if (centerAlignment == null) {
            this.centerAlignment = Alignment.NORTH;
        }
        return this;
    }

    /**
     * Set the alignment strategy which determines how the current mouse position is considered when calculating the
     * tooltip location.
     * <p>
     * Default will be {@link AlignmentStrategy#COMPONENT_BOTH}.
     *
     * @param  alignmentStrategy the alignment strategy.
     * @return                   this
     * @see                      AlignmentStrategy
     */
    public ToolTipContext setAlignmentStrategy(final AlignmentStrategy alignmentStrategy) {
        this.alignmentStrategy = alignmentStrategy;
        if (alignmentStrategy == null) {
            this.alignmentStrategy = AlignmentStrategy.COMPONENT_BOTH;
        }
        return this;
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target          the component which the tooltip belongs to.
     * @param alignment       {@link #setAlignment(Alignment)}
     * @param centerAlignment {@link #setCenterAlignment(Alignment)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment, final Alignment centerAlignment) {
        this(target, alignment, centerAlignment, null, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target      the component which the tooltip belongs to.
     * @param alignment   {@link #setAlignment(Alignment)}
     * @param alignInside {@link #setAlignInside(boolean)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment, final boolean alignInside) {
        this(target, alignment, null, null, alignInside, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target          the component which the tooltip belongs to.
     * @param alignment       {@link #setAlignment(Alignment)}
     * @param centerAlignment {@link #setCenterAlignment(Alignment)}
     * @param alignInside     {@link #setAlignInside(boolean)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment, final Alignment centerAlignment,
                          final boolean alignInside) {
        this(target, alignment, centerAlignment, null, alignInside, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target            the component which the tooltip belongs to.
     * @param alignment         {@link #setAlignment(Alignment)}
     * @param alignmentStrategy {@link #setAlignmentStrategy(AlignmentStrategy)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment,
                          final AlignmentStrategy alignmentStrategy) {
        this(target, alignment, null, alignmentStrategy, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target            the component which the tooltip belongs to.
     * @param alignment         {@link #setAlignment(Alignment)}
     * @param centerAlignment   {@link #setCenterAlignment(Alignment)}
     * @param alignmentStrategy {@link #setAlignmentStrategy(AlignmentStrategy)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment, final Alignment centerAlignment,
                          final AlignmentStrategy alignmentStrategy) {
        this(target, alignment, centerAlignment, alignmentStrategy, true, null);
    }

    /**
     * Create a new tooltip context to ease the creation of custom tooltips.
     *
     * @param target            the component which the tooltip belongs to.
     * @param alignment         {@link #setAlignment(Alignment)}
     * @param alignmentStrategy {@link #setAlignmentStrategy(AlignmentStrategy)}
     * @param alignInside       {@link #setAlignInside(boolean)}
     */
    public ToolTipContext(final JComponent target, final Alignment alignment, final AlignmentStrategy alignmentStrategy,
                          final boolean alignInside) {
        this(target, alignment, null, alignmentStrategy, alignInside, null);
    }

    /**
     * Sets whether the tooltip should be closed if the mouse has left the area set by {@link #setInsideArea(Area)}.
     * <p>
     * Default is false.
     *
     * @param  hideOnExit true if tooltip should hide.
     * @return            this.
     */
    public ToolTipContext setHideOnExit(final boolean hideOnExit) {
        this.hideOnExit = hideOnExit;
        if (target != null) {
            if (hideOnExit) {
                target.addMouseListener(mouseListener);
            } else {
                target.removeMouseListener(mouseListener);
            }
        }
        return this;
    }

    /**
     * Set the supplier for the rectangle which is used to calculate the location of the tooltip. The coordinates should
     * be relative to the components origin.
     * <p>
     * Default will be the component bounding rectangle.
     *
     * @param  toolTipRectSupplier rectangle supplier method.
     * @return                     this
     */
    public ToolTipContext setToolTipRectSupplier(final Function<MouseEvent, Rectangle> toolTipRectSupplier) {
        this.toolTipRectSupplier = toolTipRectSupplier;
        if (toolTipRectSupplier == null) {
            this.toolTipRectSupplier = e -> new Rectangle(0, 0, target.getWidth(), target.getHeight());
        }
        return this;
    }

    public void updateToolTip() {
        if (toolTip != null) {
            toolTip.putClientProperty(DarkToolTipUI.KEY_POINTER_LOCATION,
                                      alignment == Alignment.CENTER
                                              ? centerAlignment.opposite()
                                              : alignInside ? alignment : alignment.opposite());
            toolTip.putClientProperty(DarkToolTipUI.KEY_INSETS, insets);
            toolTip.doLayout();
        }
    }

    /**
     * Set the area to check if the tooltip should hide. If the mouse is outside of the area the the tooltip hides if
     * {@link #setHideOnExit(boolean)} is true.
     * <p>
     * Defaults to
     *
     * @param  insideRect the area to check.
     * @return            this.
     */
    public ToolTipContext setInsideArea(final Area insideRect) {
        this.hotSpotArea = insideRect;
        return this;
    }

    /**
     * Sets whether the insets of the component insets should be subtracted from the area returned by {@link
     * #setToolTipRectSupplier(Function)}.
     * <p>
     * Default is false.
     *
     * @param  applyInsetsToRect true if they should be applied.
     * @return                   this.
     */
    public ToolTipContext setApplyComponentInsetsToRect(final boolean applyInsetsToRect) {
        this.applyInsetsToRect = applyInsetsToRect;
        return this;
    }

    /**
     * @param  insideRect the rectangle to check.
     * @return            this
     * @see               #setInsideArea(Area)
     */
    public ToolTipContext setInsideArea(final Rectangle insideRect) {
        return setInsideArea(new Area(insideRect));
    }

    /**
     * Sets insets for the tooltip.
     *
     * @param  insets the insets to set.
     * @return        this.
     */
    public ToolTipContext setToolTipInsets(final Insets insets) {
        this.insets = insets;
        return this;
    }

    public Point getToolTipLocation(final MouseEvent event) {
        Point mp = SwingUtilities.convertPoint((Component) event.getSource(), event.getPoint(), target);
        return getToolTipLocation(mp, event);
    }

    /**
     * Sets whether the border should be ignored when aligning outside. If true the tooltip is aligned w.r.t. to the
     * content rect and not the component bounds.
     *
     * @param  ignoreBorder true if border insets should be ignored.
     * @return              this.
     */
    public ToolTipContext setIgnoreBorder(final boolean ignoreBorder) {
        this.ignoreBorder = ignoreBorder;
        return this;
    }

    /**
     * Sets whether the tooltip should try its best to fit inside the window/screen.
     *
     * @param  bestFit true if best fit adjustments should be made.
     * @return         this.
     */
    public ToolTipContext setUseBestFit(final boolean bestFit) {
        this.bestFit = bestFit;
        return this;
    }

    /**
     * Calculates the tooltip location.
     *
     * @param  mp         the mouse position in the target component coordinate space.
     * @param  mouseEvent the mouse event.
     * @return            the tooltip location.
     * @see               JComponent#getToolTipLocation(MouseEvent)
     */
    public Point getToolTipLocation(final Point mp, final MouseEvent mouseEvent) {
        if (target == null) return null;
        updateToolTip();
        MouseEvent event = processEvent(mouseEvent, mp);
        Rectangle rect = getTargetRect(event);
        if (applyInsetsToRect) {
            DarkUIUtil.applyInsets(rect, target.getInsets(calcInsets));
        }
        getToolTip().setTipText(target.getToolTipText(event));
        Dimension dim = getContentSize();
        Rectangle mRect = new Rectangle(mp.x, mp.y, 1, 1);
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

        return alignmentStrategy.align(compPoint, mousePoint);
    }

    private Rectangle getTargetRect(final MouseEvent event) {
        Rectangle rect = toolTipRectSupplier.apply(event);
        if (ignoreBorder) {
            Border border = target.getBorder();
            if (border != null) {
                Insets ins = border.getBorderInsets(target);
                rect.x += ins.left;
                rect.y += ins.top;
                rect.width -= ins.left + ins.right;
                rect.height -= ins.top + ins.bottom;
            }
        }
        return rect;
    }

    private MouseEvent processEvent(final MouseEvent mouseEvent, final Point mp) {
        if (mouseEvent != null) return mouseEvent;
        return new MouseEvent(target, MouseEvent.MOUSE_MOVED, System.currentTimeMillis(), 0, mp.x, mp.y,
                              0, false, 0);
    }

    public JComponent getTarget() {
        return target;
    }

    public void setTarget(final JComponent target) {
        this.target = target;
    }

    /**
     * Get the tooltip.
     *
     * @return the tooltip.
     * @see    JComponent#createToolTip()
     */
    public JToolTip getToolTip() {
        if (toolTip == null) {
            setToolTip(new JToolTip());
        }
        return toolTip;
    }

    public void updateToolTipUI() {
        if (toolTip != null) toolTip.updateUI();
    }

    private Dimension getContentSize() {
        Dimension dim = toolTip.getPreferredSize();
        Alignment align = alignment == Alignment.CENTER ? centerAlignment : alignment;
        if (align == Alignment.EAST || align == Alignment.WEST) {
            dim.height -= ((DarkTooltipBorder) toolTip.getBorder()).getShadowSize(toolTip);
        }
        return dim;
    }

    private Point alignCenter(final Dimension dim, final Rectangle rect) {
        rect.x += rect.width / 2;
        rect.y += rect.height / 2;
        rect.width = 1;
        rect.height = 1;
        Point p = centerAlignment.alignOutside(dim, rect);
        return adjustPoint(p, centerAlignment, dim, true);
    }

    private Point alignInside(final Dimension dim, final Rectangle rect) {
        Point p = alignment.alignInside(dim, rect);
        return adjustPoint(p, alignment, dim, false);
    }

    private Point alignOutside(final Dimension dim, final Rectangle rect) {
        Point p = alignment.alignOutside(dim, rect);
        return adjustPoint(p, alignment, dim, true);
    }

    private Point adjustPoint(final Point p, final Alignment align, final Dimension dim, final boolean outside) {
        int factor = outside ? 1 : -1;
        DarkTooltipBorder border = ((DarkTooltipBorder) toolTip.getBorder());
        if (align == Alignment.EAST) {
            p.x -= factor * border.getDistanceToPointer();
        } else if (align == Alignment.WEST) {
            p.x += factor * border.getDistanceToPointer();
        } else if (align.isNorth()) {
            p.y += factor * border.getDistanceToPointer();
        } else if (align.isSouth()) {
            p.y -= factor * border.getDistanceToPointer();
        }
        if (align == Alignment.NORTH_EAST || align == Alignment.SOUTH_EAST) {
            p.x -= factor * border.getPointerOffset(toolTip, dim, factor);
        } else if (align == Alignment.NORTH_WEST || align == Alignment.SOUTH_WEST) {
            p.x += factor * border.getPointerOffset(toolTip, dim, factor);
        }
        return p;
    }

    public void removeToolTip() {
        setHideOnExit(false);
    }

    public Alignment getAlignment() {
        return alignment;
    }

    public Alignment getCenterAlignment() {
        return centerAlignment;
    }

    public AlignmentStrategy getAlignmentStrategy() {
        return alignmentStrategy;
    }

    public boolean isAlignInside() {
        return alignInside;
    }

    public void setToolTip(final JToolTip toolTip) {
        if (toolTip == null) return;
        this.toolTip = toolTip;
        if (this.target != toolTip.getComponent()) {
            this.toolTip.setComponent(this.target);
        }
    }

    public Point getFallBackPosition() {
        return fallBackPositionProvider.apply(this);
    }

    public ToolTipContext setFallBackPositionProvider(final Function<ToolTipContext, Point> fallBackPositionProvider) {
        this.fallBackPositionProvider = fallBackPositionProvider;
        if (fallBackPositionProvider == null) this.fallBackPositionProvider = c -> null;
        return this;
    }
}
