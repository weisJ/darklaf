/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
import java.awt.event.*;
import java.awt.geom.Area;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicToolTipUI;
import javax.swing.text.View;

import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.graphics.*;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.uiresource.DarkColorUIResource;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

/** @author Jannis Weis */
public class DarkToolTipUI extends BasicToolTipUI
        implements PropertyChangeListener, HierarchyListener, ToolTipConstants {

    protected static final float MAX_ALPHA = 1.0f;
    protected static final float MIN_ALPHA = 0.1f;

    protected final MouseListener exitListener = new MouseAdapter() {
        @Override
        public void mouseExited(final MouseEvent e) {
            boolean inside = isInside(e);
            if (!inside) {
                ToolTipManager.sharedInstance()
                        .mouseExited(new MouseEvent(
                                toolTip.getComponent(), e.getID(), e.getWhen(),
                                e.getModifiersEx(), Integer.MIN_VALUE, Integer.MIN_VALUE,
                                e.getClickCount(), e.isPopupTrigger(), e.getButton()));
            }
        }
    };
    protected final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseEntered(final MouseEvent e) {
            if (e.getButton() == MouseEvent.BUTTON1) return;
            /*
             * We redispatch the event to the ToolTipManager with a corrected location. Because the
             * ToolTipManager check for outside using >= width/height instead of > width/height and due to the
             * nature of mouseEntered events most of the times having width/height as their coordinated ToolTips
             * would not show up when the component is entered through the bottom/right side of the component.
             */
            Point p = e.getPoint();
            Component c = toolTip.getComponent();
            if (p.x == c.getWidth()) p.x--;
            if (p.y == c.getHeight()) p.y--;
            p.x = Math.max(p.x, 0);
            p.y = Math.max(p.y, 0);
            ToolTipManager.sharedInstance()
                    .mouseEntered(new MouseEvent(
                            c, e.getID(), e.getWhen(),
                            e.getModifiersEx(), p.x, p.y,
                            e.getClickCount(), e.isPopupTrigger(), e.getButton()));
        }

        @Override
        public void mouseExited(final MouseEvent e) {
            exitListener.mouseExited(e);
        }
    };
    protected final PropertyChangeListener componentPropertyChaneListener = e -> {
        if (KEY_STYLE.equals(e.getPropertyName())) {
            updateStyle();
        }
    };

    protected Animator fadeAnimator;
    protected float alpha = 0;
    protected JToolTip toolTip;
    protected ToolTipStyle style;

    private boolean animationScheduled;
    protected String effectiveTipText;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkToolTipUI();
    }

    @Override
    public void installUI(final JComponent c) {
        toolTip = (JToolTip) c;
        super.installUI(c);
    }

    @Override
    protected void installDefaults(final JComponent c) {
        super.installDefaults(c);
        toolTip.putClientProperty(DarkPopupFactory.KEY_NO_DECORATION, !useDecoratedPopup());
        toolTip.putClientProperty(DarkPopupFactory.KEY_START_HIDDEN, true);
        toolTip.putClientProperty(DarkPopupFactory.KEY_FORCE_HEAVYWEIGHT, true);
        toolTip.setOpaque(false);
        fadeAnimator = new FadeInAnimator();
        updateTipText(toolTip);
        updateStyle();
    }

    protected boolean useDecoratedPopup() {
        return false;
    }

    @Override
    public void update(final Graphics g, final JComponent c) {
        if (style.isOpaque()) {
            g.setColor(c.getBackground());
        } else {
            // Erase background completely.
            g.setColor(PaintUtil.TRANSPARENT_COLOR);
        }
        g.fillRect(0, 0, c.getWidth(), c.getHeight());
        paint(g, c);
    }

    @Override
    protected void installListeners(final JComponent c) {
        super.installListeners(c);
        c.addHierarchyListener(this);
        c.addPropertyChangeListener(this);
        c.addMouseListener(exitListener);
        Component comp = toolTip.getComponent();
        if (comp != null) {
            comp.addMouseListener(mouseListener);
        }
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        fadeAnimator.dispose();
        toolTip = null;
    }

    @Override
    protected void uninstallListeners(final JComponent c) {
        super.uninstallListeners(c);
        c.removePropertyChangeListener(this);
        c.removeMouseListener(exitListener);
        c.removeHierarchyListener(this);
        Component comp = toolTip.getComponent();
        if (comp != null) {
            comp.removeMouseListener(mouseListener);
        }
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        if (getTipText().isEmpty()) {
            return;
        }
        boolean isPlain = style == ToolTipStyle.PLAIN;
        if (animationScheduled) {
            startAnimation();
        } else {
            Window window = DarkUIUtil.getWindow(c);
            if (window != null) {
                if (window.getOpacity() != 1 && !transparencySupported(window)) {
                    window.setOpacity(1);
                }
            }
        }
        GraphicsContext context = GraphicsUtil.setupAntialiasing(g);
        g.setColor(c.getBackground());
        if (!isPlain && c.getBorder() instanceof DarkTooltipBorder) {
            Area area = ((DarkTooltipBorder) c.getBorder()).getBackgroundArea(c, c.getWidth(), c.getHeight());
            ((Graphics2D) g).fill(area);
        } else {
            PaintUtil.fillRect(g, 0, 0, c.getWidth(), c.getHeight());
        }
        paintText(g, c);
        context.restore();
    }

    protected void startAnimation() {
        animationScheduled = false;
        fadeAnimator.reset();
        alpha = 0;
        fadeAnimator.resume();
    }

    protected void paintText(final Graphics g, final JComponent c) {
        Dimension size = c.getSize();

        g.setColor(c.getForeground());
        String tipText = getTipText();

        // noinspection StringEquality
        if (tipText != NO_TEXT) {
            Insets insets = c.getInsets();
            Rectangle paintTextR = new Rectangle(insets.left, insets.top, size.width - (insets.left + insets.right),
                    size.height - (insets.top + insets.bottom));
            StringPainter.drawString(g, c, tipText, paintTextR);
        }
    }

    protected String getTipText() {
        if (effectiveTipText == null) {
            updateTipText(toolTip);
        }
        if (effectiveTipText == null) {
            effectiveTipText = "";
        }
        return effectiveTipText;
    }

    public Dimension getPreferredSize(final JComponent c) {
        Font font = c.getFont();
        FontMetrics fm = c.getFontMetrics(font);
        Insets insets = c.getInsets();
        Dimension prefSize = new Dimension(insets.left + insets.right, insets.top + insets.bottom);
        String text = getTipText();
        if ((text != null) && !text.equals("")) {
            View v = (View) c.getClientProperty(PropertyKey.HTML);
            if (v != null) {
                prefSize.width += (int) v.getPreferredSpan(View.X_AXIS);
                prefSize.height += (int) v.getPreferredSpan(View.Y_AXIS);
            } else {
                prefSize.width += fm.stringWidth(text);
                prefSize.height += fm.getHeight();
            }
        }
        return prefSize;
    }

    @Override
    public void hierarchyChanged(final HierarchyEvent e) {
        // For MediumWeightPopup still need to make parent non opaque.
        Component component = toolTip.getParent();
        if (component instanceof JComponent && style != ToolTipStyle.PLAIN) {
            ((JComponent) component).setOpaque(false);
            component.setBackground(new DarkColorUIResource(ColorUtil.toAlpha(component.getBackground(), 0)));
        }
    }

    protected boolean isInside(final MouseEvent e) {
        if (toolTip.getComponent() == null) return true;
        Point screenPos = e.getPoint();
        SwingUtilities.convertPointToScreen(screenPos, e.getComponent());
        Point toolTipPoint = new Point(screenPos.x, screenPos.y);
        Point compPoint = new Point(screenPos.x, screenPos.y);
        SwingUtilities.convertPointFromScreen(toolTipPoint, toolTip);
        SwingUtilities.convertPointFromScreen(compPoint, toolTip.getComponent());
        return toolTip.getComponent().contains(compPoint) || contains(toolTip, toolTipPoint.x, toolTipPoint.y);
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        Border b = c.getBorder();
        if (b instanceof DarkTooltipBorder) {
            Area insideArea =
                    ((DarkTooltipBorder) b).getBackgroundArea(toolTip, toolTip.getWidth(), toolTip.getHeight());
            return insideArea.contains(x, y);
        } else {
            return super.contains(c, x, y);
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (evt.getSource() instanceof JToolTip) {
            JToolTip tooltip = (JToolTip) evt.getSource();
            if (tooltip.getBorder() instanceof AlignableTooltipBorder) {
                AlignableTooltipBorder b = (AlignableTooltipBorder) tooltip.getBorder();
                Object newVal = evt.getNewValue();
                if (KEY_POINTER_LOCATION.equals(key)) {
                    if (newVal instanceof Alignment) {
                        b.setPointerLocation((Alignment) newVal, pointerEnabled());
                    } else {
                        b.setPointerLocation(Alignment.CENTER, pointerEnabled());
                    }
                    updateSize();
                }

                if (b instanceof DarkTooltipBorder) {
                    DarkTooltipBorder border = (DarkTooltipBorder) tooltip.getBorder();
                    if (pointerEnabled() && KEY_POINTER_HEIGHT.equals(key)) {
                        if (newVal instanceof Integer) {
                            border.setPointerHeight((Integer) newVal);
                        }
                        updateSize();
                    } else if (pointerEnabled() && KEY_POINTER_WIDTH.equals(key)) {
                        if (newVal instanceof Integer) {
                            border.setPointerWidth((Integer) newVal);
                        }
                        updateSize();
                    } else if (KEY_INSETS.equals(key)) {
                        updateSize();
                    }
                }
            }
            if (PropertyKey.COMPONENT.equals(key)) {
                Object oldComp = evt.getOldValue();
                if (oldComp instanceof Component) {
                    ((Component) oldComp).removeMouseListener(mouseListener);
                    ((Component) oldComp).removePropertyChangeListener(componentPropertyChaneListener);
                }
                Object newComp = evt.getNewValue();
                if (newComp instanceof Component) {
                    ((Component) newComp).addMouseListener(mouseListener);
                    ((Component) newComp).addPropertyChangeListener(componentPropertyChaneListener);
                }
                updateStyle();
            } else if (TIP_TEXT_PROPERTY.equals(key)) {
                updateTipText(tooltip);
                updateSize();
            } else if (PropertyKey.ANCESTOR.equals(key)) {
                if (evt.getOldValue() == null) {
                    // Added to hierarchy. Schedule animation start.
                    scheduleAnimation();
                    ToolTipUtil.applyContext(toolTip);
                }
                if (evt.getNewValue() == null) {
                    alpha = 0;
                }
            } else if (KEY_STYLE.equals(key)) {
                updateStyle();
            }
        }
    }

    protected boolean pointerEnabled() {
        return style.supportsPointer();
    }

    protected void updateTipText(final JToolTip tooltip) {
        effectiveTipText = tooltip.getTipText();
        if (effectiveTipText == null) effectiveTipText = "";
    }

    protected void scheduleAnimation() {
        Window window = DarkUIUtil.getWindow(toolTip);
        animationScheduled = transparencySupported(DarkUIUtil.getWindow(toolTip));
        if (window != null && animationScheduled) window.setOpacity(MIN_ALPHA);
    }

    protected boolean transparencySupported(final Window window) {
        return style != ToolTipStyle.PLAIN && GraphicsUtil.supportsTransparency(window);
    }

    protected void updateStyle() {
        JComponent comp = toolTip.getComponent();
        ToolTipStyle style = comp != null ? ToolTipStyle.parse(comp.getClientProperty(KEY_STYLE)) : null;
        ToolTipStyle tooltipStyle = ToolTipStyle.parse(toolTip.getClientProperty(KEY_STYLE));
        if (style == null) style = tooltipStyle;
        if (style == null) style = ToolTipStyle.parse(UIManager.get("ToolTip.defaultStyle"));
        if (style.isOpaque() && !GraphicsUtil.supportsTransparency()) style = ToolTipStyle.PLAIN;
        if (style != tooltipStyle) {
            toolTip.putClientProperty(KEY_STYLE, style);
        }
        if (style != this.style) {
            this.style = style;
            updateBorder(style);
            /*
             * In non opaque windows on Windows subpixel AA doesn't work. The translucent background isn't
             * needed for the plain tooltip style, so we set it here.
             *
             * See https://bugs.openjdk.java.net/browse/JDK-8215980?attachmentOrder=desc
             */
            toolTip.putClientProperty(DarkPopupFactory.KEY_OPAQUE, style.isOpaque());
        }
    }

    protected void updateBorder(final ToolTipStyle style) {
        if (style == ToolTipStyle.PLAIN) {
            toolTip.setBorder(new DarkDefaultToolTipBorder());
        } else {
            DarkTooltipBorder border = new DarkTooltipBorder();
            border.setPointerLocation(Alignment.CENTER, pointerEnabled());
            toolTip.setBorder(border);
        }
    }

    protected void updateSize() {
        toolTip.doLayout();
    }

    protected class FadeInAnimator extends Animator {
        private static final int FADEIN_FRAMES_COUNT = 15;

        public FadeInAnimator() {
            super(FADEIN_FRAMES_COUNT, FADEIN_FRAMES_COUNT * 15, false);
        }

        @Override
        public boolean isEnabled() {
            return toolTip.getComponent() != null && super.isEnabled();
        }

        @Override
        public void paintNow(final float fraction) {
            alpha = MIN_ALPHA + fraction * (MAX_ALPHA - MIN_ALPHA);
            Window window = SwingUtilities.getWindowAncestor(toolTip);
            if (DarkUIUtil.isDecorated(window)) return;
            if (window != null) window.setOpacity(alpha);
        }

        @Override
        protected void paintCycleEnd() {
            alpha = MAX_ALPHA;
            Window window = SwingUtilities.getWindowAncestor(toolTip);
            if (window != null && !DarkUIUtil.isDecorated(window)) {
                window.setOpacity(alpha);
            }
        }
    }
}
