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
import com.github.weisj.darklaf.graphics.Animator;
import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.util.*;

/**
 * @author Jannis Weis
 */
public class DarkTooltipUI extends BasicToolTipUI implements PropertyChangeListener, HierarchyListener,
                           ToolTipConstants {

    protected static final float MAX_ALPHA = 1.0f;
    protected Animator fadeAnimator;
    protected float alpha = 0;

    protected JToolTip toolTip;
    protected ToolTipStyle style;
    protected boolean isTipTextChanging;
    protected final MouseListener exitListener = new MouseAdapter() {
        @Override
        public void mouseExited(final MouseEvent e) {
            boolean inside = isInside(e);
            if (!inside) {
                ToolTipManager.sharedInstance().mouseExited(new MouseEvent(toolTip.getComponent(), e.getID(),
                                                                           e.getWhen(), e.getModifiersEx(),
                                                                           Integer.MIN_VALUE, Integer.MIN_VALUE,
                                                                           e.getClickCount(), e.isPopupTrigger(),
                                                                           e.getButton()));
            }
        }
    };
    protected final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseEntered(final MouseEvent e) {
            if (e.getButton() == MouseEvent.BUTTON1) return;
            /*
             * We redispatch the event to the ToolTipManager with a corrected location.
             * Because the ToolTipManager check for outside using >= width/height instead of > width/height and due to
             * the nature of mouseEntered events most of the times having width/height as their coordinated ToolTips
             * would
             * not show up when the component is entered through the bottom/right side of the component.
             */
            Point p = e.getPoint();
            Component c = toolTip.getComponent();
            if (p.x == c.getWidth()) p.x--;
            if (p.y == c.getHeight()) p.y--;
            p.x = Math.max(p.x, 0);
            p.y = Math.max(p.y, 0);
            ToolTipManager.sharedInstance()
                          .mouseEntered(new MouseEvent(c, e.getID(), e.getWhen(), e.getModifiersEx(), p.x, p.y,
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
    private boolean added;

    public static ComponentUI createUI(final JComponent c) {
        if (Boolean.TRUE.equals(c.getClientProperty(KEY_PLAIN_TOOLTIP))) {
            return BasicToolTipUI.createUI(c);
        } else {
            return new DarkTooltipUI();
        }
    }

    @Override
    public void installUI(final JComponent c) {
        toolTip = (JToolTip) c;
        super.installUI(c);
    }

    @Override
    protected void installDefaults(final JComponent c) {
        super.installDefaults(c);
        toolTip.putClientProperty(DarkPopupFactory.KEY_NO_DECORATION, true);
        toolTip.putClientProperty(DarkPopupFactory.KEY_START_HIDDEN, true);
        toolTip.putClientProperty(DarkPopupFactory.KEY_FORCE_HEAVYWEIGHT, true);
        fadeAnimator = new FadeInAnimator();
        c.setOpaque(false);
        DarkTooltipBorder border = new DarkTooltipBorder();
        Alignment align = (Alignment) c.getClientProperty(KEY_POINTER_LOCATION);
        border.setPointerLocation(align == null ? Alignment.CENTER : align);
        toolTip.setBorder(border);
        updateStyle();
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
        if (((JToolTip) c).getTipText() == null) return;
        if (added) {
            added = false;
            alpha = 0;
            fadeAnimator.reset();
            fadeAnimator.resume();
        }
        GraphicsContext config = new GraphicsContext(g);
        g.setColor(c.getBackground());
        if (c.getBorder() instanceof DarkTooltipBorder) {
            Area area = ((DarkTooltipBorder) c.getBorder()).getBackgroundArea(c, c.getWidth(), c.getHeight(), true);
            ((Graphics2D) g).fill(area);
        }
        super.paint(g, c);
        config.restore();
    }

    public Dimension getPreferredSize(final JComponent c) {
        Font font = c.getFont();
        FontMetrics fm = c.getFontMetrics(font);
        Insets insets = c.getInsets();
        Dimension prefSize = new Dimension(insets.left + insets.right,
                                           insets.top + insets.bottom);
        String text = ((JToolTip) c).getTipText();
        if ((text != null) && !text.equals("")) {
            View v = (View) c.getClientProperty(PropertyKey.HTML);
            if (v != null) {
                prefSize.width += (int) v.getPreferredSpan(View.X_AXIS) + 6;
                prefSize.height += (int) v.getPreferredSpan(View.Y_AXIS);
            } else {
                prefSize.width += fm.stringWidth(text) + 6;
                prefSize.height += fm.getHeight();
            }
        }
        return prefSize;
    }

    @Override
    public void hierarchyChanged(final HierarchyEvent e) {
        Window w = SwingUtilities.getWindowAncestor(toolTip);
        if (toolTip.getParent() instanceof JComponent) {
            // For MediumWeightPopup still need to make parent non opaque.
            ((JComponent) toolTip.getParent()).setOpaque(false);
        }
    }

    protected boolean isInside(final MouseEvent e) {
        Point screenPos = e.getPoint();
        SwingUtilities.convertPointToScreen(screenPos, e.getComponent());
        Point toolTipPoint = new Point(screenPos.x, screenPos.y);
        Point compPoint = new Point(screenPos.x, screenPos.y);
        SwingUtilities.convertPointFromScreen(toolTipPoint, toolTip);
        SwingUtilities.convertPointFromScreen(compPoint, toolTip.getComponent());
        return toolTip.getComponent().contains(compPoint)
               || contains(toolTip, toolTipPoint.x, toolTipPoint.y);
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        Border b = c.getBorder();
        if (b instanceof DarkTooltipBorder) {
            Area insideArea = ((DarkTooltipBorder) b)
                                                     .getBackgroundArea(toolTip, toolTip.getWidth(),
                                                                        toolTip.getHeight(), false);
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
            if (tooltip.getBorder() instanceof DarkTooltipBorder) {
                DarkTooltipBorder border = (DarkTooltipBorder) tooltip.getBorder();
                Object newVal = evt.getNewValue();
                if (KEY_POINTER_LOCATION.equals(key)) {
                    if (newVal instanceof Alignment) {
                        border.setPointerLocation((Alignment) newVal);
                    } else {
                        border.setPointerLocation(Alignment.CENTER);
                    }
                    updateSize();
                } else if (KEY_POINTER_HEIGHT.equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerHeight((Integer) newVal);
                    }
                    updateSize();
                } else if (KEY_POINTER_WIDTH.equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerWidth((Integer) newVal);
                    }
                    updateSize();
                } else if (KEY_INSETS.equals(key)) {
                    updateSize();
                } else if (PropertyKey.COMPONENT.equals(key)) {
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
                    if (!isTipTextChanging) {
                        isTipTextChanging = true;
                        String tipText = tooltip.getTipText();
                        if (tipText != null && !tipText.startsWith("<html>")) {
                            if (tipText.contains("\n")) {
                                tooltip.setTipText("<html>" + tipText.replaceAll("\n", "<\\br>") + "</html>");
                            } else {
                                tooltip.setTipText("<html><body><nobr>" + tipText + "</nobr></body></html>");
                            }
                        } else {
                            tooltip.setTipText(tipText);
                        }
                        updateSize();
                        isTipTextChanging = false;
                    }
                } else if (PropertyKey.ANCESTOR.equals(key)) {
                    if (evt.getOldValue() == null) {
                        // Added to hierarchy. Schedule animation start.
                        added = true;
                        ToolTipUtil.applyContext(toolTip);
                    }
                    if (evt.getNewValue() == null) {
                        alpha = 0;
                    }
                }
            }
            if (KEY_STYLE.equals(key)) {
                updateSize();
            }
        }
    }

    private void updateStyle() {
        JComponent comp = toolTip.getComponent();
        if (comp != null) {
            ToolTipStyle style = getStyle(comp.getClientProperty(KEY_STYLE));
            ToolTipStyle tooltipStyle = getStyle(toolTip.getClientProperty(KEY_STYLE));
            toolTip.putClientProperty(KEY_STYLE, style != null ? style : tooltipStyle != null ? tooltipStyle
                    : ToolTipStyle.PLAIN_BALLOON);
        }
    }

    public static ToolTipStyle getStyle(final Object style) {
        if (style instanceof ToolTipStyle) return (ToolTipStyle) style;
        if (style == null) return null;
        String name = style.toString();
        if (VARIANT_PLAIN_BALLOON.equals(name)) return ToolTipStyle.PLAIN_BALLOON;
        if (VARIANT_BALLOON.equals(name)) return ToolTipStyle.BALLOON;
        if (VARIANT_PLAIN.equals(name)) return ToolTipStyle.PLAIN;
        return null;
    }

    protected void updateSize() {
        toolTip.doLayout();
    }

    protected class FadeInAnimator extends Animator {
        private static final int DELAY_FRAMES = 6;
        private static final int FADEIN_FRAMES_COUNT = DELAY_FRAMES + 10;

        public FadeInAnimator() {
            super("Tooltip fadein", FADEIN_FRAMES_COUNT,
                  FADEIN_FRAMES_COUNT * 15, false);
        }

        @Override
        public void paintNow(final int frame, final int totalFrames, final int cycle) {
            alpha = ((float) frame * MAX_ALPHA) / totalFrames;
            Window window = SwingUtilities.getWindowAncestor(toolTip);
            if (window != null) window.setOpacity(alpha);
            Border border = toolTip.getBorder();
            if (border instanceof DarkTooltipBorder) {
                ((DarkTooltipBorder) border).setSkipShadow(false);
            }
        }

        @Override
        protected void paintCycleEnd() {
            alpha = MAX_ALPHA;
            Window window = SwingUtilities.getWindowAncestor(toolTip);
            if (window != null) {
                window.setOpacity(alpha);
                Border border = toolTip.getBorder();
                if (window.getFocusableWindowState() && border instanceof DarkTooltipBorder) {
                    ((DarkTooltipBorder) border).setSkipShadow(true);
                }
            }
        }
    }
}
