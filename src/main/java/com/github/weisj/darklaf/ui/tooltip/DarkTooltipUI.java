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
package com.github.weisj.darklaf.ui.tooltip;

import com.github.weisj.darklaf.components.alignment.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicToolTipUI;
import javax.swing.text.View;
import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.Area;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkTooltipUI extends BasicToolTipUI implements PropertyChangeListener, HierarchyListener {

    protected JToolTip toolTip;
    protected MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseEntered(@NotNull final MouseEvent e) {
            if (e.getButton() == MouseEvent.BUTTON1) return;
            /*
             * We redispatch the event to the ToolTipManager with a corrected location.
             * Because the ToolTipManager check for outside using >= width/height instead of > width/height and due to
             * the nature of mouseEntered events most of the times having width/height as their coordinated ToolTips would
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
        public void mouseExited(@NotNull final MouseEvent e) {
            boolean inside = isInside(e);
            if (!inside) {
                ToolTipManager.sharedInstance().mouseExited(
                        new MouseEvent(toolTip.getComponent(), e.getID(), e.getWhen(), e.getModifiersEx(),
                                       Integer.MIN_VALUE, Integer.MIN_VALUE, e.getClickCount(), e.isPopupTrigger(),
                                       e.getButton()));
            }
        }
    };
    protected MouseListener exitListener = new MouseAdapter() {
        @Override
        public void mouseExited(@NotNull final MouseEvent e) {
            ToolTipManager.sharedInstance().mouseExited(
                    new MouseEvent(toolTip.getComponent(), e.getID(), e.getWhen(), e.getModifiersEx(),
                                   Integer.MIN_VALUE, Integer.MIN_VALUE, e.getClickCount(), e.isPopupTrigger(),
                                   e.getButton()));
        }
    };

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(@NotNull final JComponent c) {
        if (Boolean.TRUE.equals(c.getClientProperty("JComponent.plainTooltip"))) {
            return BasicToolTipUI.createUI(c);
        } else {
            return new DarkTooltipUI();
        }
    }

    protected boolean isInside(@NotNull final MouseEvent e) {
        Point p = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), toolTip);
        return contains(toolTip, p.x, p.y);
    }

    @Override
    public boolean contains(@NotNull final JComponent c, final int x, final int y) {
        Border b = c.getBorder();
        if (b instanceof DarkTooltipBorder) {
            Area insideArea = ((DarkTooltipBorder) b).getBackgroundArea(toolTip,
                                                                        toolTip.getWidth(), toolTip.getHeight());
            return insideArea.contains(x, y);
        } else {
            return super.contains(c, x, y);
        }
    }

    @Override
    public void installUI(final JComponent c) {
        toolTip = (JToolTip) c;
        super.installUI(c);
        toolTip.setBorder(new DarkTooltipBorder());
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        toolTip = null;
    }

    @Override
    protected void installDefaults(final JComponent c) {
        super.installDefaults(c);
        c.setOpaque(false);
        if (c.getBorder() instanceof DarkTooltipBorder) {
            Alignment align = (Alignment) c.getClientProperty("JToolTip.pointerLocation");
            ((DarkTooltipBorder) c.getBorder()).setPointerLocation(align == null ? Alignment.CENTER : align);
        }
    }

    @Override
    protected void installListeners(final JComponent c) {
        super.installListeners(c);
        c.addHierarchyListener(this);
        c.addPropertyChangeListener(this);
        toolTip.addMouseListener(exitListener);
        Component comp = toolTip.getComponent();
        if (comp != null) {
            comp.addMouseListener(mouseListener);
        }
    }

    @Override
    protected void uninstallListeners(final JComponent c) {
        super.uninstallListeners(c);
        c.removePropertyChangeListener(this);
        toolTip.removeMouseListener(exitListener);
        Component comp = toolTip.getComponent();
        if (comp != null) {
            comp.removeMouseListener(mouseListener);
        }
    }

    @Override
    public void paint(@NotNull final Graphics g, @NotNull final JComponent c) {
        if (((JToolTip) c).getTipText() == null) return;
        g.setColor(c.getBackground());
        if (c.getBorder() instanceof DarkTooltipBorder) {
            Area area = ((DarkTooltipBorder) c.getBorder()).getBackgroundArea(c, c.getWidth(), c.getHeight());
            ((Graphics2D) g).fill(area);
        }
        super.paint(g, c);
    }

    public Dimension getPreferredSize(@NotNull final JComponent c) {
        Font font = c.getFont();
        FontMetrics fm = c.getFontMetrics(font);
        Insets insets = c.getInsets();

        Dimension prefSize = new Dimension(insets.left + insets.right,
                                           insets.top + insets.bottom);
        String text = ((JToolTip) c).getTipText();

        if ((text != null) && !text.equals("")) {
            View v = (View) c.getClientProperty("html");
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
        if (w != null && !toolTip.isLightweight() && !isDecorated(w)) {
            w.setBackground(DarkUIUtil.TRANSPARENT_COLOR);
        }
    }

    protected boolean isDecorated(final Window w) {
        if (w instanceof Dialog) {
            return !((Dialog) w).isUndecorated();
        }
        if (w instanceof Frame) {
            return !((Frame) w).isUndecorated();
        }
        return false;
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (evt.getSource() instanceof JToolTip) {
            JToolTip tooltip = (JToolTip) evt.getSource();
            if (tooltip.getBorder() instanceof DarkTooltipBorder) {
                DarkTooltipBorder border = (DarkTooltipBorder) tooltip.getBorder();
                Object newVal = evt.getNewValue();
                if ("JToolTip.pointerLocation".equals(key)) {
                    if (newVal instanceof Alignment) {
                        border.setPointerLocation((Alignment) newVal);
                    } else {
                        border.setPointerLocation(Alignment.CENTER);
                    }
                    updateSize();
                } else if ("JToolTip.pointerHeight".equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerHeight((Integer) newVal);
                    }
                    updateSize();
                } else if ("JToolTip.pointerWidth".equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerWidth((Integer) newVal);
                    }
                    updateSize();
                } else if ("JToolTip.insets".equals(key)) {
                    updateSize();
                } else if ("component".equals(key)) {
                    Object oldComp = evt.getOldValue();
                    if (oldComp instanceof Component) {
                        ((Component) oldComp).removeMouseListener(mouseListener);
                    }
                    Object newComp = evt.getNewValue();
                    if (newComp instanceof Component) {
                        ((Component) newComp).addMouseListener(mouseListener);
                    }
                }
            }
        }
    }

    protected void updateSize() {
        toolTip.setTipText(toolTip.getTipText());
        toolTip.setPreferredSize(getPreferredSize(toolTip));
    }
}
