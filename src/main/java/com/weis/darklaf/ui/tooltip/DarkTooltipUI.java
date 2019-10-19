package com.weis.darklaf.ui.tooltip;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicToolTipUI;
import javax.swing.text.View;
import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkTooltipUI extends BasicToolTipUI implements PropertyChangeListener, HierarchyListener {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(@NotNull final JComponent c) {
        if (Boolean.TRUE.equals(c.getClientProperty("JComponent.plainTooltip"))) {
            return BasicToolTipUI.createUI(c);
        } else {
            return new DarkTooltipUI();
        }
    }

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
            var p = e.getPoint();
            var c = toolTip.getComponent();
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

    protected boolean isInside(@NotNull final MouseEvent e) {
        var p = SwingUtilities.convertPoint(e.getComponent(), e.getPoint(), toolTip);
        return contains(toolTip, p.x, p.y);
    }

    @Override
    public boolean contains(@NotNull final JComponent c, final int x, final int y) {
        var b = c.getBorder();
        if (b instanceof DarkTooltipBorder) {
            var insideArea = ((DarkTooltipBorder) b).getBackgroundArea(toolTip,
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
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        toolTip = null;
    }

    @Override
    protected void installListeners(final JComponent c) {
        super.installListeners(c);
        c.addHierarchyListener(this);
        c.addPropertyChangeListener(this);
        toolTip.addMouseListener(exitListener);
        var comp = toolTip.getComponent();
        if (comp != null) {
            comp.addMouseListener(mouseListener);
        }
    }

    @Override
    protected void uninstallListeners(final JComponent c) {
        super.uninstallListeners(c);
        c.removePropertyChangeListener(this);
        toolTip.removeMouseListener(exitListener);
        var comp = toolTip.getComponent();
        if (comp != null) {
            comp.removeMouseListener(mouseListener);
        }
    }

    @Override
    public void paint(@NotNull final Graphics g, @NotNull final JComponent c) {
        if (((JToolTip) c).getTipText() == null) return;
        g.setColor(c.getBackground());
        if (c.getBorder() instanceof DarkTooltipBorder) {
            var area = ((DarkTooltipBorder) c.getBorder()).getBackgroundArea(c, c.getWidth(), c.getHeight());
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
    public void hierarchyChanged(final HierarchyEvent e) {
        var w = SwingUtilities.getWindowAncestor(toolTip);
        if (w != null && !toolTip.isLightweight() && !isDecorated(w)) {
            w.setBackground(DarkUIUtil.TRANSPARENT_COLOR);
        }
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        var key = evt.getPropertyName();
        if (evt.getSource() instanceof JToolTip) {
            var tooltip = (JToolTip) evt.getSource();
            if (tooltip.getBorder() instanceof DarkTooltipBorder) {
                var border = (DarkTooltipBorder) tooltip.getBorder();
                var newVal = evt.getNewValue();
                if ("JToolTip.pointerLocation".equals(key)) {
                    if (newVal instanceof Alignment) {
                        border.setPointerLocation((Alignment) newVal);
                    } else {
                        border.setPointerLocation(Alignment.CENTER);
                    }
                    tooltip.setComponent(tooltip.getComponent());
                } else if ("JToolTip.pointerHeight".equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerHeight((Integer) newVal);
                    }
                    tooltip.setComponent(tooltip.getComponent());
                } else if ("JToolTip.pointerWidth".equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerWidth((Integer) newVal);
                    }
                    tooltip.setComponent(tooltip.getComponent());
                } else if ("JToolTip.insets".equals(key)) {
                    tooltip.setComponent(tooltip.getComponent());
                } else if ("component".equals(key)) {
                    var oldComp = evt.getOldValue();
                    if (oldComp instanceof Component) {
                        ((Component) oldComp).removeMouseListener(mouseListener);
                    }
                    var newComp = evt.getNewValue();
                    if (newComp instanceof Component) {
                        ((Component) newComp).addMouseListener(mouseListener);
                    }
                }
            }
        }
    }

}
