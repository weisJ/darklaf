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
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkTooltipUI extends BasicToolTipUI implements PropertyChangeListener {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(@NotNull final JComponent c) {
        if (Boolean.TRUE.equals(c.getClientProperty("JComponent.plainTooltip"))) {
            return BasicToolTipUI.createUI(c);
        } else {
            return new DarkTooltipUI();
        }
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
    }

    @Override
    protected void installDefaults(final JComponent c) {
        super.installDefaults(c);
        c.setOpaque(false);
    }

    @Override
    protected void installListeners(final JComponent c) {
        super.installListeners(c);
        c.addHierarchyListener(e -> {
            var w = SwingUtilities.getWindowAncestor(c);
            if (w != null && !c.isLightweight() && !isDecorated(w)) {
                w.setBackground(DarkUIUtil.TRANSPARENT_COLOR);
            }
        });
        c.addPropertyChangeListener(this);
    }

    @Override
    public void paint(@NotNull final Graphics g, @NotNull final JComponent c) {
        if (((JToolTip) c).getTipText() == null) return;
        g.setColor(c.getBackground());
        if (c.getBorder() instanceof DarkTooltipBorder) {
            var area = ((DarkTooltipBorder) c.getBorder()).getBackgroundArea(c.getWidth(), c.getHeight());
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
                } else if ("JToolTip.pointerHeight".equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerHeight((Integer) newVal);
                    }
                } else if ("JToolTip.pointerWidth".equals(key)) {
                    if (newVal instanceof Integer) {
                        border.setPointerWidth((Integer) newVal);
                    }
                }
            }
        }
    }
}
