package com.weis.darklaf.ui.button;

import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.geom.Ellipse2D;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeListener;

public class DarkToggleButtonUI extends DarkButtonUI {

    private static final int SLIDER_HEIGHT = 17;
    private static final int SLIDER_WIDTH = 35;
    private static final Rectangle rect = new Rectangle();

    private final PropertyChangeListener propertyChangeListener = evt -> {
        if ("ToggleButton.variant".equals(evt.getPropertyName())) {
            var oldVal = evt.getOldValue();
            var newVal = evt.getNewValue();
            if (oldVal != null && oldVal.equals(newVal)) {
                return;
            }
            if ("slider".equals(newVal)) {
                button.setBorderPainted(false);
            } else {
                button.setBorderPainted(true);
            }
        }
    };

    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return new DarkToggleButtonUI();
    }

    @Override
    protected void installListeners(final AbstractButton b) {
        super.installListeners(b);
        button.addPropertyChangeListener(propertyChangeListener);
    }

    @Override
    protected void uninstallListeners(final AbstractButton b) {
        super.uninstallListeners(b);
        button.removePropertyChangeListener(propertyChangeListener);
    }

    public Dimension getPreferredSize(final JComponent c) {
        Dimension d = super.getPreferredSize(c);
        d.width += SLIDER_WIDTH + DarkButtonBorder.BORDER_SIZE;
        return d;
    }

    @Override
    public void paint(final Graphics g, @NotNull final JComponent c) {
        if (isSlider(c)) {
            GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
            AbstractButton b = (AbstractButton) c;
            String text = layout(b, c, SwingUtilities2.getFontMetrics(b, g),
                                 b.getWidth(), b.getHeight());

            paintSlider((Graphics2D) g, b);
            paintIcon(g, b, c);
            paintText(g, b, c, text);
            config.restore();
        } else {
            super.paint(g, c);
        }
    }

    @Override
    protected String layout(@NotNull final AbstractButton b, final JComponent c,
                            final FontMetrics fm, final int width, final int height) {
        if (isSlider(c)) {
            Insets i = b.getInsets();
            var bounds = getSliderBounds(c);
            viewRect.x = bounds.x + bounds.width + DarkButtonBorder.BORDER_SIZE;
            viewRect.y = i.top;
            viewRect.width = width - (i.right + viewRect.x);
            viewRect.height = height - (i.bottom + viewRect.y);

            textRect.x = textRect.y = textRect.width = textRect.height = 0;
            iconRect.x = iconRect.y = iconRect.width = iconRect.height = 0;

            // layout the text and icon
            return SwingUtilities.layoutCompoundLabel(
                    b, fm, b.getText(), b.getIcon(),
                    b.getVerticalAlignment(), b.getHorizontalAlignment(),
                    b.getVerticalTextPosition(), b.getHorizontalTextPosition(),
                    viewRect, iconRect, textRect,
                    b.getText() == null ? 0 : b.getIconTextGap());
        } else {
            return super.layout(b, c, fm, width, height);
        }
    }

    protected Color getBackgroundColor(@NotNull final JComponent c) {
        if ((c instanceof JToggleButton && ((JToggleButton) c).isSelected())) {
            return UIManager.getColor("Button.darcula.activeFillColor");
        } else {
            return UIManager.getColor("Button.darcula.inactiveFillColor");
        }
    }

    @Override
    public boolean contains(@NotNull final JComponent c, final int x, final int y) {
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        var bounds = getSliderBounds(c);
        return new RoundRectangle2D.Float(bounds.x, bounds.y, bounds.width, bounds.height,
                                          bounds.height, bounds.height).contains(x, y);
    }

    @NotNull
    private Rectangle getSliderBounds(@NotNull final JComponent c) {
        int x = DarkButtonBorder.BORDER_SIZE;
        int y = (c.getHeight() - SLIDER_HEIGHT) / 2;
        rect.x = x;
        rect.y = y;
        rect.width = SLIDER_WIDTH;
        rect.height = SLIDER_HEIGHT;
        return rect;
    }

    @Contract("null -> false")
    private static boolean isSlider(final JComponent c) {
        return c instanceof JToggleButton
                && "slider".equals(c.getClientProperty("ToggleButton.variant"));
    }

    private void paintSlider(@NotNull final Graphics2D g, final AbstractButton c) {
        var bounds = getSliderBounds(c);
        g.translate(bounds.x, bounds.y);
        Shape slider = new RoundRectangle2D.Float(0, 0, bounds.width, bounds.height,
                                                  bounds.height, bounds.height);
        g.setColor(getBackgroundColor(c));
        g.fill(slider);
        g.setColor(getToggleBorderColor(c));
        g.draw(slider);
        if (c.hasFocus()) {
            var config = new GraphicsContext(g);
            g.setComposite(DarkUIUtil.ALPHA_COMPOSITE);
            g.translate(-2, -2);
            DarkUIUtil.paintFocusBorder(g, bounds.width + 4, bounds.height + 4,
                                        (float) (bounds.height / 2.0) + 2, true);
            g.translate(2, 2);
            config.restore();
        }
        g.setColor(getSliderColor(c));
        if (c.isSelected()) {
            g.fill(new Ellipse2D.Float(
                    bounds.width - bounds.height + 1, 1, bounds.height - 1.5f, bounds.height - 1.5f));
        } else {
            g.fill(new Ellipse2D.Float(1, 1, bounds.height - 1.5f, bounds.height - 1.5f));
        }
        g.translate(-bounds.x, -bounds.y);
    }

    private static Color getToggleBorderColor(@NotNull final AbstractButton b) {
        return b.isEnabled() ? UIManager.getColor("ToggleButton.sliderBorderColor")
                             : UIManager.getColor("ToggleButton.disabledSliderBorderColor");
    }

    private static Color getSliderColor(@NotNull final AbstractButton b) {
        return b.isEnabled() ? UIManager.getColor("ToggleButton.sliderColor")
                             : UIManager.getColor("ToggleButton.disabledSliderColor");
    }
}
