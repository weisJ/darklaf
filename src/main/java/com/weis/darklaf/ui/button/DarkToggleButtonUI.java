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
package com.weis.darklaf.ui.button;

import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.GraphicsContext;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkToggleButtonUI extends DarkButtonUI {

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
    protected Dimension sliderSize;

    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return new DarkToggleButtonUI();
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        sliderSize = UIManager.getDimension("ToggleButton.sliderSize");
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

    @Override
    protected String layout(@NotNull final AbstractButton b, final JComponent c,
                            final FontMetrics fm, final int width, final int height) {
        if (isSlider(c)) {
            Insets i = b.getInsets();
            var bounds = getSliderBounds(c);
            viewRect.x = bounds.x + bounds.width + DarkButtonBorder.getBorderSize();
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

    protected Color getBackgroundColor(@NotNull final JComponent c) {
        if (c instanceof JToggleButton && c.isEnabled()) {
            if (((JToggleButton) c).isSelected()) {
                return DarkColors.get().getToggleButtonBackground();
            } else {
                return DarkColors.get().getToggleButtonInactiveBackground();
            }
        }
        return super.getBackgroundColor(c);
    }

    @NotNull
    private Rectangle getSliderBounds(@NotNull final JComponent c) {
        int x = borderSize;
        int y = (c.getHeight() - sliderSize.height) / 2;
        rect.x = x;
        rect.y = y;
        rect.width = sliderSize.width;
        rect.height = sliderSize.height;
        return rect;
    }

    public Dimension getPreferredSize(final JComponent c) {
        Dimension d = super.getPreferredSize(c);
        if (isSlider(c)) {
            d.width += sliderSize.width + DarkButtonBorder.getBorderSize();
        }
        return d;
    }

    private void paintSlider(@NotNull final Graphics2D g, final AbstractButton c) {
        var bounds = getSliderBounds(c);
        g.translate(bounds.x, bounds.y);

        if (c.hasFocus()) {
            g.translate(-borderSize, -borderSize);
            DarkUIUtil.paintFocusBorder(g, bounds.width + 2 * borderSize, bounds.height + 2 * borderSize,
                                        bounds.height, borderSize);
            g.translate(borderSize, borderSize);
        }

        g.setColor(getBackgroundColor(c));
        DarkUIUtil.paintRoundRect(g, 0, 0, bounds.width, bounds.height, bounds.height);
        g.setColor(getToggleBorderColor(c));
        DarkUIUtil.paintLineBorder(g, 0, 0, bounds.width, bounds.height, bounds.height, true);
        g.setColor(getSliderColor(c));

        int size = bounds.height - 2;
        if (c.isSelected()) {
            DarkUIUtil.paintRoundRect(g, bounds.width - size - 1, 1, size, size, size);
        } else {
            DarkUIUtil.paintRoundRect(g, 1, 1, size, size, size);
        }
        g.translate(-bounds.x, -bounds.y);
    }

    @Contract("null -> false")
    private static boolean isSlider(final JComponent c) {
        return c instanceof JToggleButton
                && "slider".equals(c.getClientProperty("ToggleButton.variant"));
    }

    @Override
    public boolean contains(@NotNull final JComponent c, final int x, final int y) {
        if (!isSlider(c)) return super.contains(c, x, y);
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        var bounds = getSliderBounds(c);
        return new RoundRectangle2D.Float(bounds.x, bounds.y, bounds.width, bounds.height,
                                          bounds.height, bounds.height).contains(x, y);
    }

    private static Color getToggleBorderColor(@NotNull final AbstractButton b) {
        if (b.hasFocus()) {
            return DarkColors.get().getToggleButtonFocusBorderColor();
        }
        return b.isEnabled() ? DarkColors.get().getToggleButtonBorderColor()
                             : DarkColors.get().getToggleButtonInactiveBorderColor();
    }

    private static Color getSliderColor(@NotNull final AbstractButton b) {
        return b.isEnabled() ? DarkColors.get().getToggleButtonSliderColor()
                             : DarkColors.get().getToggleButtonInactiveSliderColor();
    }
}
