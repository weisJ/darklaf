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
 */
package com.github.weisj.darklaf.ui.button;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.GraphicsContext;
import com.github.weisj.darklaf.util.GraphicsUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.geom.RoundRectangle2D;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkToggleButtonUI extends DarkButtonUI {

    protected static final String KEY_PREFIX = "JToggleButton.";
    public static final String KEY_VARIANT = KEY_PREFIX + "variant";
    public static final String KEY_IS_TREE_EDITOR = KEY_PREFIX + "isTreeCellEditor";
    public static final String KEY_IS_TREE_RENDER = KEY_PREFIX + "isTreeCellRenderer";
    public static final String KEY_IS_TABLE_EDITOR = KEY_PREFIX + "isTableCellEditor";
    public static final String KEY_IS_TABLE_RENDERER = KEY_PREFIX + "isTableCellRenderer";
    public static final String KEY_CLEAR_HIT_AREA = KEY_PREFIX + "clearHitArea";
    public static final String VARIANT_SLIDER = "slider";
    private static final Rectangle rect = new Rectangle();

    private final PropertyChangeListener propertyChangeListener = evt -> {
        String key = evt.getPropertyName();
        if (KEY_VARIANT.equals(key)) {
            Object oldVal = evt.getOldValue();
            Object newVal = evt.getNewValue();
            if (oldVal != null && oldVal.equals(newVal)) {
                return;
            }
            if (VARIANT_SLIDER.equals(newVal)) {
                button.setBorderPainted(false);
            } else {
                button.setBorderPainted(true);
            }
        } else if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            button.repaint();
        }
    };
    protected Dimension sliderSize;
    protected Color background;
    protected Color backgroundInactive;
    protected Color focusBorderColor;
    protected Color borderColor;
    protected Color inactiveBorderColor;
    protected Color sliderColor;
    protected Color inactiveSliderColor;
    protected Color sliderBorderColor;
    protected Color inactiveSliderBorderColor;
    protected Color selectedForeground;


    public static ComponentUI createUI(final JComponent c) {
        return new DarkToggleButtonUI();
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        sliderSize = UIManager.getDimension("ToggleButton.sliderSize");
        background = UIManager.getColor("ToggleButton.activeFillColor");
        backgroundInactive = UIManager.getColor("ToggleButton.inactiveFillColor");
        focusBorderColor = UIManager.getColor("ToggleButton.focusedSliderBorderColor");
        borderColor = UIManager.getColor("ToggleButton.sliderBorderColor");
        inactiveBorderColor = UIManager.getColor("ToggleButton.disabledSliderBorderColor");
        sliderColor = UIManager.getColor("ToggleButton.sliderKnobFillColor");
        inactiveSliderColor = UIManager.getColor("ToggleButton.disabledSliderKnobFillColor");
        sliderBorderColor = UIManager.getColor("ToggleButton.sliderKnobBorderColor");
        inactiveSliderBorderColor = UIManager.getColor("ToggleButton.disabledSliderKnobBorderColor");
        selectedForeground = UIManager.getColor("ToggleButton.selectedForeground");
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
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
    protected Color getForeground(final AbstractButton button) {
        if (button.isSelected() && !isSlider(button) && button.getForeground() instanceof UIResource) {
            return selectedForeground;
        }
        return super.getForeground(button);
    }

    protected Color getBackgroundColor(final JComponent c) {
        AbstractButton b = (AbstractButton) c;
        boolean rollOver = (b.isRolloverEnabled() || doConvertToShadow(b)) && (((JButton) c).getModel().isRollover());
        boolean clicked = b.getModel().isArmed();
        boolean isSelected = c instanceof JToggleButton && ((JToggleButton) c).isSelected();
        if (c.isEnabled()) {
            if (isSelected) return background;
            if (clicked) {
                return clickBackground;
            } else if (rollOver) {
                return hoverBackground;
            } else {
                if (c instanceof JToggleButton) {
                    return backgroundInactive;
                } else {
                    return super.getBackgroundColor(c);
                }
            }
        } else {
            return inactiveBackground;
        }
    }

    @Override
    protected String layout(final AbstractButton b, final JComponent c,
                            final FontMetrics fm, final int width, final int height) {
        if (isSlider(c)) {
            Insets i = b.getInsets();
            Rectangle bounds = getSliderBounds(c);
            viewRect.x = bounds.width + 2 * borderSize;
            viewRect.width = width - (i.right + viewRect.x);
            viewRect.y = i.top;
            viewRect.height = height - (i.bottom + viewRect.y);

            if (!b.getComponentOrientation().isLeftToRight()) {
                viewRect.x = bounds.x - viewRect.width - borderSize;
            }

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
    public boolean contains(final JComponent c, final int x, final int y) {
        if (!isSlider(c)) return super.contains(c, x, y);
        if (!(x >= 0 && x <= c.getWidth() && y >= 0 && y <= c.getHeight())) return false;
        Rectangle bounds = getSliderBounds(c);
        return new RoundRectangle2D.Float(bounds.x, bounds.y, bounds.width, bounds.height,
                                          bounds.height, bounds.height).contains(x, y);
    }


    private static boolean isSlider(final JComponent c) {
        return c instanceof JToggleButton
            && VARIANT_SLIDER.equals(c.getClientProperty(KEY_VARIANT));
    }

    private void paintSlider(final Graphics2D g, final AbstractButton c) {
        Rectangle bounds = getSliderBounds(c);
        g.translate(bounds.x, bounds.y);

        if (c.hasFocus()) {
            g.translate(-borderSize, -borderSize);
            DarkUIUtil.paintFocusBorder(g, bounds.width + 2 * borderSize, bounds.height + 2 * borderSize,
                                        bounds.height, borderSize);
            g.translate(borderSize, borderSize);
        }

        g.setColor(getBackgroundColor(c));
        DarkUIUtil.fillRoundRect(g, 0, 0, bounds.width, bounds.height, bounds.height);
        g.setColor(getToggleBorderColor(c));
        DarkUIUtil.paintLineBorder(g, 0, 0, bounds.width, bounds.height, bounds.height);

        int size = bounds.height - 2;
        GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
        if (c.isSelected()) {
            g.setColor(getSliderColor(c));
            DarkUIUtil.fillRoundRect(g, bounds.width - size - 1, 1, size, size, size);
            g.setColor(getSliderBorderColor(c));
            DarkUIUtil.paintLineBorder(g, bounds.width - size - 1, 1, size, size, size);
        } else {
            g.setColor(getSliderColor(c));
            DarkUIUtil.fillRoundRect(g, 1, 1, size, size, size);
            g.setColor(getSliderBorderColor(c));
            DarkUIUtil.paintLineBorder(g, 1, 1, size, size, size);
        }
        g.translate(-bounds.x, -bounds.y);
        config.restore();
    }


    private Rectangle getSliderBounds(final JComponent c) {
        int x = borderSize;
        int y = (c.getHeight() - sliderSize.height) / 2;
        rect.x = x;
        rect.y = y;
        rect.width = sliderSize.width;
        rect.height = sliderSize.height;
        if (!c.getComponentOrientation().isLeftToRight()) {
            rect.x = c.getWidth() - rect.x - rect.width;
        }
        return rect;
    }

    protected Color getToggleBorderColor(final AbstractButton b) {
        if (b.hasFocus()) {
            return focusBorderColor;
        }
        return b.isEnabled() ? borderColor : inactiveBorderColor;
    }

    protected Color getSliderColor(final AbstractButton b) {
        return b.isEnabled() ? sliderColor : inactiveSliderColor;
    }

    protected Color getSliderBorderColor(final AbstractButton b) {
        return b.isEnabled() ? sliderBorderColor : inactiveSliderBorderColor;
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
        if (isSlider(c)) {
            d.width += sliderSize.width + borderSize;
        }
        return d;
    }
}
