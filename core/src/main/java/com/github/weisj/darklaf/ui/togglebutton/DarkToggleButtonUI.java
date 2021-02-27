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
package com.github.weisj.darklaf.ui.togglebutton;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicButtonListener;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;
import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.ui.button.ButtonConstants;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;

/** @author Jannis Weis */
public class DarkToggleButtonUI extends DarkButtonUI implements ToggleButtonConstants {

    protected boolean showSliderHints;
    protected Dimension sliderSize;
    protected Color selectedBackground;
    protected Color backgroundInactive;
    protected Color focusBorderColor;
    protected Color borderColor;
    protected Color inactiveBorderColor;
    protected Color sliderColor;
    protected Color inactiveSliderColor;
    protected Color sliderBorderColor;
    protected Color inactiveSliderBorderColor;
    protected Color selectedForeground;
    protected DarkToggleButtonListener toggleButtonListener;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkToggleButtonUI();
    }

    @Override
    protected void installDefaults(final AbstractButton b) {
        sliderSize = UIManager.getDimension("ToggleButton.sliderSize");

        selectedBackground = UIManager.getColor("ToggleButton.activeFillColor");
        backgroundInactive = UIManager.getColor("ToggleButton.inactiveFillColor");
        focusBorderColor = UIManager.getColor("ToggleButton.focusedSliderBorderColor");
        borderColor = UIManager.getColor("ToggleButton.sliderBorderColor");
        inactiveBorderColor = UIManager.getColor("ToggleButton.disabledSliderBorderColor");

        sliderColor = UIManager.getColor("ToggleButton.sliderKnobFillColor");
        inactiveSliderColor = UIManager.getColor("ToggleButton.disabledSliderKnobFillColor");
        sliderBorderColor = UIManager.getColor("ToggleButton.sliderKnobBorderColor");
        inactiveSliderBorderColor = UIManager.getColor("ToggleButton.disabledSliderKnobBorderColor");
        selectedForeground = UIManager.getColor("ToggleButton.selectedForeground");
        showSliderHints = UIManager.getBoolean("ToggleButton.showSliderHints");

        super.installDefaults(b);
    }

    @Override
    protected LayoutManager createLayout() {
        return new DarkToggleButtonLayout();
    }

    @Override
    protected BasicButtonListener createButtonListener(final AbstractButton b) {
        if (toggleButtonListener == null) {
            toggleButtonListener = new DarkToggleButtonListener(b, this);
        }
        return toggleButtonListener;
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        if (ToggleButtonConstants.isSlider(c)) {
            GraphicsContext config = GraphicsUtil.setupStrokePainting(g);
            paintSlider((Graphics2D) g, (AbstractButton) c);
            config.restoreClip();
        }
        super.paint(g, c);
    }

    @Override
    protected boolean shouldDrawBackground(final AbstractButton c) {
        return super.shouldDrawBackground(c) && !ToggleButtonConstants.isSlider(c);
    }

    @Override
    protected Insets getMargins(final AbstractButton b) {
        if (ToggleButtonConstants.isSlider(b)) {
            boolean ltr = b.getComponentOrientation().isLeftToRight();
            int extra = 2 * borderSize + getSliderBounds(b).width;
            int left = ltr ? extra : 0;
            int right = !ltr ? extra : 0;
            return new Insets(0, left, 0, right);
        }
        return super.getMargins(b);
    }

    private void paintSlider(final Graphics2D g, final AbstractButton c) {
        Rectangle bounds = getSliderBounds(c);
        g.translate(bounds.x, bounds.y);

        if (c.hasFocus()) {
            g.translate(-borderSize, -borderSize);
            PaintUtil.paintFocusBorder(g, bounds.width + 2 * borderSize, bounds.height + 2 * borderSize, bounds.height,
                    borderSize);
            g.translate(borderSize, borderSize);
        }

        int knobSize = bounds.height;
        int arc = Math.min(bounds.width, bounds.height);

        Rectangle knobBounds = new Rectangle(0, 0, knobSize, knobSize);
        knobBounds.x = (int) ((bounds.width - knobBounds.width) * toggleButtonListener.getAnimationState());

        boolean enabled = c.isEnabled();
        Color selectedBg = getBackgroundColor(c, false, false, false, enabled, true);
        Color deselectedBg = getBackgroundColor(c, false, false, false, enabled, false);

        Shape clip = g.getClip();
        g.clipRect(0, 0, knobBounds.x + knobBounds.width / 2, bounds.height);
        g.setColor(selectedBg);
        PaintUtil.fillRoundRect(g, 0, 0, bounds.width, bounds.height, arc);
        g.setClip(clip);

        g.clipRect(knobBounds.x + knobBounds.width / 2, 0, bounds.width - knobBounds.width / 2, bounds.height);
        g.setColor(deselectedBg);
        PaintUtil.fillRoundRect(g, 0, 0, bounds.width, bounds.height, arc);
        g.setClip(clip);

        g.setColor(getToggleBorderColor(c));
        PaintUtil.paintLineBorder(g, 0, 0, bounds.width, bounds.height, arc);

        if (showSliderHints) {
            paintSliderHints(g, c, bounds, knobSize);
        }

        paintSliderKnob(g, c, knobBounds);

        g.translate(-bounds.x, -bounds.y);
    }

    protected void paintSliderHints(final Graphics2D g, final AbstractButton c, final Rectangle bounds,
            final int knobSize) {
        int pad = 5;
        int w = bounds.width - knobSize - 2 * pad;
        int y = (bounds.height - w) / 2;
        if (c.isSelected()) {
            g.setColor(selectedForeground);
            g.fillRect(pad + (w - 1) / 2, y, 1, w);
        } else {
            int x = knobSize + pad;
            g.setColor(getForegroundColor(c, false, false));
            PaintUtil.paintLineBorder(g, x, y, w, w, w);
        }
    }

    protected void paintSliderKnob(final Graphics2D g, final AbstractButton c, final Rectangle bound) {
        g.setColor(getSliderColor(c));
        PaintUtil.fillRoundRect(g, bound.x, bound.y, bound.width, bound.height, bound.height);
        g.setColor(getSliderBorderColor(c));
        PaintUtil.paintLineBorder(g, bound.x, bound.y, bound.width, bound.height, bound.height);
    }


    @Override
    protected Color getForegroundColor(final AbstractButton b, final boolean defaultButton, final boolean enabled) {
        if (b.isSelected() && enabled && b.getForeground() instanceof UIResource && !ToggleButtonConstants.isSlider(b)
                && !ButtonConstants.isBorderlessVariant(b)) {
            return selectedForeground;
        }
        return super.getForegroundColor(b, defaultButton, enabled);
    }

    @Override
    protected Color getBackgroundColor(final AbstractButton b, final boolean defaultButton, final boolean rollOver,
            final boolean clicked, final boolean enabled) {
        boolean effectiveRollover = rollOver;
        boolean effectiveArmed = clicked;
        if (effectiveRollover || effectiveArmed) {
            boolean slider = !ToggleButtonConstants.isSlider(b);
            effectiveArmed &= slider;
            effectiveRollover &= slider;
        }
        return getBackgroundColor(b, defaultButton, effectiveRollover, effectiveArmed, enabled,
                b.getModel().isSelected());
    }

    protected Color getBackgroundColor(final AbstractButton b, final boolean defaultButton, final boolean rollOver,
            final boolean clicked, final boolean enabled, final boolean selected) {
        if (!enabled) return backgroundInactive;
        if (selected) return selectedBackground;
        return super.getBackgroundColor(b, defaultButton, rollOver, clicked, true);
    }

    @Override
    public boolean isRolloverBorderless(final AbstractButton b) {
        return super.isRolloverBorderless(b) || b.isSelected();
    }

    @Override
    public boolean isArmedBorderless(final AbstractButton b) {
        return super.isArmedBorderless(b) || b.isSelected();
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

    protected Rectangle getSliderBounds(final JComponent c) {
        Rectangle r = new Rectangle();
        Insets ins = c.getInsets();
        int x = ins.left;
        int height = c.getHeight() - ins.bottom - ins.top;
        int y = ins.top + (height - sliderSize.height) / 2;
        r.x = x;
        r.y = y;
        r.width = sliderSize.width;
        r.height = sliderSize.height;
        if (!c.getComponentOrientation().isLeftToRight()) {
            r.x = c.getWidth() - ins.right - r.x - r.width;
        }
        return r;
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        Dimension dim = super.getPreferredSize(c);
        if (ToggleButtonConstants.isSlider(c)) {
            Insets ins = c.getInsets();
            dim.height = Math.max(dim.height, sliderSize.height + ins.top + ins.bottom);
        }
        return dim;
    }

    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        if (!ToggleButtonConstants.isSlider(c)) return super.contains(c, x, y);
        if (c instanceof JToggleButton) {
            Rectangle bounds = getSliderBounds(c);
            int arc = Math.min(bounds.width, bounds.height);
            hitArea.setRoundRect(bounds.x, bounds.y, bounds.width, bounds.height, arc, arc);
        }
        return hitArea.contains(x, y);
    }

    protected class DarkToggleButtonLayout extends DarkButtonLayout {

        @Override
        protected int getHorizontalAlignment(final AbstractButton b) {
            if (ToggleButtonConstants.isSlider(b)) {
                int horizontalPos = SwingConstants.LEFT;
                if (!b.getComponentOrientation().isLeftToRight()) {
                    horizontalPos = SwingConstants.RIGHT;
                }
                return horizontalPos;
            }
            return super.getHorizontalAlignment(b);
        }
    }
}
