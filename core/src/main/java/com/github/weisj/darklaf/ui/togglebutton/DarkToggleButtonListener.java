/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.event.ChangeEvent;

import com.github.weisj.darklaf.graphics.Animator;
import com.github.weisj.darklaf.graphics.DefaultInterpolator;
import com.github.weisj.darklaf.ui.button.DarkButtonListener;
import com.github.weisj.darklaf.util.PropertyKey;

public class DarkToggleButtonListener extends DarkButtonListener<DarkToggleButtonUI> implements ToggleButtonConstants {

    private final SliderAnimator animator;
    private final AbstractButton button;
    private boolean selected;

    public DarkToggleButtonListener(final AbstractButton b, final DarkToggleButtonUI ui) {
        super(b, ui);
        this.selected = b.isSelected();
        button = b;
        animator = createAnimator();
        animator.setEnabled(UIManager.getBoolean("ToggleButton.animated"));
        animator.state = selected ? 1 : 0;
    }

    protected SliderAnimator createAnimator() {
        return new SliderAnimator(button);
    }

    public float getAnimationState() {
        return animator.getState();
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        AbstractButton b = (AbstractButton) evt.getSource();
        String key = evt.getPropertyName();
        if (ToggleButtonConstants.KEY_VARIANT.equals(key)) {
            Object oldVal = evt.getOldValue();
            Object newVal = evt.getNewValue();
            if (oldVal != null && oldVal.equals(newVal)) {
                return;
            }
            b.setBorderPainted(!VARIANT_SLIDER.equals(newVal));
            ui.updateMargins(b);
        } else if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            b.doLayout();
            b.repaint();
        }
    }

    @Override
    public void stateChanged(final ChangeEvent e) {
        super.stateChanged(e);
        if (!ToggleButtonConstants.isSlider(button)) return;
        boolean sel = button.isSelected();
        if (sel != selected) {
            selected = sel;
            float endState = sel ? 1 : 0;
            int startFrame = 0;
            if (animator.isRunning()) {
                startFrame = animator.getCurrentFrame();
            }
            animator.animationBounds = ui.getSliderBounds(button);
            animator.suspend();
            animator.reverse = !sel;
            animator.setEndValue(endState);
            animator.resume(startFrame, button);
        }
    }

    protected static class SliderAnimator extends Animator {

        private final JComponent c;
        private float state;
        private float endValue;
        private boolean reverse;
        private Rectangle animationBounds;

        public SliderAnimator(final JComponent c) {
            super(10, 100, 0);
            setInterpolator(DefaultInterpolator.EASE_OUT_QUAD);
            this.c = c;
        }

        public float getState() {
            return state;
        }

        @Override
        public void paintNow(final float fraction) {
            this.state = reverse ? 1 - fraction : fraction;
            repaint();
        }

        @Override
        protected void paintCycleEnd() {
            System.out.println("here");
            this.state = endValue;
            repaint();
            animationBounds = null;
        }

        private void repaint() {
            if (c != null) {
                c.paintImmediately(animationBounds != null ? animationBounds : c.getVisibleRect());
            }
        }

        public void setEndValue(final float endValue) {
            this.endValue = endValue;
        }
    }
}
