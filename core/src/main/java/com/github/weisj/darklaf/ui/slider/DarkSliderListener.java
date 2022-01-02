/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.slider;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;

import com.github.weisj.darklaf.util.PropertyKey;

public class DarkSliderListener extends MouseAdapter implements PropertyChangeListener {

    private final DarkSliderUI ui;
    private final JSlider slider;
    private boolean muted = false;
    private int oldValue;

    public DarkSliderListener(final DarkSliderUI ui, final JSlider slider) {
        this.ui = ui;
        this.slider = slider;
    }

    @Override
    public void mouseWheelMoved(final MouseWheelEvent e) {
        super.mouseWheelMoved(e);
        if (!slider.hasFocus()) return;
        if (e.getScrollType() == MouseWheelEvent.WHEEL_BLOCK_SCROLL) return;
        int amount = e.getWheelRotation();
        boolean ltr = slider.getComponentOrientation().isLeftToRight();
        if (ltr) amount *= -1;
        if (slider.getSnapToTicks()) {
            int spacing = slider.getMinorTickSpacing();
            amount *= spacing;
        }
        ui.setValue(slider.getValue() + amount, true);
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        if (slider.isEnabled() && ui.showVolumeIcon(slider) && ui.iconRect.contains(e.getPoint())) {
            if (muted && slider.getValue() == slider.getMinimum()) {
                ui.setValue(oldValue, true);
                muted = false;
            } else {
                oldValue = slider.getValue();
                ui.setValue(slider.getMinimum(), true);
                muted = true;
            }
        }
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (DarkSliderUI.KEY_VARIANT.equals(key)
                || DarkSliderUI.KEY_SHOW_VOLUME_ICON.equals(key)
                || PropertyKey.ORIENTATION.equals(key)
                || PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            ui.updateVisualPaddings();
            slider.repaint();
        }
    }
}
