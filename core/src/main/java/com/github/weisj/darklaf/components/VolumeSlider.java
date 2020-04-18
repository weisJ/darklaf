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
package com.github.weisj.darklaf.components;

import javax.swing.*;

import com.github.weisj.darklaf.ui.slider.DarkSliderUI;

public class VolumeSlider extends JSlider {

    private boolean showVolumeIcon;

    /**
     * Creates a horizontal slider with the range 0 to 100 and an initial value of 50.
     */
    public VolumeSlider() {
        this(HORIZONTAL, 0, 100, 50);
    }

    /**
     * Creates a slider using the specified orientation with the range {@code 0} to {@code 100} and an initial value of
     * {@code 50}. The orientation can be either <code>SwingConstants.VERTICAL</code> or
     * <code>SwingConstants.HORIZONTAL</code>.
     *
     * @param  orientation              the orientation of the slider
     * @throws IllegalArgumentException if orientation is not one of {@code VERTICAL}, {@code HORIZONTAL}
     * @see                             #setOrientation
     */
    public VolumeSlider(final int orientation) {
        this(orientation, 0, 100, 50);
    }

    /**
     * Creates a horizontal slider using the specified min and max with an initial value equal to the average of the min
     * plus max.
     * <p>
     * The <code>BoundedRangeModel</code> that holds the slider's data handles any issues that may arise from improperly
     * setting the minimum and maximum values on the slider. See the {@code BoundedRangeModel} documentation for
     * details.
     *
     * @param min the minimum value of the slider
     * @param max the maximum value of the slider
     * @see       BoundedRangeModel
     * @see       #setMinimum
     * @see       #setMaximum
     */
    public VolumeSlider(final int min, final int max) {
        this(HORIZONTAL, min, max, (min + max) / 2);
    }

    /**
     * Creates a horizontal slider using the specified min, max and value.
     * <p>
     * The <code>BoundedRangeModel</code> that holds the slider's data handles any issues that may arise from improperly
     * setting the minimum, initial, and maximum values on the slider. See the {@code BoundedRangeModel} documentation
     * for details.
     *
     * @param min   the minimum value of the slider
     * @param max   the maximum value of the slider
     * @param value the initial value of the slider
     * @see         BoundedRangeModel
     * @see         #setMinimum
     * @see         #setMaximum
     * @see         #setValue
     */
    public VolumeSlider(final int min, final int max, final int value) {
        this(HORIZONTAL, min, max, value);
    }

    /**
     * Creates a slider with the specified orientation and the specified minimum, maximum, and initial values. The
     * orientation can be either <code>SwingConstants.VERTICAL</code> or
     * <code>SwingConstants.HORIZONTAL</code>.
     * <p>
     * The <code>BoundedRangeModel</code> that holds the slider's data handles any issues that may arise from improperly
     * setting the minimum, initial, and maximum values on the slider. See the {@code BoundedRangeModel} documentation
     * for details.
     *
     * @param  orientation              the orientation of the slider
     * @param  min                      the minimum value of the slider
     * @param  max                      the maximum value of the slider
     * @param  value                    the initial value of the slider
     * @throws IllegalArgumentException if orientation is not one of {@code VERTICAL}, {@code HORIZONTAL}
     * @see                             BoundedRangeModel
     * @see                             #setOrientation
     * @see                             #setMinimum
     * @see                             #setMaximum
     * @see                             #setValue
     */
    public VolumeSlider(final int orientation, final int min, final int max, final int value) {
        super(orientation, min, max, value);
        init();
    }

    /**
     * Creates a horizontal slider using the specified BoundedRangeModel.
     *
     * @param brm a {@code BoundedRangeModel} for the slider
     */
    public VolumeSlider(final BoundedRangeModel brm) {
        super(brm);
        init();
    }

    private void init() {
        putClientProperty(DarkSliderUI.KEY_VARIANT, DarkSliderUI.VARIANT_VOLUME);
        putClientProperty(DarkSliderUI.KEY_INSTANT_SCROLL, true);
    }

    public boolean isShowVolumeIcon() {
        return showVolumeIcon;
    }

    public void setShowVolumeIcon(final boolean showVolumeIcon) {
        this.showVolumeIcon = showVolumeIcon;
        putClientProperty(DarkSliderUI.KEY_SHOW_VOLUME_ICON, showVolumeIcon);
    }
}
