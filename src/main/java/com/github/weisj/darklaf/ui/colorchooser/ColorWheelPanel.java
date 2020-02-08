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

package com.github.weisj.darklaf.ui.colorchooser;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import java.awt.*;

/**
 * @author pegov
 * @author Konstantin Bulenkov
 */
public class ColorWheelPanel extends JPanel {
    private final ColorWheel colorWheel;
    private final SlideComponent brightnessSlider;
    private SlideComponent opacitySlider = null;
    private boolean enableOpacity;

    public ColorWheelPanel(final ColorListener colorListener, final boolean enableOpacity,
                           final boolean opacityInPercent) {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));

        this.enableOpacity = enableOpacity;

        colorWheel = new ColorWheel();
        add(colorWheel, BorderLayout.CENTER);

        brightnessSlider = new SlideComponent("Brightness", true, false);
        brightnessSlider.setToolTipText("Brightness");
        brightnessSlider.addListener(value -> {
            colorWheel.setBrightness(1 - (value / 255f));
            colorWheel.repaint();
        });

        add(brightnessSlider, BorderLayout.EAST);
        colorWheel.addListener(colorListener);
        colorWheel.addListener(brightnessSlider);


        if (enableOpacity) {
            opacitySlider = new SlideComponent("Opacity", false, true);
            opacitySlider.setToolTipText("Opacity");
            opacitySlider.setUnits(opacityInPercent ? SlideComponent.Unit.PERCENT : SlideComponent.Unit.LEVEL);
            opacitySlider.addListener(integer -> {
                colorWheel.setOpacity(integer);
                colorWheel.repaint();
            });

            add(opacitySlider, BorderLayout.SOUTH);
            colorWheel.addListener(opacitySlider);

        }
    }

    public void setColor(@NotNull final Color color, final Object source) {
        float[] hsb = new float[3];
        Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), hsb);

        brightnessSlider.setValue(255 - (int) (hsb[2] * 255));
        brightnessSlider.repaint();

        colorWheel.dropImage();
        if (opacitySlider != null && source instanceof AbstractColorChooserPanel) {
            opacitySlider.setValue(color.getAlpha());
            opacitySlider.repaint();
        }

        colorWheel.setColor(color, source, hsb[0], hsb[1], hsb[2]);
    }

    public boolean isColorTransparencySelectionEnabled() {
        return enableOpacity;
    }

    public void setColorTransparencySelectionEnabled(final boolean b) {
        if (b != enableOpacity) {
            enableOpacity = b;
            opacitySlider.setEnabled(b);
            opacitySlider.setVisible(b);
        }
    }
}
