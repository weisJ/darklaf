// Copyright 2000-2019 JetBrains s.r.o. Use of this source code is governed by the Apache 2.0 license
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
package com.weis.darklaf.ui.colorchooser;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import java.awt.*;

/**
 * @author pegov
 * @author Konstantin Bulenkov
 */
public class ColorWheelPanel extends JPanel {
    private final ColorWheel myColorWheel;
    private final SlideComponent myBrightnessComponent;
    private SlideComponent myOpacityComponent = null;
    private boolean enableOpacity;

    public ColorWheelPanel(final ColorListener colorListener, final boolean enableOpacity,
                           final boolean opacityInPercent) {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));

        this.enableOpacity = enableOpacity;

        myColorWheel = new ColorWheel();
        add(myColorWheel, BorderLayout.CENTER);

        myColorWheel.addListener(colorListener);

        myBrightnessComponent = new SlideComponent("Brightness", true);
        myBrightnessComponent.setToolTipText("Brightness");
        myBrightnessComponent.addListener(value -> {
            myColorWheel.setBrightness(1 - (value / 255f));
            myColorWheel.repaint();
        });

        add(myBrightnessComponent, BorderLayout.EAST);

        if (enableOpacity) {
            myOpacityComponent = new SlideComponent("Opacity", false);
            myOpacityComponent.setUnits(opacityInPercent ? SlideComponent.Unit.PERCENT : SlideComponent.Unit.LEVEL);
            myOpacityComponent.setToolTipText("Opacity");
            myOpacityComponent.addListener(integer -> {
                myColorWheel.setOpacity(integer);
                myColorWheel.repaint();
            });

            add(myOpacityComponent, BorderLayout.SOUTH);
        }
    }

    public void setColor(@NotNull final Color color, final Object source) {
        float[] hsb = new float[3];
        Color.RGBtoHSB(color.getRed(), color.getGreen(), color.getBlue(), hsb);

        myBrightnessComponent.setValue(255 - (int) (hsb[2] * 255));
        myBrightnessComponent.repaint();

        myColorWheel.dropImage();
        if (myOpacityComponent != null && source instanceof AbstractColorChooserPanel) {
            myOpacityComponent.setValue(color.getAlpha());
            myOpacityComponent.repaint();
        }

        myColorWheel.setColor(color, source, hsb[0], hsb[1], hsb[2]);
    }

    public boolean isColorTransparencySelectionEnabled() {
        return enableOpacity;
    }

    public void setColorTransparencySelectionEnabled(final boolean b) {
        if (b != enableOpacity) {
            enableOpacity = b;
            myOpacityComponent.setEnabled(b);
            myOpacityComponent.setVisible(b);
        }
    }
}
