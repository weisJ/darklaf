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
package com.github.weisj.darklaf.ui.colorchooser;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.color.DarkColorModel;

/**
 * @author pegov
 * @author Konstantin Bulenkov
 */
public class ColorWheelPanel extends JPanel {
    private final ColorTriangle colorWheel;
    private SlideComponent opacitySlider = null;
    private boolean enableOpacity;

    public ColorWheelPanel(final boolean enableOpacity, final boolean opacityInPercent) {
        setLayout(new BorderLayout());
        setBorder(BorderFactory.createEmptyBorder(5, 0, 5, 0));

        this.enableOpacity = enableOpacity;

        colorWheel = new ColorTriangle();
        add(colorWheel, BorderLayout.CENTER);

        if (enableOpacity) {
            opacitySlider = new SlideComponent("Opacity", false, true);
            opacitySlider.setToolTipText("Opacity");
            opacitySlider.setUnits(opacityInPercent ? SlideComponent.Unit.PERCENT : SlideComponent.Unit.LEVEL);
            opacitySlider.addListener(integer -> {
                colorWheel.setOpacity(integer / 255.0);
                ColorWheelPanel.this.repaint();
            });
            add(opacitySlider, BorderLayout.SOUTH);
            colorWheel.addListener(opacitySlider);
        }
    }

    public void addListener(final ColorListener listener) {
        colorWheel.addListener(listener);
    }

    public void setColor(final Color color, final Object source) {
        if (opacitySlider != null) {
            opacitySlider.setValue(color.getAlpha());
            opacitySlider.repaint();
        }
        colorWheel.setColor(source, color);
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

    public void setModel(final DarkColorModel darkColorModel) {
        colorWheel.setColorModel(darkColorModel);
    }
}
