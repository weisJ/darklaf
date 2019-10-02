package com.weis.darklaf.ui.colorchooser;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.colorchooser.AbstractColorChooserPanel;
import java.awt.*;

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
