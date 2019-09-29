package com.weis.darklaf.ui.colorchooser;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

class RecentSwatchPanel extends SwatchPanel {
    private Color defaultRecentColor;

    protected void initValues() {
        swatchSize = UIManager.getDimension("ColorChooser.swatchesRecentSwatchSize", getLocale());
        numSwatches = new Dimension(30, 5);
        gap = new Dimension(1, 1);
    }


    protected void initColors() {
        defaultRecentColor = UIManager.getColor("ColorChooser.swatchesDefaultRecentColor", getLocale());
        int numColors = numSwatches.width * numSwatches.height;

        colors = new Color[numColors];
        for (int i = 0; i < numColors; i++) {
            colors[i] = defaultRecentColor;
        }
    }

    public void setMostRecentColor(final Color c) {
        if (colors[0].equals(c)) return;
        System.arraycopy(colors, 0, colors, 1, colors.length - 1);
        colors[0] = c;
        repaint();
    }

    @Override
    public String getToolTipText(final MouseEvent e) {
        Color color = getColorForLocation(e.getX(), e.getY());
        if (color == defaultRecentColor) return null;
        return color.getRed() + ", " + color.getGreen() + ", " + color.getBlue();
    }
}
