package com.weis.darklaf.components.border;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 * @since 2019
 */
public class AdaptiveLineBorder extends MutableLineBorder {

    private final String colorKey;

    public AdaptiveLineBorder(
            final int top, final int left, final int bottom, final int right, final String color) {
        super(top, left, bottom, right, UIManager.getColor(color));
        this.colorKey = color;
    }

    @Override
    protected Color getColor() {
        return UIManager.getColor(colorKey);
    }
}
