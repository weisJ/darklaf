package com.weis.darklaf.ui;

import com.weis.darklaf.components.border.AdaptiveLineBorder;

import java.awt.*;

/**
 * Table cell border. Prevents content to be shifted when selected.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class MimaTableCellBorder extends AdaptiveLineBorder {

    /**
     * Create new Cell border.
     */
    public MimaTableCellBorder() {
        super(1, 1, 1, 1, "Border.line1");
    }

    @Override
    public Insets getBorderInsets(final Component c) {
        var i = super.getBorderInsets(c);
        i.left += 1;
        return i;
    }
}
