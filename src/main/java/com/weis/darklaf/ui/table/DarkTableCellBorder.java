package com.weis.darklaf.ui.table;

import javax.swing.border.EmptyBorder;
import javax.swing.plaf.UIResource;

/**
 * Table cell border. Prevents content to be shifted when selected.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class DarkTableCellBorder extends EmptyBorder implements UIResource {

    public DarkTableCellBorder() {
        super(2, 5, 2, 5);
    }
}
