package com.weis.darklaf;

import javax.swing.*;

/**
 * {@link javax.swing.UIManager.LookAndFeelInfo} for {@link DarkLaf}.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class DarkLafInfo extends UIManager.LookAndFeelInfo {
    /**
     * Constructs a {@link UIManager}s {@link javax.swing.UIManager.LookAndFeelInfo} object.
     */
    public DarkLafInfo() {
        super("Darklaf", DarkLaf.class.getCanonicalName());
    }
}
