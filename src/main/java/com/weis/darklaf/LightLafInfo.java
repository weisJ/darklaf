package com.weis.darklaf;

import javax.swing.*;

/**
 * {@link javax.swing.UIManager.LookAndFeelInfo} for {@link LightLaf}.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class LightLafInfo extends UIManager.LookAndFeelInfo {
    /**
     * Constructs a {@link UIManager}s {@link javax.swing.UIManager.LookAndFeelInfo} object.
     */
    public LightLafInfo() {
        super("Darklaf-Light", LightLaf.class.getCanonicalName());
    }
}
