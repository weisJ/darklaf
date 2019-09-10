package com.weis.darklaf;

import com.bulenkov.iconloader.util.SystemInfo;
import org.jetbrains.annotations.NotNull;

import javax.swing.plaf.metal.MetalLookAndFeel;

/**
 * Extension of {@link DarkLaf} to light colours.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class LightLaf extends DarkLaf {

    private static final String NAME = "Darklaf-Light";

    /**
     * Create new light Darcula LaF.
     */
    public LightLaf() {
        super();
        if (SystemInfo.isWindows || SystemInfo.isLinux) {
            MetalLookAndFeel.setCurrentTheme(new LightMetalTheme());
        }
    }

    @NotNull
    @Override
    public String getPrefix() {
        return "darcula_light";
    }

    @NotNull
    @Override
    public String getName() {
        return NAME;
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Light Look and feel based on Darcula-LAF";
    }
}
