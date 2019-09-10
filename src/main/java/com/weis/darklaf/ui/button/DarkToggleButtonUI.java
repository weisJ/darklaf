package com.weis.darklaf.ui.button;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

public class DarkToggleButtonUI extends DarkButtonUI {

    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return new DarkToggleButtonUI();
    }


    protected Color getBackgroundColor(@NotNull final JComponent c) {
        return c.isEnabled() || (c instanceof JToggleButton && ((JToggleButton) c).isSelected())
               ? (c instanceof JButton && (((JButton) c).isDefaultButton()))
                 ? UIManager.getColor("Button.darcula.defaultFillColor")
                 : UIManager.getColor("Button.darcula.activeFillColor")
               : UIManager.getColor("Button.darcula.inactiveFillColor");
    }
}
