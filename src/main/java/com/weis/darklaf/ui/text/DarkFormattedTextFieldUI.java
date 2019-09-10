package com.weis.darklaf.ui.text;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

public class DarkFormattedTextFieldUI extends DarkTextFieldUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkFormattedTextFieldUI();
    }
}
