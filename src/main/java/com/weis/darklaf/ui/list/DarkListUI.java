package com.weis.darklaf.ui.list;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicListUI;

public class DarkListUI extends BasicListUI {

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent list) {
        return new DarkListUI();
    }
}
