package com.weis.darklaf.ui.label;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicLabelUI;

public class DarkLabelUI extends BasicLabelUI {

    protected static final DarkLabelUI darkLabelUI = new DarkLabelUI();

    @Contract(pure = true)
    public static ComponentUI createUI(final JComponent c) {
        return darkLabelUI;
    }
}
