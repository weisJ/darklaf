package com.weis.darklaf.ui.internalframe;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicDesktopIconUI;

public class DarkDesktopIconUI extends BasicDesktopIconUI {

    public static ComponentUI createUI(final JComponent c) {
        return new DarkDesktopIconUI();
    }

}
