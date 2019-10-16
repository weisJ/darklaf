package com.weis.darklaf.ui.internalframe;

import com.weis.darklaf.components.border.MutableLineBorder;

import javax.swing.*;
import javax.swing.plaf.UIResource;

/**
 * @author Jannis Weis
 */
public class DarkDesktopIconBorder extends MutableLineBorder implements UIResource {

    public DarkDesktopIconBorder() {
        super(1, 1, 1, 1, null);
        setColor(UIManager.getColor("DesktopIcon.borderColor"));
    }
}
