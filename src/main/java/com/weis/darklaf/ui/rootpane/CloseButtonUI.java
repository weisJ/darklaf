package com.weis.darklaf.ui.rootpane;

import com.weis.darklaf.ui.button.DarkButtonUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

class CloseButtonUI extends DarkButtonUI {

    @Override
    protected Color getShadowColor(@NotNull final AbstractButton c) {
        return c.getModel().isArmed() ? UIManager.getColor("TitlePane.close.clickColor")
                                      : UIManager.getColor("TitlePane.close.rollOverColor");
    }
}
