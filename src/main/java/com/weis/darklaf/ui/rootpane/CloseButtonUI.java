package com.weis.darklaf.ui.rootpane;

import com.weis.darklaf.ui.button.DarkButtonUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
public class CloseButtonUI extends DarkButtonUI {

    protected Color closeHover;
    protected Color closeClick;

    @Override
    protected void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        closeHover = UIManager.getColor("TitlePane.close.rollOverColor");
        closeClick = UIManager.getColor("TitlePane.close.clickColor");
    }

    @Override
    protected Color getShadowColor(@NotNull final AbstractButton c) {
        return c.getModel().isArmed() ? closeClick : closeHover;
    }
}
