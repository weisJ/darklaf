package com.weis.darklaf.ui.rootpane;

import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.ui.button.DarkButtonUI;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

/**
 * @author Jannis Weis
 */
class CloseButtonUI extends DarkButtonUI {

    @Override
    protected Color getShadowColor(@NotNull final AbstractButton c) {
        return c.getModel().isArmed() ? DarkColors.get().getTitleCloseClickBackground()
                                      : DarkColors.get().getTitleCloseHoverBackground();
    }
}
