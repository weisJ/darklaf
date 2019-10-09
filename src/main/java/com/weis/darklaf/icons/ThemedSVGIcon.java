package com.weis.darklaf.icons;

import com.weis.darklaf.LafManager;
import com.weis.darklaf.theme.Theme;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.net.URI;

/**
 * @author Jannis Weis
 */
public class ThemedSVGIcon extends DarkSVGIcon {

    private Theme currentTheme;

    public ThemedSVGIcon(@NotNull final URI uri, final int displayWidth, final int displayHeight) {
        super(uri, displayWidth, displayHeight);
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        ensureTheme();
        super.paintIcon(c, g, x, y);
    }

    private void ensureTheme() {
        var theme = LafManager.getTheme();
        if (currentTheme != theme) {
            IconColorMapper.patchColors(getSVGIcon());
            currentTheme = theme;
        }
    }
}
