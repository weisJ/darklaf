/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.util.Map;
import java.util.WeakHashMap;

import javax.swing.*;

import com.github.weisj.darklaf.util.ImageUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.Scale;

public class IconUtil {

    private static final int FRAME_ICON_SIZE = 32;
    private static final Map<Window, Icon> windowIconSet = new WeakHashMap<>();

    /** Reload all created frame icons if necessary. */
    public static void reloadDynamicFrameIcons() {
        SwingUtilities.invokeLater(() -> {
            windowIconSet.forEach((window, icon) -> window.setIconImage(iconToImage(icon, window)));
        });
    }

    static int getDynamicFrameIconCount() {
        return windowIconSet.size();
    }

    public static Image createFrameIcon(final Icon icon, final Window c) {
        if (icon == null)
            return null;
        if (c != null) {
            if (isDynamic(icon)) {
                windowIconSet.put(c, icon);
            }
            PropertyChangeListener propertyChangeListener = e -> c.setIconImage(iconToImage(icon, c));
            c.addPropertyChangeListener(PropertyKey.GRAPHICS_CONFIGURATION, propertyChangeListener);
        }
        return createScaledFrameIcon(icon, c);
    }

    private static boolean isDynamic(final Icon icon) {
        return icon instanceof DynamicIcon;
    }

    private static Image createScaledFrameIcon(final Icon icon, final Window c) {
        if (c != null && !c.isVisible()) {
            Component parent = c.getParent();
            if (parent != null) {
                return iconToImage(icon, c);
            }
        }
        return iconToImage(icon, c);
    }

    public static Image iconToImage(final Icon icon, final Component c) {
        if (icon == null)
            return null;
        int w = icon.getIconWidth();
        int h = icon.getIconHeight();
        GraphicsConfiguration gc = c.getGraphicsConfiguration();
        double sx = Scale.getScaleX(gc);
        double sy = Scale.getScaleY(gc);
        double scaleX = sx * (((double) FRAME_ICON_SIZE) / w);
        double scaleY = sy * (((double) FRAME_ICON_SIZE) / h);
        return createScaledImage(icon, scaleX, scaleY);
    }

    public static Image createScaledImage(final Icon icon, final double scalex, final double scaley) {
        if (icon == null)
            return null;
        int w = (int) (scalex * icon.getIconWidth());
        int h = (int) (scaley * icon.getIconHeight());
        if (icon instanceof ImageSource) {
            return ((ImageSource) icon).createImage(w, h);
        } else {
            BufferedImage image = ImageUtil.createCompatibleTransparentImage(w, h);
            Graphics2D g = (Graphics2D) image.getGraphics();
            g.scale(scalex, scaley);
            icon.paintIcon(null, g, 0, 0);
            g.dispose();
            return image;
        }
    }
}
