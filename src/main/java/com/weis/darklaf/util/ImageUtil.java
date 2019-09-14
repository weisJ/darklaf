package com.weis.darklaf.util;

import com.weis.darklaf.LogFormatter;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

/**
 * Image utilities.
 *
 * @author Jannis Weis
 * @since 2018
 */
public final class ImageUtil {

    /**
     * The scaling factor.
     */
    public static final double SCALE_X;
    public static final double SCALE_Y;
    private static final Logger LOGGER = Logger.getLogger(ImageUtil.class.getName());

    static {
        LOGGER.setUseParentHandlers(false);
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new LogFormatter());
        LOGGER.addHandler(handler);

        var mode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        var screenSize = Toolkit.getDefaultToolkit().getScreenSize();
        SCALE_X = mode.getWidth() / (double) screenSize.width;
        SCALE_Y = mode.getHeight() / (double) screenSize.height;
        LOGGER.info("Using screen scaling SCALE_X=" + SCALE_X + ", SCALE_Y=" + SCALE_Y);
    }

    @Contract(pure = true)
    private ImageUtil() {
    }

    /**
     * Create image from component.
     *
     * @param c      the component.
     * @param bounds the bounds inside the component to capture.
     * @return image containing the captured area.
     */
    @NotNull
    public static Image imageFromComponent(@NotNull final Component c, @NotNull final Rectangle bounds) {
        BufferedImage image = new BufferedImage((int) (SCALE_X * bounds.width), (int) (SCALE_Y * bounds.height),
                                                BufferedImage.TYPE_INT_RGB);
        final Graphics2D g2d = (Graphics2D) image.getGraphics();
        g2d.scale(SCALE_Y, SCALE_Y);
        g2d.translate(-bounds.x, -bounds.y);
        c.printAll(g2d);

        g2d.dispose();
        return image;
    }
}
