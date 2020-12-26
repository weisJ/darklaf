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
package test;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.logging.Logger;

import javax.swing.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.IntelliJTheme;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.SystemInfo;

class TooltipTest extends AbstractImageTest {

    private static final Logger LOGGER = LogUtil.getLogger(TooltipTest.class);

    public TooltipTest() {
        super("tooltip");
    }

    @BeforeAll
    static void setup() {
        LafManager.install(new IntelliJTheme());
    }

    private JToolTip createTooltip() {
        JToolTip toolTip = new JToolTip();
        toolTip.setTipText("Test ToolTip");
        toolTip.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        toolTip.putClientProperty(DarkPopupFactory.KEY_START_HIDDEN, false);
        toolTip.setSize(toolTip.getPreferredSize());
        toolTip.doLayout();
        return toolTip;
    }

    @Test
    @EnabledOnOs({OS.MAC, OS.WINDOWS, OS.LINUX})
    void testTooltipTransparency() throws Exception {
        LOGGER.info("DISPLAY = " + System.getenv("DISPLAY"));
        LOGGER.info("java.awt.headless = " + System.getProperty("java.awt.headless"));
        LOGGER.info("Headless: " + GraphicsEnvironment.isHeadless());
        JToolTip toolTip = createTooltip();

        LOGGER.info("Requesting popup");
        SwingUtilities.invokeAndWait(() -> {
            Popup popup = PopupFactory.getSharedInstance().getPopup(null, toolTip, 0, 0);
            popup.show();
            LOGGER.info("Got popup: " + popup);
        });

        Window window = DarkUIUtil.getWindow(toolTip);
        LOGGER.info("Popup Window: " + window);
        if (window != null) {
            SwingUtilities.invokeAndWait(() -> window.setOpacity(1));
        }

        TestUtils.runOnSwingThreadNotThrowing(() -> {
            Component c;
            LOGGER.info("Checking backgrounds:");
            for (c = toolTip.getParent(); c != null; c = c.getParent()) {
                Color bg = c.getBackground();
                LOGGER.info("bg = " + bg + ": " + c);
                Assertions.assertFalse(c.isOpaque(), "Component is opaque: " + c);
                Assertions.assertNotNull(bg, "Background is null for " + c);
                Assertions.assertEquals(0, bg.getAlpha(), "Background " + bg + " is opaque " + c);
                if (c instanceof Window) break;
            }
            Assertions.assertEquals(c, window, "Did not traverse full hierarchy");

            JRootPane rootPane = SwingUtilities.getRootPane(window);
            Assertions.assertNotNull(rootPane, "RootPane is null");
            Assertions.assertFalse(rootPane.isOpaque(), "RootPane is opaque");

            BufferedImage img = saveScreenShot(getPath("tooltip_" + SystemInfo.getOsName()), window);
            Assertions.assertNotNull(img, "Tooltip Image is null");
            int alpha = getAlpha(img.getRGB(img.getMinX(), img.getMinY() + img.getHeight() - 1));
            Assertions.assertEquals(0, alpha, "Tooltip is opaque");

            LOGGER.info("Done!");
        });
    }

    private int getAlpha(final int rgb) {
        return new Color(rgb, true).getAlpha();
    }
}
