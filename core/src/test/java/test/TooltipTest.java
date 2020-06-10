/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 */
package test;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.concurrent.atomic.AtomicReference;

import javax.swing.*;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.IntelliJTheme;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.SystemInfo;

public class TooltipTest extends AbstractImageTest {

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
        toolTip.setSize(toolTip.getPreferredSize());
        toolTip.doLayout();
        return toolTip;
    }

    @Test
    @EnabledOnOs({OS.MAC, OS.WINDOWS})
    public void testTooltipTransparency() throws Exception {
        JToolTip toolTip = createTooltip();

        SwingUtilities.invokeAndWait(() -> {
            Popup popup = PopupFactory.getSharedInstance().getPopup(null, toolTip, 0, 0);
            popup.show();
        });

        Window window = DarkUIUtil.getWindow(toolTip);
        SwingUtilities.invokeAndWait(() -> window.setOpacity(1));

        AtomicReference<Exception> exception = new AtomicReference<>();

        SwingUtilities.invokeLater(() -> {
            try {
                JRootPane rootPane = SwingUtilities.getRootPane(window);
                Assertions.assertNotNull(rootPane, "RootPane is null");
                Assertions.assertFalse(rootPane.isOpaque(), "RootPane is opaque");

                Color backgroundColor = window.getBackground();
                Assertions.assertNotNull(backgroundColor, "Background is null");
                Assertions.assertEquals(0, backgroundColor.getAlpha(), "Background is opaque");

                BufferedImage img = saveScreenShot(getPath("tooltip_" + SystemInfo.getOsName()), window);
                Assertions.assertNotNull(img, "Tooltip Image is null");
                int alpha = getAlpha(img.getRGB(img.getMinX(), img.getMinY() + img.getHeight() - 1));
                Assertions.assertEquals(0, alpha, "Tooltip is opaque");
            } catch (Exception e) {
                exception.set(e);
            }
        });

        Assertions.assertNotNull(exception.get(), () -> exception.get().getMessage());
    }

    private int getAlpha(final int rgb) {
        return new Color(rgb, true).getAlpha();
    }
}
