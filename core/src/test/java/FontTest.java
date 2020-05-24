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
import java.awt.*;
import java.awt.font.TextAttribute;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.text.AttributedCharacterIterator;
import java.util.Collections;
import java.util.Map;

import javax.imageio.ImageIO;
import javax.swing.*;

import ui.DemoResources;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.graphics.ImageUtil;
import com.github.weisj.darklaf.theme.IntelliJTheme;
import com.github.weisj.darklaf.util.SystemInfo;

public class FontTest {

    private static final Map<AttributedCharacterIterator.Attribute, Integer> kerning;
    private static final int SCALING_FACTOR = 3;
    private static final String WORKING_DIR = "font_test";

    static {
        kerning = Collections.singletonMap(TextAttribute.KERNING,
                                           TextAttribute.KERNING_ON);
    }

    public static void main(final String[] args) throws IOException {
        if (!SystemInfo.isMacOSCatalina) return;
        Files.createDirectory(new File(WORKING_DIR).toPath());
        LafManager.install(new IntelliJTheme());
        JTextArea textArea = new JTextArea();
        textArea.setText(DemoResources.KERNING_TEST);
        textArea.setSize(textArea.getPreferredSize());
        textArea.doLayout();

        textArea.setFont(createFont(".AppleSystemUIFont"));
        setFractionalMetrics(true);
        saveScreenShot("system_font_frac_metrics", textArea);

        textArea.setFont(createFont(".SF NS Text"));
        setFractionalMetrics(false);
        saveScreenShot("san_francisco", textArea);

        textArea.setFont(createFont(".SF NS Text"));
        setFractionalMetrics(true);
        saveScreenShot("san_francisco_fra_metrics", textArea);

        textArea.setFont(createFont("Helvetica Neue"));
        setFractionalMetrics(false);
        saveScreenShot("helvetica", textArea);

        textArea.setFont(createFont("Helvetica Neue"));
        setFractionalMetrics(true);
        saveScreenShot("helvetica_frac_metrics", textArea);
    }

    private static void saveScreenShot(final String name, final JComponent c) {
        try {
            Rectangle rect = new Rectangle(0, 0, c.getWidth(), c.getHeight());
            BufferedImage image = ImageUtil.scaledImageFromComponent(c, rect, SCALING_FACTOR, SCALING_FACTOR, false);
            ImageIO.write(image, "png", new File(WORKING_DIR + "/" + name + ".png"));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private static void setFractionalMetrics(final boolean enabled) {
        UIManager.put(RenderingHints.KEY_FRACTIONALMETRICS,
                      enabled ? RenderingHints.VALUE_FRACTIONALMETRICS_ON
                              : RenderingHints.VALUE_FRACTIONALMETRICS_OFF);
    }

    private static Font createFont(final String name) {
        return new Font(name, Font.PLAIN, 12).deriveFont(kerning);
    }
}
