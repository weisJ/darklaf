/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

import javax.swing.*;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.condition.EnabledOnOs;
import org.junit.jupiter.api.condition.OS;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.color.ColorUtil;
import com.github.weisj.darklaf.theme.DarculaTheme;
import com.github.weisj.darklaf.theme.IntelliJTheme;
import com.github.weisj.darklaf.ui.rootpane.DarkRootPaneUI;
import com.github.weisj.darklaf.util.SystemInfo;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class CustomTitleBarTest extends AbstractImageTest {

    private static final Color TITLE_BAR_COLOR = Color.RED;
    private static final Color CONTENT_COLOR = Color.BLUE;
    private static final int TITLE_BAR_Y = 10;
    private static final int TOLERANCE = SystemInfo.isMac ? 55 : 0;

    public CustomTitleBarTest() {
        super("titlebar");
    }

    @BeforeAll
    static void setup() {
        LafManager.registerInitTask((t, d) -> {
            d.put("MacOS.TitlePane.background", TITLE_BAR_COLOR);
            d.put("MacOS.TitlePane.inactiveBackground", TITLE_BAR_COLOR);
            d.put("Windows.TitlePane.background", TITLE_BAR_COLOR);
            d.put("Windows.TitlePane.inactiveBackground", TITLE_BAR_COLOR);
        });
    }

    @BeforeEach
    void beforeEach() {
        LafManager.setDecorationsEnabled(true);
    }

    private AtomicReference<JFrame> createFrame(final Consumer<JFrame> frameModifier) {
        AtomicReference<JFrame> frame = new AtomicReference<>();
        final Object lock = new Object();
        TestUtils.runOnSwingThreadNotThrowing(() -> {
            JFrame f = new JFrame("");
            frame.set(f);
            JPanel content = new JPanel();
            content.setBackground(CONTENT_COLOR);
            content.setPreferredSize(new Dimension(200, 200));
            f.setContentPane(content);
            f.pack();
            f.setLocationRelativeTo(null);
            f.addWindowListener(new WindowAdapter() {
                @Override
                public void windowOpened(final WindowEvent e) {
                    f.removeWindowListener(this);
                    SwingUtilities.invokeLater(() -> {
                        synchronized (lock) {
                            lock.notify();
                        }
                    });
                }
            });
            f.setAlwaysOnTop(true);
            f.setVisible(true);
            frameModifier.accept(f);
        });
        synchronized (lock) {
            try {
                lock.wait(100000);
                // Wait some time because the window may still be transparent.
                Thread.sleep(500);
            } catch (final InterruptedException e) {
                e.printStackTrace();
                Thread.currentThread().interrupt();
            }
        }
        try {
            new Robot().waitForIdle();
        } catch (AWTException e) {
            e.printStackTrace();
        }
        return frame;
    }

    private boolean checkScreenColor(final Color expected, final Color value) {
        return Math.abs(expected.getRed() - value.getRed()) <= CustomTitleBarTest.TOLERANCE
                && Math.abs(expected.getGreen() - value.getGreen()) <= CustomTitleBarTest.TOLERANCE
                && Math.abs(expected.getBlue() - value.getBlue()) <= CustomTitleBarTest.TOLERANCE;
    }

    private void assertScreenColorEquals(final Color expected, final Color value,
            final String message) {
        if (!checkScreenColor(expected, value)) {
            String failureMessage = "Expected " + expected + ", but got " + value + ". Allowed tolerance is "
                    + CustomTitleBarTest.TOLERANCE + ". " + message;
            Assertions.fail(failureMessage);
        }
    }

    private void assertScreenColorNotEquals(final Color expected, final Color value,
            final String message) {
        if (checkScreenColor(expected, value)) {
            String failureMessage = "Did not expect " + expected + ", but got " + value + ". Allowed tolerance is "
                    + CustomTitleBarTest.TOLERANCE + ". " + message;
            Assertions.fail(failureMessage);
        }
    }

    private void checkImage(final String fileName, final Consumer<BufferedImage> check) {
        checkImage(fileName, f -> {
        }, check);
    }

    private void checkImage(final String fileName, final Consumer<JFrame> frameModifier,
            final Consumer<BufferedImage> check) {
        AtomicReference<JFrame> frame = createFrame(frameModifier);
        TestUtils.runOnSwingThreadNotThrowing(() -> {
            Rectangle rect = frame.get().getBounds();
            rect.setLocation(0, 0);
            check.accept(saveWindowScreenShot(getPath(fileName), frame.get()));
        });
    }

    @Test
    @EnabledOnOs({OS.MAC, OS.WINDOWS})
    void checkTitleBarColored() {
        SwingUtilities.invokeLater(() -> LafManager.install(new IntelliJTheme()));
        UIManager.put("macos.coloredTitleBar", true);
        checkImage("colored_title_" + SystemInfo.getOsName(),
                img -> assertScreenColorEquals(TITLE_BAR_COLOR, new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y)),
                        "Title color not equal."));
    }

    @Test
    @EnabledOnOs(OS.MAC)
    void checkTitleBarNotColored() {
        SwingUtilities.invokeLater(() -> LafManager.install(new IntelliJTheme()));
        UIManager.put("macos.coloredTitleBar", false);
        checkImage("non_colored_title_" + SystemInfo.getOsName(), img -> {
            Color c = new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y));
            assertScreenColorNotEquals(TITLE_BAR_COLOR, c, "Title is colored. Shouldn't be");
            assertScreenColorNotEquals(CONTENT_COLOR, c, "No native titlebar is visible");
        });
    }

    @Test
    @EnabledOnOs(OS.MAC)
    void checkForDarkNativeTitle() {
        SwingUtilities.invokeLater(() -> LafManager.install(new DarculaTheme()));
        UIManager.put("macos.coloredTitleBar", false);
        checkImage("native_title_dark_mac", img -> {
            assertScreenColorNotEquals(CONTENT_COLOR, new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y)),
                    "No native titlebar is visible");
            Color bg = new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y));
            double brightness = ColorUtil.getPerceivedBrightness(bg);
            Assertions.assertTrue(brightness < 80,
                    "Titlebar is not dark. brightness = " + brightness + ". For color " + bg);
        });
    }

    @Test
    @EnabledOnOs(OS.MAC)
    void checkForLightNativeTitle() {
        SwingUtilities.invokeLater(() -> LafManager.install(new IntelliJTheme()));
        UIManager.put("macos.coloredTitleBar", false);
        checkImage("native_title_light_mac", img -> {
            assertScreenColorNotEquals(CONTENT_COLOR, new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y)),
                    "No native titlebar is visible");
            Color bg = new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y));
            double brightness = ColorUtil.getPerceivedBrightness(bg);
            Assertions.assertTrue(brightness > 200,
                    "Titlebar is not light. brightness = " + brightness + ". For color " + bg);
        });
    }

    @Test
    @EnabledOnOs({OS.MAC, OS.WINDOWS})
    void checkTitleBarHidden() {
        TestUtils.runOnSwingThreadNotThrowing(() -> LafManager.install(new IntelliJTheme()));
        UIManager.put("macos.coloredTitleBar", true);
        Assertions.assertTrue(LafManager.isDecorationsEnabled());
        checkImage("title_bar_hidden_" + SystemInfo.getOsName(),
                frame -> {
                    JRootPane rootPane = frame.getRootPane();
                    rootPane.putClientProperty(DarkRootPaneUI.HIDE_TITLEBAR, true);
                },
                img -> assertScreenColorEquals(CONTENT_COLOR, new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y)),
                        "Titlebar isn't hidden"));
    }

    @Test
    @EnabledOnOs(OS.WINDOWS)
    void checkDisableCustomDecoration() {
        SwingUtilities.invokeLater(() -> LafManager.install(new IntelliJTheme()));
        checkImage("native_title_bar_window",
                f -> LafManager.setDecorationsEnabled(false),
                img -> assertScreenColorNotEquals(TITLE_BAR_COLOR,
                        new Color(img.getRGB(img.getWidth() / 2, TITLE_BAR_Y)),
                        "No native titlebar installed"));
    }
}
