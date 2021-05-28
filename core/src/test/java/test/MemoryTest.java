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

import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.WindowEvent;
import java.lang.ref.WeakReference;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.JTextPane;
import javax.swing.SwingUtilities;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Timeout;

import com.github.weisj.darklaf.LafManager;
import com.github.weisj.darklaf.theme.DarculaTheme;

@Timeout(value = 5)
class MemoryTest {

    @Test
    void frameGetsGarbageCollectedSimpleContent() {
        testWithContentPane(JPanel::new);
    }

    @Test
    void frameGetsGarbageCollectedComplexContent() {
        testWithContentPane(() -> {
            FlowLayout layout = new FlowLayout();
            layout.setAlignOnBaseline(true);
            JPanel panel = new JPanel(layout);
            panel.add(new JButton("Test"));
            panel.add(new JTextArea("Test"));
            panel.add(new JTextField("Test"));
            panel.add(new JTextPane() {
                {
                    setText("Test");
                }
            });
            panel.add(new JEditorPane() {
                {
                    setText("Test");
                }
            });
            return panel;
        });
    }

    private void testWithContentPane(final Supplier<JComponent> content) {
        AtomicReference<JFrame> frame = new AtomicReference<>();
        TestUtils.runOnSwingThreadNotThrowing(() -> {
            LafManager.install(new DarculaTheme());
            JFrame f = new JFrame();
            f.setContentPane(content.get());
            f.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
            f.setVisible(true);
            frame.set(f);
        });
        WeakReference<JFrame> ref = new WeakReference<>(frame.get());
        SwingUtilities.invokeLater(() -> {
            closeWindow(frame.get());
            frame.set(null);
        });
        waitForGarbageCollection(ref);
    }

    private void closeWindow(final Window window) {
        window.dispatchEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSING));
        window.dispose();
    }

    private void waitForGarbageCollection(WeakReference<?> ref) {
        Object[] objects;
        while (ref.get() != null) {
            // Allocate a bunch of objects to force GC.
            int count = 10000;
            objects = new Object[count];
            for (int i = 0; i < count; i++) {
                objects[i] = objects;
            }
            try {
                // noinspection BusyWait
                Thread.sleep(100);
            } catch (InterruptedException ignored) {
            }
            System.gc();
        }
    }
}
