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
 */
package demo.splitPane;

import com.github.weisj.darklaf.LafManager;

import javax.swing.*;
import java.awt.*;

public final class SplitPaneDemo {

    public static void main(final String[] args) {
        //Todo Rework Demo
        SwingUtilities.invokeLater(() -> {
            LafManager.install();
            final JFrame frame = new JFrame();
            frame.setSize(500, 500);
            JSplitPane splitPane = new JSplitPane() {
            };
            splitPane.setLeftComponent(new JPanel() {{
                setBackground(Color.RED);
            }});
            splitPane.setRightComponent(new JPanel() {{
                setBackground(Color.BLUE);
            }});
            splitPane.putClientProperty("JSplitPane.style", "invisible");
            splitPane.putClientProperty("JSplitPane.style", "line");
            splitPane.setOneTouchExpandable(true);
            frame.setContentPane(new JPanel(new BorderLayout()) {{
                add(splitPane, BorderLayout.CENTER);
            }});
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        });
    }
}
