/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.misc;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.*;

import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.util.DarkUIUtil;

public class FullscreenDemo extends BaseComponentDemo {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new FullscreenDemo());
    }

    @Override
    public JComponent createComponent() {
        JPanel panel = new JPanel(new GridBagLayout());
        JLabel label = new JLabel("Double click to toggle fullscreen");
        panel.add(label);
        MouseListener listener = new MouseAdapter() {
            private long lastClicked = -1;
            private boolean fullscreen = false;

            @Override
            public void mouseClicked(final MouseEvent e) {
                long time = e.getWhen();
                if (Math.abs(time - lastClicked) < 2000) {
                    Window window = DarkUIUtil.getWindow(panel);
                    if (fullscreen) {
                        window.getGraphicsConfiguration().getDevice().setFullScreenWindow(null);
                    } else {
                        window.getGraphicsConfiguration().getDevice().setFullScreenWindow(window);
                    }
                    fullscreen = !fullscreen;
                    lastClicked = -1;
                } else {
                    lastClicked = time;
                }
            }
        };
        panel.addMouseListener(listener);
        label.addMouseListener(listener);
        return panel;
    }

    @Override
    public String getName() {
        return "Fullscreen Demo";
    }
}
