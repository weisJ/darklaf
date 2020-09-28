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
package com.github.weisj.darklaf.listener;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import javax.swing.*;

/** @author Jannis Weis */
public class HoverListener implements MouseListener {

    private final JComponent component;
    private boolean hover = false;
    private boolean scheduled = false;

    public HoverListener(final JComponent component) {
        this.component = component;
    }

    public boolean isHover() {
        return hover;
    }

    @Override
    public void mouseClicked(final MouseEvent e) {}

    @Override
    public void mousePressed(final MouseEvent e) {}

    @Override
    public void mouseReleased(final MouseEvent e) {}

    @Override
    public void mouseEntered(final MouseEvent e) {
        if (!hover) {
            hover = true;
            scheduleRepaint();
        }
    }

    private void scheduleRepaint() {
        if (!scheduled) {
            scheduled = true;
            SwingUtilities.invokeLater(() -> {
                component.invalidate();
                component.repaint();
                scheduled = false;
            });
        }
    }

    @Override
    public void mouseExited(final MouseEvent e) {
        if (hover) {
            hover = false;
            scheduleRepaint();
        }
    }

    public void refresh() {
        boolean newHover = false;
        PointerInfo info = MouseInfo.getPointerInfo();
        if (info != null) {
            Point p = info.getLocation();
            SwingUtilities.convertPointFromScreen(p, component);
            newHover = component.contains(p);
        }
        if (newHover != hover) {
            hover = newHover;
            scheduleRepaint();
        }
    }
}
