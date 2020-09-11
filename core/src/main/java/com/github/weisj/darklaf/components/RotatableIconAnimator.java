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
package com.github.weisj.darklaf.components;

import java.awt.event.ActionEvent;

import javax.swing.*;

import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.util.Alignment;

public class RotatableIconAnimator extends Timer {

    private final RotatableIcon icon;
    private final JComponent parent;
    private final int frameCount;
    private int frame;

    public RotatableIconAnimator(final RotatableIcon icon, final JComponent parent) {
        this(Alignment.values().length, icon, parent);
    }

    public RotatableIconAnimator(final int frames, final RotatableIcon icon, final JComponent parent) {
        super(100, null);
        if (icon == null) throw new IllegalArgumentException("Icon is null");
        if (parent == null) throw new IllegalArgumentException("Component is null");
        addActionListener(this::onAction);
        setRepeats(true);
        this.icon = icon;
        this.frameCount = frames;
        this.parent = parent;
    }

    public void resume() {
        start();
    }

    public void onAction(final ActionEvent e) {
        icon.setRotation(Math.PI * 2 * (((double) frame) / frameCount));
        parent.repaint();
        frame = (frame + 1) % frameCount;
    }

    public void suspend() {
        stop();
    }
}
