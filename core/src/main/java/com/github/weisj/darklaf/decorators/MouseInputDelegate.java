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
package com.github.weisj.darklaf.decorators;

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;

import javax.swing.event.MouseInputListener;

public class MouseInputDelegate implements MouseInputListener {

    protected MouseListener mouseDelegate;
    protected MouseMotionListener motionDelegate;

    public MouseInputDelegate(final MouseListener mouseListener) {
        this(mouseListener, null);
    }

    public MouseInputDelegate(final MouseMotionListener motionDelegate) {
        this(null, motionDelegate);
    }

    public MouseInputDelegate(final MouseListener mouseListener, final MouseMotionListener motionDelegate) {
        setMotionDelegate(motionDelegate);
        setMouseDelegate(mouseListener);
    }

    public MouseInputDelegate(final MouseInputListener delegate) {
        this(delegate, delegate);
    }

    public MouseListener getMouseDelegate() {
        return mouseDelegate;
    }

    public MouseMotionListener getMotionDelegate() {
        return motionDelegate;
    }

    public void setMotionDelegate(final MouseMotionListener motionDelegate) {
        this.motionDelegate = motionDelegate;
        if (motionDelegate == null) {
            this.motionDelegate = new MouseAdapter() {};
        }
    }

    public void setMouseDelegate(final MouseListener delegate) {
        this.mouseDelegate = delegate;
        if (delegate == null) {
            this.mouseDelegate = new MouseAdapter() {};
        }
    }

    @Override
    public void mouseClicked(final MouseEvent e) {
        getMouseDelegate().mouseClicked(e);
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        getMouseDelegate().mousePressed(e);
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        getMouseDelegate().mousePressed(e);
    }

    @Override
    public void mouseEntered(final MouseEvent e) {
        getMouseDelegate().mouseEntered(e);
    }

    @Override
    public void mouseExited(final MouseEvent e) {
        getMouseDelegate().mouseExited(e);
    }

    @Override
    public void mouseDragged(final MouseEvent e) {
        getMotionDelegate().mouseDragged(e);
    }

    @Override
    public void mouseMoved(final MouseEvent e) {
        getMotionDelegate().mouseMoved(e);
    }
}
