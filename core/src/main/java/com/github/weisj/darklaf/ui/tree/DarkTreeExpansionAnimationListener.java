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
package com.github.weisj.darklaf.ui.tree;

import java.awt.*;

import javax.swing.*;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.tree.TreePath;

import com.github.weisj.darklaf.graphics.Animator;

public class DarkTreeExpansionAnimationListener implements TreeExpansionListener {

    private final JTree tree;
    private final TreeStateAnimator animator;

    public DarkTreeExpansionAnimationListener(final JTree tree) {
        this.tree = tree;
        this.animator = new TreeStateAnimator();
        animator.setEnabled(UIManager.getBoolean("Tree.iconAnimations"));
        tree.addTreeExpansionListener(this);
    }

    public void install() {
        this.tree.addTreeExpansionListener(this);
    }

    public void uninstall() {
        animator.stop();
        this.tree.removeTreeExpansionListener(this);
    }

    @Override
    public void treeExpanded(final TreeExpansionEvent event) {
        startAnimation(event.getPath());
    }

    @Override
    public void treeCollapsed(final TreeExpansionEvent event) {
        startAnimation(event.getPath());
    }

    public void startAnimation(final TreePath path) {
        if (!animator.isEnabled()) return;
        animator.state = 0;
        animator.path = path;
        animator.animationRow = tree.getRowForPath(path);
        animator.stop();
        animator.resume(tree);
    }

    public TreePath getAnimationPath() {
        return animator.path;
    }

    public float getAnimationState() {
        return animator.state;
    }

    protected class TreeStateAnimator extends Animator {

        private TreePath path;
        private float state;
        private int animationRow;

        public TreeStateAnimator() {
            super(60);
        }

        private void repaint() {
            if (animationRow >= 0) {
                Rectangle bounds = tree.getRowBounds(animationRow);
                bounds.x = 0;
                bounds.width = tree.getWidth();
                tree.paintImmediately(bounds);
            }
        }

        @Override
        public void paintAnimationFrame(float fraction) {
            state = fraction;
            repaint();
        }

        @Override
        protected void onAnimationFinished() {
            state = 1;
            repaint();
            animationRow = -1;
        }
    }
}
