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
 */
package com.github.weisj.darklaf;



import java.awt.AlphaComposite;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Window;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.swing.JComponent;
import javax.swing.JLayeredPane;
import javax.swing.RootPaneContainer;

import com.github.weisj.darklaf.graphics.Animator;
import com.github.weisj.darklaf.graphics.DefaultInterpolator;
import com.github.weisj.darklaf.util.ImageUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.graphics.ImagePainter;
import com.github.weisj.darklaf.util.value.SharedNonNull;

class LafTransition {

    private static final String ANIMATED_LAF_CHANGE = DarkLaf.SYSTEM_PROPERTY_PREFIX + "animatedLafChange";

    private LafTransition() {}

    static LafTransition showSnapshot() {
        return PropertyUtil.getSystemFlag(ANIMATED_LAF_CHANGE)
                ? new AnimatedLafTransition()
                : new LafTransition();
    }

    void runTransition() {
        // Do nothing.
    }

    static final class AnimatedLafTransition extends LafTransition {

        private final Animator animator;
        private final Map<JLayeredPane, Component> uiSnapshots;
        private final SharedNonNull<Float> sharedAlpha;

        private AnimatedLafTransition() {
            sharedAlpha = new SharedNonNull<>(1f);
            animator = new TransitionAnimator();
            uiSnapshots = new LinkedHashMap<>();
            Window[] windows = Window.getWindows();
            if (windows.length == 0) {
                animator.setEnabled(false);
                return;
            }
            for (Window window : windows) {
                if (window instanceof RootPaneContainer && window.isShowing()) {
                    RootPaneContainer rootPaneContainer = (RootPaneContainer) window;
                    Image img = ImageUtil.scaledImageFromComponent(rootPaneContainer.getRootPane());
                    JLayeredPane layeredPane = rootPaneContainer.getLayeredPane();
                    JComponent imageLayer = new ImageLayer(layeredPane, img, sharedAlpha);
                    imageLayer.setSize(layeredPane.getSize());
                    layeredPane.add(imageLayer, JLayeredPane.DRAG_LAYER);
                    uiSnapshots.put(layeredPane, imageLayer);
                }
            }
            doPaint();
        }

        @Override
        void runTransition() {
            animator.play();
        }

        private void disposeSnapshots() {
            for (Map.Entry<JLayeredPane, Component> entry : uiSnapshots.entrySet()) {
                entry.getKey().remove(entry.getValue());
                entry.getKey().revalidate();
                entry.getKey().repaint();
            }
            uiSnapshots.clear();
        }

        private void doPaint() {
            for (Map.Entry<JLayeredPane, Component> entry : uiSnapshots.entrySet()) {
                if (entry.getKey().isShowing()) {
                    entry.getValue().revalidate();
                    entry.getValue().repaint();
                }
            }
        }

        private class TransitionAnimator extends Animator {

            public TransitionAnimator() {
                super(160, DefaultInterpolator.EASE_IN_SINE);
            }

            @Override
            public void paintAnimationFrame(float fraction) {
                sharedAlpha.set(1f - fraction);
                doPaint();
            }

            @Override
            public void play() {
                doPaint();
                super.play();
            }

            @Override
            protected void onAnimationFinished() {
                disposeSnapshots();
            }
        }
    }

    private static class ImageLayer extends JComponent {

        private final JLayeredPane layeredPane;
        private final SharedNonNull<Float> sharedAlpha;
        private final Image image;

        private ImageLayer(final JLayeredPane layeredPane, final Image image, final SharedNonNull<Float> sharedAlpha) {
            this.layeredPane = layeredPane;
            this.image = image;
            this.sharedAlpha = sharedAlpha;
        }

        @Override
        public void updateUI() {}

        @Override
        public void paint(final Graphics g) {
            Graphics gg = g.create();
            ((Graphics2D) gg).setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, sharedAlpha.get()));
            ImagePainter.drawImage(gg, image, 0, 0, this);
        }

        @Override
        public Rectangle getBounds() {
            return layeredPane.getBounds();
        }
    }

}
