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
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;

import com.github.weisj.darklaf.graphics.LegacyAnimator;
import com.github.weisj.darklaf.util.graphics.GraphicsContext;
import com.github.weisj.darklaf.util.graphics.GraphicsUtil;

public abstract class DarkRoundedScrollBarUI extends DarkScrollBarUI {

    protected boolean hideScrollBar = false;
    private int minimumSize;

    @Override
    protected void installDefaults() {
        super.installDefaults();
        minimumSize = UIManager.getInt("ScrollBar.minimumWidth");
    }

    protected abstract int getTrackFadeOutDelay();

    @Override
    protected void paintTrack(final Graphics g, final JComponent c, final Rectangle bounds) {}

    @Override
    protected void paintMaxiThumb(final Graphics2D g, final Rectangle rect) {
        GraphicsContext context = GraphicsUtil.setupStrokePainting(g);
        g.setComposite(COMPOSITE.derive(thumbAlpha));
        boolean horizontal = scrollbar.getOrientation() == JScrollBar.HORIZONTAL;
        int ins = 2;
        RoundRectangle2D roundRect = new RoundRectangle2D.Float();
        int width = rect.width - 2 * ins;
        int height = rect.height - 2 * ins;
        int x = rect.x + ins;
        int y = rect.y + ins;
        if (hideScrollBar) {
            float animationState = scrollBarListener.getTrackState();
            if (horizontal) {
                int newHeight = Math.round(minimumSize + (height - minimumSize) * animationState);
                y += height - newHeight;
                height = newHeight;
            } else {
                int newWidth = Math.round(minimumSize + (width - minimumSize) * animationState);
                if (scrollbar.getComponentOrientation().isLeftToRight()) {
                    x += width - newWidth;
                }
                width = newWidth;
            }
        }
        int arc = horizontal ? height : width;
        roundRect.setRoundRect(x, y, width, height, arc, arc);
        if (!roundRect.isEmpty()) {
            g.setColor(getThumbColor());
            g.fill(roundRect);
            g.setColor(getThumbBorderColor());
            g.draw(roundRect);
        }
        context.restore();
    }

    @Override
    protected DarkScrollBarListener<DarkRoundedScrollBarUI> createScrollBarListener() {
        return new RoundedScrollBarListener(scrollbar, this);
    }

    private static class RoundedScrollBarListener extends DarkScrollBarListener<DarkRoundedScrollBarUI> {

        private final Timer hideTimer;

        public RoundedScrollBarListener(final JScrollBar scrollbar, final DarkRoundedScrollBarUI ui) {
            super(scrollbar, ui);
            int hideDelay = getTrackFadeOutDelay();
            hideTimer = new Timer(hideDelay, e -> {
                Point p = MouseInfo.getPointerInfo().getLocation();
                SwingUtilities.convertPointFromScreen(p, scrollbar);
                mouseOverTrack = scrollbar.contains(p);
                resetTrackAnimator();
            });
            hideTimer.setRepeats(false);
        }

        @Override
        protected boolean animateTrackOnScroll(final JScrollBar scrollbar) {
            return ui.hideScrollBar;
        }

        @Override
        protected LegacyAnimator createTrackFadeinAnimator() {
            return ui.hideScrollBar ? super.createTrackFadeinAnimator() : null;
        }

        @Override
        protected LegacyAnimator createTrackFadeoutAnimator() {
            return ui.hideScrollBar ? super.createTrackFadeoutAnimator() : null;
        }

        @Override
        protected int getTrackFadeOutDelay() {
            return ui.getTrackFadeOutDelay();
        }

        @Override
        protected void runOnScrollTrackAnimation() {
            super.runOnScrollTrackAnimation();
            hideTimer.stop();
            hideTimer.start();
        }

        @Override
        protected void resetTrackAnimator() {
            hideTimer.stop();
            super.resetTrackAnimator();
        }
    }
}
