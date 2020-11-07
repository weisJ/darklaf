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
package com.github.weisj.darklaf.ui.scrollpane;

import java.awt.*;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.graphics.GraphicsContext;
import com.github.weisj.darklaf.graphics.GraphicsUtil;

public class DarkMacScrollBarUI extends DarkScrollBarUI {

    private boolean hideScrollBar;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkMacScrollBarUI();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        hideScrollBar = UIManager.getBoolean("ScrollBar.macos.hideScrollBar");
    }

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
            if (horizontal) {
                int newHeight = (int) (height * scrollBarListener.getTrackState());
                y += (height - newHeight);
                height = newHeight;
            } else {
                int newWidth = (int) (width * scrollBarListener.getTrackState());
                if (scrollbar.getComponentOrientation().isLeftToRight()) {
                    x += (width - newWidth);
                }
                width = newWidth;
            }
        }
        int arc = horizontal ? height : width;
        roundRect.setRoundRect(x, y, width, height, arc, arc);
        g.setColor(getThumbColor());
        g.fill(roundRect);
        g.setColor(getThumbBorderColor());
        g.draw(roundRect);
        context.restore();
    }

    @Override
    protected DarkScrollBarListener createScrollBarListener() {
        return new MacScrollBarListener(scrollbar, this);
    }

    private static class MacScrollBarListener extends DarkScrollBarListener {


        public MacScrollBarListener(final JScrollBar scrollbar, final DarkScrollBarUI ui) {
            super(scrollbar, ui);
        }

        // @Override
        // protected Animator createTrackFadeinAnimator() {
        // return null;
        // }
        //
        // @Override
        // protected Animator createTrackFadeoutAnimator() {
        // return null;
        // }
    }
}
