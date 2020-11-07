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

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollBarUI;

import com.github.weisj.darklaf.graphics.PaintUtil;
import com.github.weisj.darklaf.util.ColorUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/** @author Jannis Weis */
public class DarkScrollBarUI extends BasicScrollBarUI implements ScrollBarConstants {

    protected static final AlphaComposite COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER);
    protected DarkScrollBarListener scrollBarListener;
    protected Color thumbBorderColor;
    protected Color thumbFadeStartColor;
    protected Color thumbFadeEndColor;
    protected Color trackBackground;
    protected int smallSize;
    protected int size;
    protected float thumbAlpha;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkScrollBarUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        scrollbar.setFocusable(false);
    }

    @Override
    protected void installDefaults() {
        final int incGap = UIManager.getInt("ScrollBar.incrementButtonGap");
        final int decGap = UIManager.getInt("ScrollBar.decrementButtonGap");
        try {
            UIManager.put("ScrollBar.incrementButtonGap", 0);
            UIManager.put("ScrollBar.decrementButtonGap", 0);
            super.installDefaults();
        } finally {
            UIManager.put("ScrollBar.incrementButtonGap", incGap);
            UIManager.put("ScrollBar.decrementButtonGap", decGap);
        }
        PropertyUtil.installProperty(scrollbar, KEY_HIGHLIGHT_ON_SCROLL,
                UIManager.getBoolean("ScrollBar.highlightOnScroll"));
        thumbBorderColor = UIManager.getColor("ScrollBar.thumbBorderColor");
        thumbFadeStartColor = UIManager.getColor("ScrollBar.fadeStartColor");
        thumbFadeEndColor = UIManager.getColor("ScrollBar.fadeEndColor");
        trackBackground = UIManager.getColor("ScrollBar.trackColor");
        smallSize = UIManager.getInt("ScrollBar.smallWidth");
        size = UIManager.getInt("ScrollBar.width");
        thumbAlpha = UIManager.getInt("ScrollBar.thumbAlpha") / 100.0f;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        scrollBarListener = createScrollBarListener();
        scrollbar.addAdjustmentListener(scrollBarListener);
        scrollbar.addMouseMotionListener(scrollBarListener);
        scrollbar.addMouseListener(scrollBarListener);
        scrollbar.addMouseWheelListener(scrollBarListener);
    }

    protected DarkScrollBarListener createScrollBarListener() {
        return new DarkScrollBarListener(scrollbar, this);
    }

    @Override
    protected ModelListener createModelListener() {
        return new ModelListener() {
            @Override
            public void stateChanged(final ChangeEvent e) {
                if (scrollbar != null) {
                    super.stateChanged(e);
                }
            }
        };
    }

    @Override
    protected JButton createDecreaseButton(final int orientation) {
        return new EmptyButton();
    }

    @Override
    protected JButton createIncreaseButton(final int orientation) {
        return new EmptyButton();
    }

    @Override
    protected void uninstallListeners() {
        if (scrollTimer != null) {
            // it is already called otherwise
            super.uninstallListeners();
        }
        scrollbar.removeMouseListener(scrollBarListener);
        scrollbar.removeMouseWheelListener(scrollBarListener);
        scrollbar.removeMouseMotionListener(scrollBarListener);
        scrollbar.removeAdjustmentListener(scrollBarListener);
        scrollBarListener.uninstall();
        scrollBarListener = null;
    }

    protected void paintTrack(final Graphics g, final JComponent c, final Rectangle bounds) {
        if (c.isOpaque()) {
            g.setColor(scrollbar.getBackground());
            g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
        }
        float trackAlpha = scrollBarListener.getTrackAlpha();
        if (trackAlpha == 0) return;
        g.setColor(getTrackColor());
        ((Graphics2D) g).setComposite(COMPOSITE.derive(trackAlpha));
        g.fillRect(bounds.x, bounds.y, bounds.width, bounds.height);
    }

    protected void paintThumb(final Graphics g, final JComponent c, final Rectangle thumbBounds) {
        if (!thumbBounds.isEmpty() && scrollbar.isEnabled()) {
            paintMaxiThumb((Graphics2D) g, thumbBounds);
        }
    }

    protected void paintMaxiThumb(final Graphics2D g, final Rectangle rect) {
        g.setComposite(COMPOSITE.derive(thumbAlpha));
        g.setColor(getThumbBorderColor());
        PaintUtil.drawRect(g, rect.x, rect.y, rect.width, rect.height, 1);
        g.setColor(getThumbColor());
        PaintUtil.fillRect(g, rect.x + 1, rect.y + 1, rect.width - 2, rect.height - 2);
    }

    protected Color getThumbBorderColor() {
        double percent = Math.min(1.0, Math.max(0.0, 1 - (scrollBarListener.getThumbAlpha() - thumbAlpha)));
        return ColorUtil.blendColors(thumbBorderColor, thumbColor, percent);
    }

    protected Color getThumbColor() {
        return ColorUtil.blendColors(thumbFadeEndColor, thumbFadeStartColor, scrollBarListener.getThumbAlpha());
    }

    @Override
    public boolean getSupportsAbsolutePositioning() {
        return true;
    }

    protected Color getTrackColor() {
        return trackBackground;
    }

    protected int getThickness() {
        return ScrollBarConstants.isSmall(scrollbar) ? smallSize : size;
    }

    /*
     * Widen visibility
     */
    @Override
    public Rectangle getTrackBounds() {
        return super.getTrackBounds();
    }

    /*
     * Widen visibility
     */
    @Override
    public Rectangle getThumbBounds() {
        return super.getThumbBounds();
    }

    @Override
    public Dimension getPreferredSize(final JComponent c) {
        return getMinimumSize(c);
    }

    @Override
    public Dimension getMaximumSize(final JComponent c) {
        return getMinimumSize(c);
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        return getMinimumThumbSize();
    }

    @Override
    protected Dimension getMinimumThumbSize() {
        int size = getThickness();
        return new Dimension(size, size);
    }

    @Override
    public void layoutContainer(final Container scrollbarContainer) {
        try {
            super.layoutContainer(scrollbarContainer);
        } catch (NullPointerException ignore) {
            // installUI is not performed yet or uninstallUI has set almost every field to null.
        }
    }
}
