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
package com.github.weisj.darklaf.components.tooltip;

import com.github.weisj.darklaf.ui.tooltip.DarkTooltipBorder;
import com.github.weisj.darklaf.ui.tooltip.DarkTooltipUI;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.Animator;
import com.github.weisj.darklaf.util.GraphicsContext;

import javax.swing.*;
import javax.swing.border.Border;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Objects;

public class DarkToolTip extends JToolTip implements PropertyChangeListener {

    private static final AlphaComposite COMPOSITE = AlphaComposite.getInstance(AlphaComposite.SRC_OVER);
    private static final float MAX_ALPHA = 1.0f;
    private final Animator fadeAnimator;
    private float alpha = 0;

    public DarkToolTip(final Alignment alignment) {
        setAlignment(alignment);
        setOpaque(false);
        fadeAnimator = new FadeInAnimator();
        addPropertyChangeListener(this);
    }

    @Override
    public void addNotify() {
        alpha = 0;
        setVisible(true);
        notifyToolTipListeners(ToolTipEvent.SHOWN);
        fadeAnimator.reset();
        fadeAnimator.resume();
        super.addNotify();
    }

    public void setAlignment(final Alignment alignment) {
        putClientProperty(DarkTooltipUI.KEY_POINTER_LOCATION, alignment);
    }

    public void notifyToolTipListeners(final ToolTipEvent event) {
        for (ToolTipListener listener : listenerList.getListeners(ToolTipListener.class)) {
            if (listener != null) {
                switch (event) {
                    case TEXT:
                        listener.textChanged(this);
                        break;
                    case SHOWN:
                        listener.toolTipShown(this);
                        break;
                    case HIDDEN:
                        listener.toolTipHidden(this);
                        break;
                }
            }
        }
    }

    public void setInsets(final Insets insets) {
        putClientProperty(DarkTooltipUI.KEY_INSETS, insets);
    }

    public void addToolTipListener(final ToolTipListener listener) {
        listenerList.add(ToolTipListener.class, listener);
    }

    public void removeToolTipListener(final ToolTipListener listener) {
        listenerList.remove(ToolTipListener.class, listener);
    }

    @Override
    public void paint(final Graphics g) {
        if (alpha == 0) return;
        GraphicsContext config = new GraphicsContext(g);
        if (alpha != MAX_ALPHA) {
            ((Graphics2D) g).setComposite(COMPOSITE.derive(alpha));
        }
        super.paint(g);
        config.restore();
    }

    @Override
    public void setBorder(final Border border) {
        if (!(border instanceof DarkTooltipBorder)) return;
        super.setBorder(border);
    }

    @Override
    public void removeNotify() {
        super.removeNotify();
        notifyToolTipListeners(ToolTipEvent.HIDDEN);
        alpha = 0;
    }

    @Override
    public String getTipText() {
        String text = super.getTipText();
        if (text == null && getComponent() != null) {
            return getComponent().getToolTipText();
        }
        return text;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        if (DarkTooltipUI.TIP_TEXT_PROPERTY.equals(evt.getPropertyName())) {
            setPreferredSize(getUI().getPreferredSize(this));
            if (!Objects.equals(evt.getNewValue(), evt.getOldValue())) {
                notifyToolTipListeners(ToolTipEvent.TEXT);
            }
        }
    }

    public void setStyle(final ToolTipStyle style) {
        putClientProperty(DarkTooltipUI.KEY_STYLE, style);
    }

    private enum ToolTipEvent {
        TEXT,
        SHOWN,
        HIDDEN
    }

    protected class FadeInAnimator extends Animator {
        private static final int DELAY_FRAMES = 6;
        private static final int FADEIN_FRAMES_COUNT = DELAY_FRAMES + 10;


        public FadeInAnimator() {
            super("Tooltip fadein", FADEIN_FRAMES_COUNT, FADEIN_FRAMES_COUNT * 15, false);
        }

        @Override
        public void paintNow(final int frame, final int totalFrames, final int cycle) {
            alpha = ((float) frame * MAX_ALPHA) / totalFrames;
            paintImmediately(0, 0, getWidth(), getHeight());
        }

        @Override
        protected void paintCycleEnd() {
            alpha = MAX_ALPHA;
            paintImmediately(0, 0, getWidth(), getHeight());
        }
    }
}
