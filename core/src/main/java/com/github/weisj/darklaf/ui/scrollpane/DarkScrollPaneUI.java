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
import java.awt.event.MouseWheelListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollPaneUI;

import com.github.weisj.darklaf.delegate.ScrollLayoutManagerDelegate;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

/** @author Jannis Weis */
public class DarkScrollPaneUI extends BasicScrollPaneUI implements PropertyChangeListener {

    private final MouseWheelListener verticalMouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled() || e.getModifiersEx() == 0 || !horizontalScrollBarEnabled()) {
            return;
        }
        JScrollBar scrollbar = scrollpane.getHorizontalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        ScrollBarUtil.doScroll(scrollbar, scrollpane.getViewport(), e,
                scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final MouseWheelListener horizontalMouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled() || e.isShiftDown() || !verticalScrollBarEnabled()) {
            return;
        }
        JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        ScrollBarUtil.doScroll(scrollbar, scrollpane.getViewport(), e,
                scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final MouseWheelListener mouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled()) {
            return;
        }
        JScrollBar scrollbar =
                e.isShiftDown() ? scrollpane.getHorizontalScrollBar() : scrollpane.getVerticalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        ScrollBarUtil.doScroll(scrollbar, scrollpane.getViewport(), e,
                scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final PropertyChangeListener scrollbarPropertyChangeListener = e -> {
        if (PropertyKey.UI.equals(e.getPropertyName())) {
            updateScrollBarBackground();
        }
    };
    private ViewPropertyChangeListener viewPropertyChangeListener;
    private ScrollPaneLayout oldLayout;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkScrollPaneUI();
    }

    private boolean horizontalScrollBarEnabled() {
        JScrollBar sb = scrollpane.getHorizontalScrollBar();
        if (sb == null) {
            return false;
        }
        return sb.isVisible();
    }

    private boolean verticalScrollBarEnabled() {
        JScrollBar sb = scrollpane.getVerticalScrollBar();
        if (sb == null) {
            return false;
        }
        return sb.isVisible();
    }

    protected Color getViewBackground() {
        Component view = getViewportView();
        return view != null ? view.getBackground() : null;
    }

    protected Component getViewportView() {
        JViewport viewport = scrollpane.getViewport();
        return viewport != null ? viewport.getView() : null;
    }

    @Override
    protected void installListeners(final JScrollPane c) {
        super.installListeners(c);
        getViewPropertyChangeListener().install();
        scrollpane.addPropertyChangeListener(this);
        scrollpane.getVerticalScrollBar().addMouseWheelListener(verticalMouseWheelListener);
        scrollpane.getVerticalScrollBar().addPropertyChangeListener(scrollbarPropertyChangeListener);
        scrollpane.getHorizontalScrollBar().addMouseWheelListener(horizontalMouseWheelListener);
        scrollpane.getHorizontalScrollBar().addPropertyChangeListener(scrollbarPropertyChangeListener);
    }

    protected ViewPropertyChangeListener getViewPropertyChangeListener() {
        if (viewPropertyChangeListener == null) {
            viewPropertyChangeListener = createViewPropertyChangeListener();
        }
        return viewPropertyChangeListener;
    }

    protected ViewPropertyChangeListener createViewPropertyChangeListener() {
        return new ViewPropertyChangeListener(scrollpane, e -> {
            if (PropertyKey.BACKGROUND.equals(e.getPropertyName())) {
                updateScrollBarBackground();
            }
        });
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        if (e.getSource() == scrollpane) {
            String propertyName = e.getPropertyName();
            if ("verticalScrollBar".equals(propertyName)) {
                transferListeners(e, verticalMouseWheelListener);
            } else if ("horizontalScrollBar".equals(propertyName)) {
                transferListeners(e, horizontalMouseWheelListener);
            }
        }
    }

    public void transferListeners(final PropertyChangeEvent e, final MouseWheelListener mouseWheelListener) {
        Object old = e.getOldValue();
        Object newVal = e.getNewValue();
        if (old instanceof JScrollBar) {
            ((JScrollBar) old).removeMouseWheelListener(mouseWheelListener);
            ((JScrollBar) old).removePropertyChangeListener(scrollbarPropertyChangeListener);
        }
        if (newVal instanceof JScrollBar) {
            ((JScrollBar) newVal).addMouseWheelListener(mouseWheelListener);
            ((JScrollBar) newVal).addPropertyChangeListener(scrollbarPropertyChangeListener);
            PropertyUtil.installBackground((Component) newVal, getViewBackground());
        }
    }

    protected void updateScrollBarBackground() {
        Color bg = getViewBackground();
        PropertyUtil.installBackground(scrollpane.getVerticalScrollBar(), bg);
        PropertyUtil.installBackground(scrollpane.getHorizontalScrollBar(), bg);
    }

    @Override
    public void installUI(final JComponent x) {
        super.installUI(x);
        oldLayout = (ScrollPaneLayout) x.getLayout();
        while (oldLayout instanceof ScrollLayoutManagerDelegate) {
            oldLayout = ((ScrollLayoutManagerDelegate) oldLayout).getDelegate();
        }
        if (oldLayout != null) {
            x.setLayout(new ScrollLayoutManagerDelegate(oldLayout) {

                @Override
                public void layoutContainer(final Container parent) {
                    super.layoutContainer(parent);
                    JScrollBar vsb = scrollpane.getVerticalScrollBar();
                    if (vsb == null) {
                        vsb = scrollpane.createHorizontalScrollBar();
                        scrollpane.setHorizontalScrollBar(vsb);
                    }
                    JScrollBar hsb = scrollpane.getHorizontalScrollBar();
                    if (hsb == null) {
                        hsb = scrollpane.createHorizontalScrollBar();
                        scrollpane.setHorizontalScrollBar(hsb);
                    }
                    Component lowerRight = scrollpane.getCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER);
                    Component lowerLeft = scrollpane.getCorner(ScrollPaneConstants.LOWER_LEFT_CORNER);
                    Rectangle vertBounds = vsb.getBounds();
                    Rectangle horBounds = hsb.getBounds();
                    if (parent.getComponentOrientation().isLeftToRight()) {
                        if (lowerRight == null && hsb.isVisible()) {
                            vertBounds.height += horBounds.height;
                        }
                    } else {
                        if (lowerLeft == null && hsb.isVisible()) {
                            vertBounds.height += horBounds.height;
                        }
                    }
                    Insets barInsets = UIManager.getInsets("ScrollPane.barInsets");
                    if (barInsets != null) {
                        vertBounds.height -= barInsets.top + barInsets.bottom;
                        vertBounds.y += barInsets.top;
                        horBounds.width -= barInsets.left + barInsets.right;
                        horBounds.x += barInsets.left;
                    }
                    vsb.setBounds(vertBounds);
                    hsb.setBounds(horBounds);
                }
            });
        }
    }

    @Override
    protected void installDefaults(final JScrollPane scrollpane) {
        super.installDefaults(scrollpane);
        updateScrollBarBackground();
    }

    @Override
    protected void uninstallListeners(final JComponent c) {
        super.uninstallListeners(c);
        getViewPropertyChangeListener().uninstall();
        scrollpane.removePropertyChangeListener(this);
        scrollpane.getVerticalScrollBar().removeMouseWheelListener(verticalMouseWheelListener);
        scrollpane.getVerticalScrollBar().removePropertyChangeListener(scrollbarPropertyChangeListener);
        scrollpane.getHorizontalScrollBar().removeMouseWheelListener(horizontalMouseWheelListener);
        scrollpane.getHorizontalScrollBar().removePropertyChangeListener(scrollbarPropertyChangeListener);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        c.setLayout(oldLayout);
    }

    @Override
    protected MouseWheelListener createMouseWheelListener() {
        return mouseWheelListener;
    }
}
