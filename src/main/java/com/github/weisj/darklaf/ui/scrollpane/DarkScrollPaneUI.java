/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.github.weisj.darklaf.ui.scrollpane;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollPaneUI;
import java.awt.*;
import java.awt.event.MouseWheelListener;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkScrollPaneUI extends BasicScrollPaneUI {

    private final MouseWheelListener verticalMouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled()
                || e.getModifiersEx() == 0 || !horizontalScrollBarEnabled()) {
            return;
        }
        JScrollBar scrollbar = scrollpane.getHorizontalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        DarkScrollBarUI.doScroll(scrollbar, scrollpane.getViewport(), e,
                                 scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final MouseWheelListener horizontalMouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled() || e.isShiftDown() || !verticalScrollBarEnabled()) {
            return;
        }
        JScrollBar scrollbar = scrollpane.getVerticalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        DarkScrollBarUI.doScroll(scrollbar, scrollpane.getViewport(), e,
                                 scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final MouseWheelListener mouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled()) {
            return;
        }
        JScrollBar scrollbar = e.isShiftDown() ? scrollpane.getHorizontalScrollBar()
                                               : scrollpane.getVerticalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        DarkScrollBarUI.doScroll(scrollbar, scrollpane.getViewport(), e,
                                 scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final PropertyChangeListener propertyChangeListener = e -> {
        if (e.getSource() == scrollpane) {
            String propertyName = e.getPropertyName();
            if ("verticalScrollBar".equals(propertyName)) {
                Object old = e.getOldValue();
                Object newVal = e.getNewValue();
                if (old instanceof JScrollBar) {
                    ((JScrollBar) old).removeMouseWheelListener(verticalMouseWheelListener);
                }
                if (newVal instanceof JScrollBar) {
                    ((JScrollBar) newVal).addMouseWheelListener(verticalMouseWheelListener);
                }
            } else if ("horizontalScrollBar".equals(propertyName)) {
                Object old = e.getOldValue();
                Object newVal = e.getNewValue();
                if (old instanceof JScrollBar) {
                    ((JScrollBar) old).removeMouseWheelListener(horizontalMouseWheelListener);
                }
                if (newVal instanceof JScrollBar) {
                    ((JScrollBar) newVal).addMouseWheelListener(horizontalMouseWheelListener);
                }
            }
        }
    };
    private ScrollPaneLayout oldLayout;

    @NotNull
    @Contract("_ -> new")
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

    @Override
    protected void installListeners(final JScrollPane c) {
        super.installListeners(c);
        scrollpane.addPropertyChangeListener(propertyChangeListener);
        scrollpane.getVerticalScrollBar().addMouseWheelListener(verticalMouseWheelListener);
        scrollpane.getHorizontalScrollBar().addMouseWheelListener(horizontalMouseWheelListener);
    }

    @Override
    public void installUI(final JComponent x) {
        super.installUI(x);
        oldLayout = (ScrollPaneLayout) x.getLayout();
        if (oldLayout != null) {
            x.setLayout(new ScrollLayoutManagerDelegate(oldLayout) {
                @Override
                public void layoutContainer(final Container parent) {
                    super.layoutContainer(parent);
                    JScrollBar vsb = scrollpane.getVerticalScrollBar();
                    JScrollBar hsb = scrollpane.getHorizontalScrollBar();
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
    protected void uninstallListeners(final JComponent c) {
        super.uninstallListeners(c);
        scrollpane.addPropertyChangeListener(propertyChangeListener);
        scrollpane.getVerticalScrollBar().removeMouseWheelListener(verticalMouseWheelListener);
        scrollpane.getHorizontalScrollBar().removeMouseWheelListener(horizontalMouseWheelListener);
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
