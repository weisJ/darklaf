package com.weis.darklaf.ui.scrollpane;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicScrollPaneUI;
import java.awt.*;
import java.awt.event.MouseWheelListener;
import java.beans.PropertyChangeListener;

public class DarkScrollPaneUI extends BasicScrollPaneUI {

    private final MouseWheelListener verticalMouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled()
                || e.getModifiersEx() == 0 || !horizontalScrollBarEnabled()) {
            return;
        }
        var scrollbar = scrollpane.getHorizontalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        DarkScrollBarUI.doScroll(scrollbar, scrollpane.getViewport(), e,
                                 scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final MouseWheelListener horizontalMouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled() || e.isShiftDown() || !verticalScrollBarEnabled()) {
            return;
        }
        var scrollbar = scrollpane.getVerticalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        DarkScrollBarUI.doScroll(scrollbar, scrollpane.getViewport(), e,
                                 scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final MouseWheelListener mouseWheelListener = e -> {
        if (!scrollpane.isWheelScrollingEnabled()) {
            return;
        }
        var scrollbar = e.isShiftDown() ? scrollpane.getHorizontalScrollBar()
                                        : scrollpane.getVerticalScrollBar();
        scrollbar.setValueIsAdjusting(true);
        DarkScrollBarUI.doScroll(scrollbar, scrollpane.getViewport(), e,
                                 scrollpane.getComponentOrientation().isLeftToRight());
        scrollbar.setValueIsAdjusting(false);
    };
    private final PropertyChangeListener propertyChangeListener = e -> {
        if (e.getSource() == scrollpane) {
            var propertyName = e.getPropertyName();
            if ("verticalScrollBar".equals(propertyName)) {
                var old = e.getOldValue();
                var newVal = e.getNewValue();
                if (old instanceof JScrollBar) {
                    ((JScrollBar) old).removeMouseWheelListener(verticalMouseWheelListener);
                }
                if (newVal instanceof JScrollBar) {
                    ((JScrollBar) newVal).addMouseWheelListener(verticalMouseWheelListener);
                }
            } else if ("horizontalScrollBar".equals(propertyName)) {
                var old = e.getOldValue();
                var newVal = e.getNewValue();
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
        var sb = scrollpane.getHorizontalScrollBar();
        if (sb == null) {
            return false;
        }
        return sb.isVisible();
    }

    private boolean verticalScrollBarEnabled() {
        var sb = scrollpane.getVerticalScrollBar();
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
                    var vsb = scrollpane.getVerticalScrollBar();
                    var hsb = scrollpane.getHorizontalScrollBar();
                    var lowerRight = scrollpane.getCorner(ScrollPaneConstants.LOWER_RIGHT_CORNER);
                    var lowerLeft = scrollpane.getCorner(ScrollPaneConstants.LOWER_LEFT_CORNER);
                    var vertBounds = vsb.getBounds();
                    var horBounds = hsb.getBounds();
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
