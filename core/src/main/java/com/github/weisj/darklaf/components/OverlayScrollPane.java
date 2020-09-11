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
package com.github.weisj.darklaf.components;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;

import com.github.weisj.darklaf.delegate.ScrollLayoutManagerDelegate;
import com.github.weisj.darklaf.ui.scrollpane.ScrollBarConstants;
import com.github.weisj.darklaf.util.PropertyKey;

/**
 * Scroll pane that displays its content beneath the scrollbar.
 *
 * @author Jannis Weis
 */
public class OverlayScrollPane extends JLayeredPane implements PropertyChangeListener {

    protected final JScrollPane scrollPane;
    private final ControlPanel controlPanel;

    /**
     * Creates a <code>OverlayScrollPane</code> that displays the contents of the specified component,
     * where both horizontal and vertical scrollbars appear whenever the component's contents are larger
     * than the view and scrolling in underway or the mouse is over the scrollbar position. The
     * scrollbars appear over the viewport.
     */
    public OverlayScrollPane() {
        this(null);
    }

    /**
     * Creates a <code>OverlayScrollPane</code> that displays the contents of the specified component,
     * where both horizontal and vertical scrollbars appear whenever the component's contents are larger
     * than the view and scrolling in underway or the mouse is over the scrollbar position.
     *
     * @param view the component to display in the scrollable viewport
     * @see        JScrollPane#setViewportView
     */
    public OverlayScrollPane(final JComponent view) {
        this(view, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    /**
     * Creates a <code>OverlayScrollPane</code> that displays the view component in a viewport whose
     * view position can be controlled with a pair of scrollbars. The scrollbars appear over the
     * viewport. The scrollbar policies specify when the scrollbars are displayed, For example, if
     * vsbPolicy is JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED then the vertical scrollbar only appears if
     * the view doesn't fit vertically. The available policy settings are listed at
     * {@link JScrollPane#setVerticalScrollBarPolicy(int)} and
     * {@link JScrollPane#setHorizontalScrollBarPolicy}.
     *
     * @param view      the view of the component.
     * @param vsbPolicy an integer that specifies the vertical scrollbar policy
     * @param hsbPolicy an integer that specifies the horizontal scrollbar policy
     */
    public OverlayScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
        this.scrollPane = createScrollPane(view, vsbPolicy, hsbPolicy);
        setViewportView(view);
        setupScrollPane(scrollPane);
        add(scrollPane, JLayeredPane.DEFAULT_LAYER);

        controlPanel = new ControlPanel(scrollPane);
        add(controlPanel, JLayeredPane.PALETTE_LAYER);
    }

    /**
     * Creates a <code>OverlayScrollPane</code> that displays the view component in a viewport whose
     * view position can be controlled with a pair of scrollbars. The scrollbars appear over the
     * viewport.
     *
     * @param scrollPane the scroll pane to use.
     */
    public OverlayScrollPane(final JScrollPane scrollPane) {
        this.scrollPane = scrollPane;
        setupScrollPane(scrollPane);
        add(scrollPane, JLayeredPane.DEFAULT_LAYER);

        controlPanel = new ControlPanel(scrollPane);
        add(controlPanel, JLayeredPane.PALETTE_LAYER);
    }

    protected JScrollPane createScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
        return new JScrollPane(view, vsbPolicy, hsbPolicy);
    }

    protected void setupScrollPane(final JScrollPane scrollPane) {
        JScrollBar verticalScrollBar = createScrollBar(JScrollBar.VERTICAL);
        verticalScrollBar.putClientProperty(ScrollBarConstants.KEY_SCROLL_PANE_PARENT, scrollPane);
        JScrollBar horizontalScrollBar = createScrollBar(JScrollBar.HORIZONTAL);
        horizontalScrollBar.putClientProperty(ScrollBarConstants.KEY_SCROLL_PANE_PARENT, scrollPane);
        scrollPane.addPropertyChangeListener(this);
        updateScrollPaneUI();
        scrollPane.setVerticalScrollBar(verticalScrollBar);
        scrollPane.setHorizontalScrollBar(horizontalScrollBar);
        scrollPane.setColumnHeader(scrollPane.getColumnHeader());
        scrollPane.setRowHeader(scrollPane.getRowHeader());
    }

    protected PopupScrollBar createScrollBar(final int orientation) {
        return new PopupScrollBar(orientation);
    }

    /**
     * Returns the scroll pane used by this scroll indicator. Use carefully (e.g. to set unit
     * increments) because not all changes have an effect. You have to write listeners in this cases
     * (e.g. for changing the scrollbar policy)
     *
     * @return the scrollPane
     */
    public JScrollPane getScrollPane() {
        return scrollPane;
    }

    @Override
    public void doLayout() {
        scrollPane.setSize(getSize());
        controlPanel.setSize(getSize());
        scrollPane.doLayout();
    }

    public void setVerticalScrollBarPolicy(final int policy) {
        scrollPane.setVerticalScrollBarPolicy(policy);
        controlPanel.showVerticalScrollBar(policy != JScrollPane.VERTICAL_SCROLLBAR_NEVER);
    }

    public void setHorizontalScrollBarPolicy(final int policy) {
        scrollPane.setHorizontalScrollBarPolicy(policy);
        controlPanel.showHorizontalScrollBar(policy != JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
    }

    public JScrollBar getVerticalScrollBar() {
        return scrollPane.getVerticalScrollBar();
    }

    @Override
    public Dimension getPreferredSize() {
        return scrollPane.getPreferredSize();
    }

    public JScrollBar getHorizontalScrollBar() {
        return scrollPane.getHorizontalScrollBar();
    }

    public void setViewportView(final Component c) {
        scrollPane.setViewportView(c);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (PropertyKey.UI.equals(key)) {
            updateScrollPaneUI();
        }
    }

    protected void updateScrollPaneUI() {
        if (scrollPane == null) return;
        scrollPane.setLayout(new ScrollLayoutManagerDelegate((ScrollPaneLayout) scrollPane.getLayout()) {

            @Override
            public void removeLayoutComponent(final Component comp) {
                if (comp == scrollPane.getVerticalScrollBar() || comp == scrollPane.getHorizontalScrollBar()) {
                    return;
                }
                super.removeLayoutComponent(comp);
            }

            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                viewport = getViewport();
                JScrollBar verticalScrollBar = scrollPane.getVerticalScrollBar();
                JScrollBar horizontalScrollBar = scrollPane.getHorizontalScrollBar();
                if (viewport != null) {
                    Rectangle bounds = viewport.getBounds();
                    Rectangle vertBounds = verticalScrollBar.getBounds();
                    Rectangle horBounds = horizontalScrollBar.getBounds();
                    JViewport columnHeader = getColumnHeader();
                    JViewport rowHeader = getRowHeader();
                    if (getComponentOrientation().isLeftToRight()) {
                        if (verticalScrollBar.isVisible()) {
                            bounds.width += vertBounds.width;
                        }
                        if (columnHeader != null && verticalScrollBar.isVisible()) {
                            Rectangle chb = columnHeader.getBounds();
                            chb.width += vertBounds.width;
                            columnHeader.setBounds(chb);
                        }
                        if (rowHeader != null && horizontalScrollBar.isVisible()) {
                            Rectangle rhb = rowHeader.getBounds();
                            rhb.height += horBounds.height;
                            rowHeader.setBounds(rhb);
                        }
                    } else {
                        if (verticalScrollBar.isVisible()) {
                            bounds.x -= vertBounds.width;
                            bounds.width += vertBounds.width;
                        }
                        if (columnHeader != null && verticalScrollBar.isVisible()) {
                            Rectangle chb = columnHeader.getBounds();
                            chb.x -= vertBounds.width;
                            chb.width += vertBounds.width;
                            columnHeader.setBounds(chb);
                        }
                        if (rowHeader != null && horizontalScrollBar.isVisible()) {
                            Rectangle rhb = rowHeader.getBounds();
                            rhb.height += horBounds.height;
                            rowHeader.setBounds(rhb);
                        }
                    }
                    if (horizontalScrollBar.isVisible()) {
                        bounds.height += horBounds.height;
                    }
                    viewport.setBounds(bounds);
                }
            }
        });
    }

    protected static final class PopupScrollBar extends JScrollBar {

        private PopupScrollBar(final int direction) {
            super(direction);
            putClientProperty(ScrollBarConstants.KEY_FAST_WHEEL_SCROLLING, true);
            setOpaque(false);
        }

        @Override
        public boolean isOpaque() {
            return false;
        }
    }

    @Override
    public void setPreferredSize(final Dimension preferredSize) {
        super.setPreferredSize(preferredSize);
        scrollPane.setPreferredSize(preferredSize);
    }

    private static final class ControlPanel extends JPanel {

        private boolean showVertical;
        private boolean showHorizontal;
        private final JScrollPane scrollPane;

        private ControlPanel(final JScrollPane scrollPane) {
            this.scrollPane = scrollPane;
            setLayout(null);
            scrollPane.setVerticalScrollBar(scrollPane.getVerticalScrollBar());
            if (scrollPane.getVerticalScrollBarPolicy() != JScrollPane.VERTICAL_SCROLLBAR_NEVER) {
                showVertical = true;
                add(scrollPane.getVerticalScrollBar());
            }

            scrollPane.setHorizontalScrollBar(scrollPane.getHorizontalScrollBar());
            if (scrollPane.getHorizontalScrollBarPolicy() != JScrollPane.HORIZONTAL_SCROLLBAR_NEVER) {
                showHorizontal = true;
                add(scrollPane.getHorizontalScrollBar());
            }
        }

        private void showVerticalScrollBar(final boolean show) {
            if (show == showVertical) {
                return;
            }
            showVertical = show;
            scrollPane.getVerticalScrollBar().setVisible(show);
        }

        private void showHorizontalScrollBar(final boolean show) {
            if (show == showHorizontal) {
                return;
            }
            showHorizontal = show;
            scrollPane.getHorizontalScrollBar().setVisible(show);
        }

        @Override
        public boolean contains(final int x, final int y) {
            if (
                scrollPane.getHorizontalScrollBar().isVisible()
                    && scrollPane.getHorizontalScrollBar().getBounds().contains(x, y)
            ) {
                return true;
            }
            return scrollPane.getVerticalScrollBar().isVisible()
                && scrollPane.getVerticalScrollBar().getBounds().contains(x, y);
        }

        @Override
        public boolean isOpaque() {
            return false;
        }
    }
}
