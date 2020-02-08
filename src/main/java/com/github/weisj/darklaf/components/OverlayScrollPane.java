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
package com.github.weisj.darklaf.components;

import com.github.weisj.darklaf.ui.scrollpane.ScrollLayoutManagerDelegate;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ScrollPaneUI;
import java.awt.*;

/**
 * Scroll pane that displays its content beneath the scrollbar.
 *
 * @author Jannis Weis
 */
public class OverlayScrollPane extends JLayeredPane {

    protected final OScrollPane scrollPane;
    private final ControlPanel controlPanel;

    /**
     * Creates a <code>JScrollIndicator</code> that displays the contents of the specified component, where both
     * horizontal and vertical scrollbars appear whenever the component's contents are larger than the view and
     * scrolling in underway or the mouse is over the scrollbar position.
     */
    public OverlayScrollPane() {
        this(null);
    }

    /**
     * Creates a <code>JScrollIndicator</code> that displays the contents of the specified component, where both
     * horizontal and vertical scrollbars appear whenever the component's contents are larger than the view and
     * scrolling in underway or the mouse is over the scrollbar position.
     *
     * @param view the component to display in the scrollable viewport
     * @see JScrollPane#setViewportView
     */
    public OverlayScrollPane(final JComponent view) {
        this(view, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    /**
     * Creates a JScrollIndicator that displays the view component in a viewport whose view position can be controlled
     * with a pair of scrollbars. The scrollbar policies specify when the scrollbars are displayed, For example, if
     * vsbPolicy is JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED then the vertical scrollbar only appears if the view
     * doesn't fit vertically. The available policy settings are listed at {@link JScrollPane#setVerticalScrollBarPolicy(int)}
     * and {@link JScrollPane#setHorizontalScrollBarPolicy}.
     *
     * @param view      the view of the component.
     * @param vsbPolicy an integer that specifies the vertical scrollbar policy
     * @param hsbPolicy an integer that specifies the horizontal scrollbar policy
     */
    public OverlayScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
        scrollPane = createScrollPane(view, vsbPolicy, hsbPolicy);
        add(scrollPane, JLayeredPane.DEFAULT_LAYER);

        controlPanel = new ControlPanel(scrollPane);
        add(controlPanel, JLayeredPane.PALETTE_LAYER);
    }

    protected OScrollPane createScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
        return new OScrollPane(view, vsbPolicy, hsbPolicy);
    }

    /**
     * Returns the scroll pane used by this scroll indicator. Use carefully (e.g. to set unit increments) because not
     * all changes have an effect. You have to write listeners in this cases (e.g. for changing the scrollbar policy)
     *
     * @return the scrollPane
     */
    @NotNull
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

    @Contract(pure = true)
    @NotNull
    public JScrollBar getVerticalScrollBar() {
        return scrollPane.verticalScrollBar;
    }

    @Override
    public Dimension getPreferredSize() {
        return scrollPane.getPreferredSize();
    }

    @Contract(pure = true)
    @NotNull
    public JScrollBar getHorizontalScrollBar() {
        return scrollPane.horizontalScrollBar;
    }

    public void setViewportView(final Component c) {
        scrollPane.setViewportView(c);
    }

    private static final class PopupScrollBar extends JScrollBar {

        private final JScrollPane pane;

        private PopupScrollBar(final int direction, final JScrollPane pane) {
            super(direction);
            this.pane = pane;
            putClientProperty("JScrollBar.fastWheelScrolling", true);
            setOpaque(false);
        }

        @Contract(pure = true)
        @Override
        public boolean isOpaque() {
            return false;
        }
    }

    protected static class OScrollPane extends JScrollPane {
        private JScrollBar verticalScrollBar;
        private JScrollBar horizontalScrollBar;

        protected OScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
            super(view, vsbPolicy, hsbPolicy);
            super.setLayout(new ScrollLayoutManagerDelegate((ScrollPaneLayout) getLayout()) {
                @Override
                public void removeLayoutComponent(final Component comp) {
                    if (comp == verticalScrollBar || comp == horizontalScrollBar) {
                        return;
                    }
                    super.removeLayoutComponent(comp);
                }

                @Override
                public void layoutContainer(final Container parent) {
                    super.layoutContainer(parent);
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

        /*
         * Ensure the correct background.
         */
        public void setUI(final ScrollPaneUI ui) {
            if (verticalScrollBar == null) {
                verticalScrollBar = new PopupScrollBar(JScrollBar.VERTICAL, this);
                verticalScrollBar.putClientProperty("JScrollBar.scrollPaneParent", this);
            }
            if (horizontalScrollBar == null) {
                horizontalScrollBar = new PopupScrollBar(JScrollBar.HORIZONTAL, this);
                horizontalScrollBar.putClientProperty("JScrollBar.scrollPaneParent", this);
            }
            super.setUI(ui);
            SwingUtilities.invokeLater(() -> {
                Component component = getViewport().getView();
                if (component != null) {
                    getViewport().setBackground(component.getBackground());
                }
            });
        }

        @Override
        public JScrollBar getHorizontalScrollBar() {
            return horizontalScrollBar;
        }

        @Override
        public JScrollBar getVerticalScrollBar() {
            return verticalScrollBar;
        }
    }

    @Override
    public void setPreferredSize(final Dimension preferredSize) {
        super.setPreferredSize(preferredSize);
        scrollPane.setPreferredSize(preferredSize);
    }

    private final class ControlPanel extends JPanel {

        private boolean showVertical;
        private boolean showHorizontal;

        private ControlPanel(@NotNull final OScrollPane scrollPane) {
            setLayout(null);

            scrollPane.setVerticalScrollBar(scrollPane.verticalScrollBar);
            if (scrollPane.getVerticalScrollBarPolicy() != JScrollPane.VERTICAL_SCROLLBAR_NEVER) {
                showVertical = true;
                add(scrollPane.verticalScrollBar);
            }

            scrollPane.setHorizontalScrollBar(scrollPane.horizontalScrollBar);
            if (scrollPane.getHorizontalScrollBarPolicy() != JScrollPane.HORIZONTAL_SCROLLBAR_NEVER) {
                showHorizontal = true;
                add(scrollPane.horizontalScrollBar);
            }
        }

        private void showVerticalScrollBar(final boolean show) {
            if (show == showVertical) {
                return;
            }
            showVertical = show;
            scrollPane.verticalScrollBar.setVisible(show);
        }

        private void showHorizontalScrollBar(final boolean show) {
            if (show == showHorizontal) {
                return;
            }
            showHorizontal = show;
            scrollPane.horizontalScrollBar.setVisible(show);
        }

        @Override
        public boolean contains(final int x, final int y) {
            if (scrollPane.horizontalScrollBar.isVisible()
                    && scrollPane.horizontalScrollBar.getBounds().contains(x, y)) {
                return true;
            }
            return scrollPane.verticalScrollBar.isVisible()
                    && scrollPane.verticalScrollBar.getBounds().contains(x, y);
        }

        @Contract(pure = true)
        @Override
        public boolean isOpaque() {
            return false;
        }
    }


}
