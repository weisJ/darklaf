package com.weis.darklaf.components;

import com.weis.darklaf.ui.scrollpane.ScrollLayoutManagerDelegate;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ScrollPaneUI;
import java.awt.*;

/**
 * Scroll pane without border.
 *
 * @author Jannis Weis
 * @since 2018
 */
public class OverlayScrollPane extends JLayeredPane {

    protected final OScrollPane scrollPane;
    private final ControlPanel controlPanel;
    private Insets barInsets;

    /**
     * Creates a <code>JScrollIndicator</code> that displays the contents of the specified component,
     * where both horizontal and vertical scrollbars appear whenever the component's contents are
     * larger than the view and scrolling in underway or the mouse is over the scrollbar position.
     */
    public OverlayScrollPane() {
        this(null);
    }

    /**
     * Creates a <code>JScrollIndicator</code> that displays the contents of the specified component,
     * where both horizontal and vertical scrollbars appear whenever the component's contents are
     * larger than the view and scrolling in underway or the mouse is over the scrollbar position.
     *
     * @param view the component to display in the scrollable viewport
     * @see JScrollPane#setViewportView
     */
    public OverlayScrollPane(final JComponent view) {
        this(view, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED, JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
    }

    /**
     * Creates a JScrollIndicator that displays the view component in a viewport whose view position
     * can be controlled with a pair of scrollbars. The scrollbar policies specify when the scrollbars
     * are displayed, For example, if vsbPolicy is JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED then the
     * vertical scrollbar only appears if the view doesn't fit vertically. The available policy
     * settings are listed at {@link JScrollPane#setVerticalScrollBarPolicy(int)} and {@link
     * JScrollPane#setHorizontalScrollBarPolicy}.
     *
     * @param view      the view of the component.
     * @param vsbPolicy an integer that specifies the vertical scrollbar policy
     * @param hsbPolicy an integer that specifies the horizontal scrollbar policy
     */
    public OverlayScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
        setBorder(null);
        scrollPane = createScrollPane(view, vsbPolicy, hsbPolicy);
        scrollPane.setBorder(BorderFactory.createEmptyBorder());
        add(scrollPane, JLayeredPane.DEFAULT_LAYER);

        controlPanel = new ControlPanel(scrollPane);
        add(controlPanel, JLayeredPane.PALETTE_LAYER);
    }

    protected OScrollPane createScrollPane(final JComponent view, final int vsbPolicy, final int hsbPolicy) {
        return new OScrollPane(view, vsbPolicy, hsbPolicy);
    }

    /**
     * Returns the scroll pane used by this scroll indicator. Use carefully (e.g. to set unit
     * increments) because not all changes have an effect. You have to write listeners in this cases
     * (e.g. for changing the scrollbar policy)
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
    }    @Override
    public Dimension getPreferredSize() {
        return scrollPane.getPreferredSize();
    }

    public void setVerticalScrollBarPolicy(final int policy) {
        scrollPane.setVerticalScrollBarPolicy(policy);
        controlPanel.showVerticalScrollBar(policy != JScrollPane.VERTICAL_SCROLLBAR_NEVER);
    }    @Override
    public void setPreferredSize(final Dimension preferredSize) {
        super.setPreferredSize(preferredSize);
        scrollPane.setPreferredSize(preferredSize);
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

    @Contract(pure = true)
    @NotNull
    public JScrollBar getHorizontalScrollBar() {
        return scrollPane.horizontalScrollBar;
    }

    public void setViewportView(final Component c) {
        scrollPane.setViewportView(c);
    }

    private static final class PopupScrollBar extends JScrollBar {

        private PopupScrollBar(final int direction) {
            super(direction);
            setOpaque(false);
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
                        var bounds = viewport.getBounds();
                        var vertBounds = verticalScrollBar.getBounds();
                        var horBounds = horizontalScrollBar.getBounds();
                        var columnHeader = getColumnHeader();
                        var rowHeader = getRowHeader();
                        if (getComponentOrientation().isLeftToRight()) {
                            if (verticalScrollBar.isVisible()) {
                                bounds.width += vertBounds.width;
                            }
                            if (columnHeader != null && verticalScrollBar.isVisible()) {
                                var chb = columnHeader.getBounds();
                                chb.width += vertBounds.width;
                                columnHeader.setBounds(chb);
                            }
                            if (rowHeader != null && horizontalScrollBar.isVisible()) {
                                var rhb = rowHeader.getBounds();
                                rhb.height += horBounds.height;
                                rowHeader.setBounds(rhb);
                            }
                        } else {
                            if (verticalScrollBar.isVisible()) {
                                bounds.x -= vertBounds.width;
                                bounds.width += vertBounds.width;
                            }
                            if (columnHeader != null && verticalScrollBar.isVisible()) {
                                var chb = columnHeader.getBounds();
                                chb.x -= vertBounds.width;
                                chb.width += vertBounds.width;
                                columnHeader.setBounds(chb);
                            }
                            if (rowHeader != null && horizontalScrollBar.isVisible()) {
                                var rhb = rowHeader.getBounds();
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
                verticalScrollBar = new PopupScrollBar(JScrollBar.VERTICAL);
            }
            if (horizontalScrollBar == null) {
                horizontalScrollBar = new PopupScrollBar(JScrollBar.HORIZONTAL);
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

    private final class ControlPanel extends JPanel {

        private boolean showVertical;
        private boolean showHorizontal;

        private ControlPanel(@NotNull final OScrollPane scrollPane) {
            setLayout(null);
            setOpaque(false);

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

        @NotNull
        private Rectangle getVerticalBounds() {
            var bounds = OverlayScrollPane.this.getBounds();
            var verticalSize = scrollPane.verticalScrollBar.getPreferredSize();
            return new Rectangle(bounds.width - verticalSize.width, 0,
                                 verticalSize.width, bounds.height);
        }

        @NotNull
        private Rectangle getHorizontalBounds() {
            var bounds = OverlayScrollPane.this.getBounds();
            var horizontalSize = scrollPane.horizontalScrollBar.getPreferredSize();
            return new Rectangle(0, bounds.height - horizontalSize.height,
                                 bounds.width, horizontalSize.height);
        }
    }




}
