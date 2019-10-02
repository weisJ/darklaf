package com.weis.darklaf.components;

import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import java.awt.*;

public class ScrollPopupMenu extends JPopupMenu {

    private int maxHeight;
    private final JPanel contentPane;
    private final JScrollPane scrollPane;
    private JWindow popWin;
    private int posX;
    private int posY;

    public ScrollPopupMenu(final int maxHeight) {
        this.maxHeight = maxHeight;
        contentPane = new JPanel(new BorderLayout());
        OverlayScrollPane overlayScrollPane = createScrollPane();
        scrollPane = overlayScrollPane.getScrollPane();
        contentPane.add(overlayScrollPane, BorderLayout.CENTER);
        contentPane.setBorder(getBorder());
        setDoubleBuffered(true);
        MenuKeyListener menuKeyListener = new MenuKeyListener() {
            @Override
            public void menuKeyTyped(final MenuKeyEvent e) {
            }

            @Override
            public void menuKeyPressed(final MenuKeyEvent e) {
                SwingUtilities.invokeLater(() -> {
                    var path = e.getMenuSelectionManager().getSelectedPath();
                    if (path.length == 0) {
                        return;
                    }
                    var bounds = path[path.length - 1].getComponent().getBounds();
                    var r = SwingUtilities.convertRectangle(ScrollPopupMenu.this, bounds, scrollPane);
                    scrollPane.getViewport().scrollRectToVisible(r);
                });
            }

            @Override
            public void menuKeyReleased(final MenuKeyEvent e) {

            }
        };
        addMenuKeyListener(menuKeyListener);
    }

    public void setMaxHeight(final int maxHeight) {
        this.maxHeight = maxHeight;
    }

    @NotNull
    private OverlayScrollPane createScrollPane() {
        var view = new JPanel(new BorderLayout());
        view.add(this, BorderLayout.CENTER);
        OverlayScrollPane overlayScrollPane = new OverlayScrollPane(view,
                                                                    JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                                    JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);

        JScrollBar bar = overlayScrollPane.getVerticalScrollBar();
        bar.putClientProperty("ScrollBar.thin", Boolean.TRUE);
        DarkUIUtil.doNotCancelPopupSetup(bar);
        DarkUIUtil.doNotCancelPopupSetup(overlayScrollPane.getScrollPane());
        return overlayScrollPane;
    }

    @Override
    public JMenuItem add(@NotNull final JMenuItem menuItem) {
        menuItem.getModel().addChangeListener(e -> contentPane.repaint(menuItem.getBounds()));
        return super.add(menuItem);
    }

    @Override
    public void setLocation(final int x, final int y) {
        if (popWin != null && popWin.isShowing()) {
            popWin.setLocation(x, y);
        } else {
            posX = x;
            posY = y;
        }
    }

    @Override
    public boolean isVisible() {
        return popWin != null && popWin.isShowing();
    }

    @Override
    public void setVisible(final boolean b) {
        if (b == isVisible()) {
            return;
        }
        if (b) {
            if (isPopupMenu()) {
                MenuElement[] menuElements = new MenuElement[1];
                if (getSubElements().length > 0) {
                    menuElements = new MenuElement[2];
                    menuElements[1] = getSubElements()[0];
                }
                menuElements[0] = this;
                MenuSelectionManager.defaultManager().setSelectedPath(menuElements);
            }
            firePopupMenuWillBecomeVisible();
            showPopup();
            firePropertyChange("visible", Boolean.FALSE, Boolean.TRUE);
        } else {
            hidePopup();
        }
    }

    protected void hidePopup() {
        if (popWin != null) {
            firePopupMenuWillBecomeInvisible();
            popWin.setVisible(false);
            popWin = null;
            firePropertyChange("visible", Boolean.TRUE, Boolean.FALSE);
            if (isPopupMenu()) {
                MenuSelectionManager.defaultManager().clearSelectedPath();
            }
        }
    }

    protected void showPopup() {
        var comp = getInvoker();
        if (comp == null) return;

        while (comp.getParent() != null) {
            comp = comp.getParent();
        }

        if (popWin == null || popWin.getOwner() != comp) {
            popWin = comp instanceof Window ? new JWindow((Window) comp) : new JWindow(new JFrame());
        }
        pack();
        popWin.setLocation(posX, posY);
        popWin.setVisible(true);
        requestFocus();
    }

    @Override
    public void pack() {
        if (popWin == null) {
            return;
        }
        final Dimension prefSize = getPreferredSize();
        if (maxHeight == 0 || prefSize.height <= maxHeight) {
            setBounds(0, 0, prefSize.width, prefSize.height);
            popWin.setContentPane(this);
            setBorderPainted(true);
            popWin.setSize(prefSize.width, prefSize.height);
        } else {
            int increment = getComponentCount() > 0
                            ? Math.max(1, getComponent(0).getPreferredSize().height / 2)
                            : 1;
            var bar = scrollPane.getVerticalScrollBar();
            bar.setValue(bar.getMinimum());
            bar.setUnitIncrement(increment);
            setBorderPainted(false);
            popWin.setContentPane(contentPane);
            popWin.pack();
            popWin.setSize(popWin.getSize().width + bar.getPreferredSize().width, maxHeight);
        }
    }

    /**
     * Get the scroll pane of the popup.
     *
     * @return scroll pane;
     */
    @Nullable
    public JScrollPane getScrollPane() {
        return scrollPane;
    }

    private boolean isPopupMenu() {
        var invoker = getInvoker();
        return ((invoker != null) && !(invoker instanceof JMenu));
    }
}
