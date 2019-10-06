package com.weis.darklaf.ui.combobox;

import com.weis.darklaf.components.OverlayScrollPane;

import javax.swing.*;
import javax.swing.plaf.basic.BasicComboPopup;
import java.awt.*;
import java.awt.event.AdjustmentListener;
import java.awt.event.MouseEvent;

public class DarkComboPopup extends BasicComboPopup {

    private final AdjustmentListener adjustmentListener = e -> {
        var p = MouseInfo.getPointerInfo().getLocation();
        SwingUtilities.convertPointFromScreen(p, list);
        MouseEvent ev = new MouseEvent(list,
                                       MouseEvent.MOUSE_MOVED, 0, 0, p.x, p.y, 1,
                                       false, 0);
        for (var ml : list.getMouseMotionListeners()) {
            if (ml != null) {
                ml.mouseMoved(ev);
            }
        }
    };
    private double lastEvent;
    private boolean visible = false;
    private OverlayScrollPane overlayScrollPane;

    /**
     * Constructs a new instance of {@code BasicComboPopup}.
     *
     * @param combo an instance of {@code JComboBox}
     */
    public DarkComboPopup(final JComboBox<Object> combo) {
        super(combo);
    }

    @Override
    protected void firePopupMenuWillBecomeVisible() {
        if (list.getModel().getSize() != 0) {
            int height = list.getUI().getCellBounds(list, 0, 0).height;
            overlayScrollPane.getVerticalScrollBar().setUnitIncrement(height);
        }
        visible = true;
        overlayScrollPane.getVerticalScrollBar().addAdjustmentListener(adjustmentListener);
        super.firePopupMenuWillBecomeVisible();
    }

    @Override
    protected void firePopupMenuWillBecomeInvisible() {
        lastEvent = System.currentTimeMillis();
        visible = false;
        overlayScrollPane.getVerticalScrollBar().removeAdjustmentListener(adjustmentListener);
        super.firePopupMenuWillBecomeInvisible();
    }

    @Override
    protected void firePopupMenuCanceled() {
        visible = false;
        overlayScrollPane.getVerticalScrollBar().removeAdjustmentListener(adjustmentListener);
        super.firePopupMenuCanceled();
    }

    @Override
    protected JScrollPane createScroller() {
        overlayScrollPane = new OverlayScrollPane(list, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,
                                                  ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        overlayScrollPane.getVerticalScrollBar().putClientProperty("ScrollBar.thin", Boolean.TRUE);
        return overlayScrollPane.getScrollPane();
    }

    @Override
    protected void configureScroller() {
        scroller.setFocusable(false);
        scroller.getVerticalScrollBar().setFocusable(false);
        scroller.setBorder(null);
    }

    @Override
    protected void configurePopup() {
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        setBorderPainted(true);
        setOpaque(false);
        add(overlayScrollPane);
        setDoubleBuffered(true);
        setFocusable(false);
    }

    @Override
    protected void togglePopup() {
        if (comboBox.getItemCount() == 0) return;
        if (visible) {
            visible = false;
            hide();
        } else if (lastEvent == 0 || (System.currentTimeMillis() - lastEvent) > 250) {
            visible = true;
            SwingUtilities.invokeLater(this::show);
        }
    }

    protected void reset() {
        lastEvent = 0;
        if (visible) {
            hide();
        }
        visible = false;
    }
}
