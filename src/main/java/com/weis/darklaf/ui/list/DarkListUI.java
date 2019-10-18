package com.weis.darklaf.ui.list;

import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.event.MouseEvent;

/**
 * @author Jannis Weis
 */
public class DarkListUI extends DarkListUIBridge {

    static {
        UIManager.put("List.cellRenderer", new DarkListCellRenderer());
    }

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent list) {
        return new DarkListUI();
    }

    protected void paintCell(final Graphics g, final int row, final Rectangle rowBounds,
                             final ListCellRenderer<Object> cellRenderer, final ListModel<Object> dataModel,
                             final ListSelectionModel selModel, final int leadIndex) {
        Object value = dataModel.getElementAt(row);
        boolean cellHasFocus = list.hasFocus() && (row == leadIndex);
        boolean isSelected = selModel.isSelectedIndex(row);

        Component rendererComponent =
                cellRenderer.getListCellRendererComponent(list, value, row, isSelected, cellHasFocus);

        int cx = rowBounds.x;
        int cy = rowBounds.y;
        int cw = rowBounds.width;
        int ch = rowBounds.height;

        if (Boolean.TRUE.equals(list.getClientProperty("JList.shrinkWrap"))) {
            // Shrink renderer to preferred size. This is mostly used on Windows
            // where selection is only shown around the file name, instead of
            // across the whole list cell.
            int w = Math.min(cw, rendererComponent.getPreferredSize().width + 4);
            if (!list.getComponentOrientation().isLeftToRight()) {
                cx += (cw - w);
            }
            cw = w;
        }

        rendererPane.paintComponent(g, rendererComponent, list, cx, cy, cw, ch, true);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        list.putClientProperty("JList.alternateRowColor", UIManager.getBoolean("List.alternateRowColor"));
    }

    @Override
    protected Handler getHandler() {
        if (handler == null) {
            handler = new DarkHandler();
        }
        return handler;
    }


    protected class DarkHandler extends Handler {

        @Override
        protected void adjustSelection(final MouseEvent e) {
            int row = list.locationToIndex(e.getPoint());
            if (row < 0) {
                // If shift is down in multi-select, we should do nothing.
                // For single select or non-shift-click, clear the selection
                if (isFileList && !Boolean.TRUE.equals(list.getClientProperty("JList.fullRowSelection"))
                        && e.getID() == MouseEvent.MOUSE_PRESSED &&
                        (!e.isShiftDown() || list.getSelectionMode() == ListSelectionModel.SINGLE_SELECTION)) {
                    list.clearSelection();
                }
            } else {
                int anchorIndex = adjustIndex(list.getAnchorSelectionIndex(), list);
                boolean anchorSelected;
                if (anchorIndex == -1) {
                    anchorIndex = 0;
                    anchorSelected = false;
                } else {
                    anchorSelected = list.isSelectedIndex(anchorIndex);
                }

                if (DarkUIUtil.isMenuShortcutKeyDown(e)) {
                    if (e.isShiftDown()) {
                        if (anchorSelected) {
                            list.addSelectionInterval(anchorIndex, row);
                        } else {
                            list.removeSelectionInterval(anchorIndex, row);
                            if (isFileList) {
                                list.addSelectionInterval(row, row);
                                list.getSelectionModel().setAnchorSelectionIndex(anchorIndex);
                            }
                        }
                    } else if (list.isSelectedIndex(row)) {
                        list.removeSelectionInterval(row, row);
                    } else {
                        list.addSelectionInterval(row, row);
                    }
                } else if (e.isShiftDown()) {
                    list.setSelectionInterval(anchorIndex, row);
                } else {
                    list.setSelectionInterval(row, row);
                }
            }
        }
    }
}
