/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.*;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;

import javax.swing.*;
import javax.swing.plaf.TabbedPaneUI;

import com.github.weisj.darklaf.ui.util.DnDUtil;

/**
 * @author Robert Futrell
 * @author Jannis Weis
 */
public class TabbedPaneTransferHandler extends TransferHandler implements DropTargetListener, SwingConstants {

    private static final String MIME_TYPE = DataFlavor.javaJVMLocalObjectMimeType + ";class="
            + TabTransferData.class.getName();
    private static TabbedPaneDragGestureRecognizer recognizer = null;
    /**
     * The location of the mouse cursor throughout the drag-and-drop. This is here because of a
     * deficiency in TransferHandler's design; you have no way of knowing the exact drop location in the
     * component with a plain TransferHandler unless you implement DropTargetListener and get it that
     * way.
     */
    protected Point mouseLocation;

    private int lastTab = -1;
    private DataFlavor tabFlavor;
    private TabTransferable currentTransferable;

    public TabbedPaneTransferHandler() {
        try {
            tabFlavor = new DataFlavor(MIME_TYPE);
        } catch (final ClassNotFoundException ignored) {
        }
    }

    @Override
    public void exportAsDrag(final JComponent comp, final InputEvent e, final int a) {
        int srcActions = getSourceActions(comp);
        int action = a;

        // only mouse events supported for drag operations
        if (!(e instanceof MouseEvent)
                // only support known actions
                || !(action == COPY || action == MOVE || action == LINK)
                // only support valid source actions
                || (srcActions & action) == 0) {

            action = NONE;
        }

        if (action != NONE && !GraphicsEnvironment.isHeadless()) {
            if (recognizer == null) {
                recognizer = new TabbedPaneDragGestureRecognizer(new TabbedPaneDragHandler());
            }
            recognizer.gestured(comp, (MouseEvent) e, srcActions, action);
        } else {
            exportDone(comp, null, NONE);
        }
    }

    /**
     * Called when the drag-and-drop operation has just completed. This creates a new tab identical to
     * the one "dragged" and places it in the destination <code>JTabbedPane</code>.
     *
     * @param c The component receiving the "drop" (the instance of <code>JTabbedPane</code>).
     * @param t The data being transfered (information about the tab and the component contained by the
     *        tab).
     * @return Whether or not the import was successful.
     */
    @Override
    public boolean importData(final JComponent c, final Transferable t) {

        boolean successful = false;
        if (hasTabFlavor(t.getTransferDataFlavors()) && mouseLocation != null) {
            try {
                JTabbedPane tabbedPane = (JTabbedPane) c;
                int tab = TabbedPaneUtil.getDroppedTabIndex(currentTransferable.getTabBounds(), tabbedPane,
                        supportsIndicator(tabbedPane), mouseLocation);
                TabTransferData td = (TabTransferData) t.getTransferData(tabFlavor);
                if (!TabbedPaneUtil.moveTabs(td.sourceTabbedPane, tabbedPane, td.tabIndex, tab)) {
                    return true;
                }

                successful = true;
                DarkTabbedPaneUI ui = supportsIndicator(c);
                if (ui != null) {
                    ui.clearDropIndicator();
                }
            } catch (final Exception e) {
                e.printStackTrace();
            }
        }
        return successful;
    }

    /** Overridden to include a check for a TabData flavor. */
    @Override
    public boolean canImport(final JComponent c, final DataFlavor[] flavors) {
        return hasTabFlavor(flavors);
    }

    /**
     * We can only move tabs, we cannot copy them.
     *
     * @param c This parameter is ignored.
     * @return <code>TransferHandler.MOVE</code>, as we can only move tabs.
     */
    @Override
    public int getSourceActions(final JComponent c) {
        return MOVE;
    }

    @Override
    protected Transferable createTransferable(final JComponent c) {
        JTabbedPane tabPane = (JTabbedPane) c;
        currentTransferable = new TabTransferable(tabPane);
        DarkTabbedPaneUI ui = supportsIndicator(c);
        int index = currentTransferable.transferData.tabIndex;
        if (tabPane.getTabCount() > 1) {
            if (index == 0) {
                index++;
            } else {
                index--;
            }
        } else {
            index = -1;
        }
        tabPane.setSelectedIndex(index);
        if (ui != null) {
            ui.setRolloverTab(-1);
            createDragImage(tabPane, ui);
            ui.setSourceIndicator(currentTransferable.transferData.tabIndex);
        }
        return currentTransferable;
    }

    protected void createDragImage(final JTabbedPane tabbedPane, final DarkTabbedPaneUI ui) {
        Color color = ui != null ? ui.getDragBorderColor()
                : tabbedPane.getBackgroundAt(currentTransferable.transferData.tabIndex);
        Image tabImage = DnDUtil.createDragImage(tabbedPane, currentTransferable.transferData.tabBounds, 2, color);
        int w = tabImage.getWidth(tabbedPane);
        int h = tabImage.getHeight(tabbedPane);
        setDragImageOffset(new Point(w / 2, h / 2));
        setDragImage(tabImage);
    }

    protected boolean hasTabFlavor(final DataFlavor[] flavors) {
        if (tabFlavor == null) {
            return false;
        }
        for (DataFlavor flavor : flavors) {
            if (tabFlavor.equals(flavor)) {
                return true;
            }
        }
        return false;
    }

    private DarkTabbedPaneUI supportsIndicator(final Component c) {
        if (c instanceof JTabbedPane) {
            TabbedPaneUI ui = ((JTabbedPane) c).getUI();
            if (ui instanceof DarkTabbedPaneUI) {
                return (DarkTabbedPaneUI) ui;
            }
        }
        return null;
    }

    @Override
    public void dragEnter(final DropTargetDragEvent e) {}

    @Override
    public void dragOver(final DropTargetDragEvent e) {
        e.getDropTargetContext().getComponent().setCursor(Cursor.getDefaultCursor());
        mouseLocation = e.getLocation();

        Component c = e.getDropTargetContext().getComponent();
        JTabbedPane destTabbedPane = (JTabbedPane) c;

        DarkTabbedPaneUI ui = supportsIndicator(destTabbedPane);
        if (ui != null) {
            TabTransferable t = currentTransferable;
            if (t != null) {
                int tab = TabbedPaneUtil.getDroppedTabIndex(t.getTabBounds(), destTabbedPane,
                        supportsIndicator(destTabbedPane), mouseLocation);
                if (tab == -1) {
                    lastTab = tab;
                    ui.clearDropIndicator();
                    return;
                }
                Rectangle dropRect = TabbedPaneUtil.getDropRect(supportsIndicator(destTabbedPane), destTabbedPane,
                        t.transferData.sourceTabbedPane, mouseLocation, t.getTabBounds(), tab, t.transferData.tabIndex,
                        lastTab);
                ui.setDnDIndicatorRect(dropRect.x, dropRect.y, dropRect.width, dropRect.height, tab,
                        t.transferData.sourceTabbedPane == destTabbedPane);
                lastTab = tab;
            }
        }
    }

    @Override
    public void dropActionChanged(final DropTargetDragEvent e) {}

    @Override
    public void dragExit(final DropTargetEvent e) {
        Component c = e.getDropTargetContext().getComponent();
        lastTab = -1;
        DarkTabbedPaneUI ui = supportsIndicator(c);
        if (ui != null) {
            ui.clearDropIndicator();
        }
    }

    @Override
    public void drop(final DropTargetDropEvent e) {
        Component c = e.getDropTargetContext().getComponent();
        DarkTabbedPaneUI ui = supportsIndicator(c);
        if (ui != null) {
            ui.clearDropIndicator();
        }
    }

    protected static class TabbedPaneDragGestureRecognizer extends DragGestureRecognizer {

        protected TabbedPaneDragGestureRecognizer(final DragGestureListener dgl) {
            super(DragSource.getDefaultDragSource(), null, NONE, dgl);
        }

        void gestured(final JComponent c, final MouseEvent e, final int srcActions, final int action) {
            setComponent(c);
            setSourceActions(srcActions);
            appendEvent(e);
            fireDragGestureRecognized(action, e.getPoint());
        }

        /** register this DragGestureRecognizer's Listeners with the Component */
        @Override
        protected void registerListeners() {}

        /**
         * unregister this DragGestureRecognizer's Listeners with the Component
         *
         * <p>
         * subclasses must override this method
         */
        @Override
        protected void unregisterListeners() {}
    }

    public static class UIResource extends TabbedPaneTransferHandler {
    }

    /** Transferable representing a tab from a tabbed pane and its contents. */
    public class TabTransferable implements Transferable {

        private final TabbedPaneTransferHandler.TabTransferData transferData;

        public TabTransferable(final JTabbedPane tabbedPane) {
            int index = tabbedPane.getSelectedIndex();
            transferData = new TabbedPaneTransferHandler.TabTransferData(tabbedPane, index);
        }

        public Rectangle getTabBounds() {
            return new Rectangle(transferData.tabBounds);
        }

        @Override
        public DataFlavor[] getTransferDataFlavors() {
            if (tabFlavor == null) return new DataFlavor[0];
            return new DataFlavor[] {tabFlavor};
        }

        @Override
        public boolean isDataFlavorSupported(final DataFlavor flavor) {
            return tabFlavor.equals(flavor);
        }

        @Override
        public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException {
            if (!isDataFlavorSupported(flavor)) {
                throw new UnsupportedFlavorException(flavor);
            }
            return transferData;
        }

    }

    /** The data remembered about the tab. */
    public static class TabTransferData {

        private final JTabbedPane sourceTabbedPane;
        private final int tabIndex;
        private final Rectangle tabBounds;

        public TabTransferData(final JTabbedPane tabbedPane, final int tabIndex) {
            this.sourceTabbedPane = tabbedPane;
            this.tabIndex = tabIndex;
            this.tabBounds = tabbedPane.getBoundsAt(tabIndex);
        }
    }

    protected class TabbedPaneDragHandler implements DragGestureListener, DragSourceListener {

        private boolean scrolls;

        // --- DragGestureListener methods -----------------------------------

        /** a Drag gesture has been recognized */
        @Override
        public void dragGestureRecognized(final DragGestureEvent dge) {
            JComponent c = (JComponent) dge.getComponent();
            TabbedPaneTransferHandler th = (TabbedPaneTransferHandler) c.getTransferHandler();
            Transferable t = th.createTransferable(c);
            if (t != null) {
                scrolls = c.getAutoscrolls();
                c.setAutoscrolls(false);
                try {
                    Image im = th.getDragImage();
                    if (im == null) {
                        dge.startDrag(Cursor.getDefaultCursor(), t, this);
                    } else {
                        dge.startDrag(Cursor.getDefaultCursor(), im, th.getDragImageOffset(), t, this);
                    }
                    return;
                } catch (final RuntimeException re) {
                    c.setAutoscrolls(scrolls);
                }
            }

            th.exportDone(c, t, NONE);
        }

        // --- DragSourceListener methods -----------------------------------

        /** as the hotspot enters a platform dependent drop site */
        @Override
        public void dragEnter(final DragSourceDragEvent dsde) {}

        /** as the hotspot moves over a platform dependent drop site */
        @Override
        public void dragOver(final DragSourceDragEvent dsde) {}

        @Override
        public void dropActionChanged(final DragSourceDragEvent dsde) {}

        /** as the hotspot exits a platform dependent drop site */
        @Override
        public void dragExit(final DragSourceEvent dsde) {}

        /** as the operation completes */
        @Override
        public void dragDropEnd(final DragSourceDropEvent dsde) {
            DragSourceContext dsc = dsde.getDragSourceContext();
            JComponent c = (JComponent) dsc.getComponent();
            if (dsde.getDropSuccess()) {
                ((TabbedPaneTransferHandler) c.getTransferHandler()).exportDone(c, dsc.getTransferable(),
                        dsde.getDropAction());
            } else {
                ((TabbedPaneTransferHandler) c.getTransferHandler()).exportDone(c, dsc.getTransferable(), NONE);
            }
            c.setAutoscrolls(scrolls);

            DarkTabbedPaneUI ui = supportsIndicator(currentTransferable.transferData.sourceTabbedPane);
            if (ui != null) {
                ui.clearSourceIndicator();
            }
            if (!dsde.getDropSuccess()) {
                TabbedPaneUtil.selectTab(currentTransferable.transferData.sourceTabbedPane,
                        currentTransferable.transferData.tabIndex);
            }
            currentTransferable = null;
        }
    }
}
