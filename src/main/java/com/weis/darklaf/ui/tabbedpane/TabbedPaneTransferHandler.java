package com.weis.darklaf.ui.tabbedpane;

import com.weis.darklaf.util.ImageUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.datatransfer.UnsupportedFlavorException;
import java.awt.dnd.DragGestureEvent;
import java.awt.dnd.DragGestureListener;
import java.awt.dnd.DragGestureRecognizer;
import java.awt.dnd.DragSource;
import java.awt.dnd.DragSourceContext;
import java.awt.dnd.DragSourceDragEvent;
import java.awt.dnd.DragSourceDropEvent;
import java.awt.dnd.DragSourceEvent;
import java.awt.dnd.DragSourceListener;
import java.awt.dnd.DropTargetDragEvent;
import java.awt.dnd.DropTargetDropEvent;
import java.awt.dnd.DropTargetEvent;
import java.awt.dnd.DropTargetListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;


public class TabbedPaneTransferHandler extends TransferHandler implements DropTargetListener, SwingConstants {

    private static final String MIME_TYPE = DataFlavor.javaJVMLocalObjectMimeType + ";class=javax.swing.JTabbedPane";
    private static TabbedPaneDragGestureRecognizer recognizer = null;
    /**
     * The location of the mouse cursor throughout the drag-and-drop.
     * This is here because of a deficiency in TransferHandler's design; you
     * have no way of knowing the exact drop location in the component with a
     * plain TransferHandler unless you implement DropTargetListener and get
     * it that way.
     */
    protected Point mouseLocation;
    private int lastTab = -1;
    private DataFlavor tabFlavor;
    private TabTransferable currentTransferable;


    public TabbedPaneTransferHandler() {
        try {
            tabFlavor = new DataFlavor(MIME_TYPE);
        } catch (ClassNotFoundException ignored) {
        }
    }

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
     * Called when the drag-and-drop operation has just completed.  This
     * creates a new tab identical to the one "dragged" and places it in the
     * destination <code>JTabbedPane</code>.
     *
     * @param c The component receiving the "drop" (the instance of
     *          <code>JTabbedPane</code>).
     * @param t The data being transfered (information about the tab and the
     *          component contained by the tab).
     * @return Whether or not the import was successful.
     */
    @Override
    public boolean importData(final JComponent c, @NotNull final Transferable t) {

        boolean successful = false;
        if (hasTabFlavor(t.getTransferDataFlavors()) && mouseLocation != null) {
            try {
                JTabbedPane tabbedPane = (JTabbedPane) c;
                int tab = TabbedPaneUtil.getDroppedTabIndex(currentTransferable.getTabBounds(), tabbedPane,
                                                            supportsIndicator(tabbedPane), mouseLocation);
                TabTransferable.TabTransferData td = (TabTransferable.TabTransferData) t.getTransferData(tabFlavor);
                if (!TabbedPaneUtil.moveTabs(td.sourceTabbedPane, tabbedPane, td.tabIndex, tab)) {
                    return true;
                }

                successful = true;
                var ui = supportsIndicator(c);
                if (ui != null) {
                    ui.clearDropIndicator();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return successful;
    }

    /**
     * Overridden to include a check for a TabData flavor.
     */
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
        var ui = supportsIndicator(c);
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
        } else {
            createDragImage(tabPane, null);
        }
        if ((ui != null && !ui.scrollableTabLayoutEnabled())
                || tabPane.getTabLayoutPolicy() == JTabbedPane.WRAP_TAB_LAYOUT) {
            tabPane.setSelectedIndex(currentTransferable.transferData.tabIndex);
        }
        return currentTransferable;
    }

    @Contract("null -> null")
    private DarkTabbedPaneUI supportsIndicator(final Component c) {
        if (c instanceof JComponent && ((JComponent) c).getUI() instanceof DarkTabbedPaneUI) {
            return ((DarkTabbedPaneUI) ((JComponent) c).getUI());
        }
        return null;
    }

    protected void createDragImage(@NotNull final JTabbedPane tabbedPane, final DarkTabbedPaneUI ui) {
        Image tabImage = ImageUtil.scaledImageFromComponent(tabbedPane, currentTransferable.transferData.tabBounds);
        int w = tabImage.getWidth(null);
        int h = tabImage.getHeight(null);
        var g = tabImage.getGraphics();

        if (ui != null) {
            g.setColor(ui.getDragBorderColor());
        } else {
            g.setColor(tabbedPane.getBackgroundAt(currentTransferable.transferData.tabIndex).brighter());
        }

        int lw = 2;
        g.fillRect(0, 0, w, lw);
        g.fillRect(0, 0, lw, h);
        g.fillRect(w - lw, 0, lw, h);
        g.fillRect(0, h - lw, w, lw);
        g.dispose();

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

    @Override
    public void dragEnter(final DropTargetDragEvent e) {
    }

    @Override
    public void dragOver(@NotNull final DropTargetDragEvent e) {
        e.getDropTargetContext().getComponent().setCursor(Cursor.getDefaultCursor());
        mouseLocation = e.getLocation();

        Component c = e.getDropTargetContext().getComponent();
        JTabbedPane destTabbedPane = (JTabbedPane) c;

        var ui = supportsIndicator(destTabbedPane);
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
                var dropRect = TabbedPaneUtil.getDropRect(supportsIndicator(destTabbedPane), destTabbedPane,
                                                          t.transferData.sourceTabbedPane, mouseLocation,
                                                          t.getTabBounds(), tab, t.transferData.tabIndex, lastTab);
                ui.setDnDIndicatorRect(dropRect.x, dropRect.y, dropRect.width, dropRect.height,
                                       tab, t.transferData.sourceTabbedPane == destTabbedPane);
                lastTab = tab;
            }
        }
    }

    @Override
    public void dropActionChanged(final DropTargetDragEvent e) {
    }

    @Override
    public void dragExit(@NotNull final DropTargetEvent e) {
        Component c = e.getDropTargetContext().getComponent();
        lastTab = -1;
        var ui = supportsIndicator(c);
        if (ui != null) {
            ui.clearDropIndicator();
        }
    }

    @Override
    public void drop(@NotNull final DropTargetDropEvent e) {
        Component c = e.getDropTargetContext().getComponent();
        var ui = supportsIndicator(c);
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

        /**
         * register this DragGestureRecognizer's Listeners with the Component
         */
        protected void registerListeners() {
        }

        /**
         * unregister this DragGestureRecognizer's Listeners with the Component
         * <p>
         * subclasses must override this method
         */
        protected void unregisterListeners() {
        }
    }

    /**
     * Transferable representing a tab from a tabbed pane and its contents.
     */
    public class TabTransferable implements Transferable {

        private final TabTransferData transferData;

        public TabTransferable(@NotNull final JTabbedPane tabbedPane) {
            int index = tabbedPane.getSelectedIndex();
            transferData = new TabTransferData(tabbedPane, index);
        }

        public Rectangle getTabBounds() {
            return new Rectangle(transferData.tabBounds);
        }

        @Override
        public DataFlavor[] getTransferDataFlavors() {
            return new DataFlavor[]{tabFlavor};
        }

        @Override
        public boolean isDataFlavorSupported(final DataFlavor flavor) {
            return tabFlavor.equals(flavor);
        }

        @NotNull
        @Override
        public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException {
            if (!isDataFlavorSupported(flavor)) {
                throw new UnsupportedFlavorException(flavor);
            }
            return transferData;
        }

        /**
         * The data remembered about the tab.
         */
        public class TabTransferData {

            private final JTabbedPane sourceTabbedPane;
            private final int tabIndex;
            private final Rectangle tabBounds;

            @Contract(pure = true)
            public TabTransferData(@NotNull final JTabbedPane tabbedPane, final int tabIndex) {
                this.sourceTabbedPane = tabbedPane;
                this.tabIndex = tabIndex;
                this.tabBounds = tabbedPane.getBoundsAt(tabIndex);
            }

        }

    }

    protected class TabbedPaneDragHandler implements DragGestureListener, DragSourceListener {

        private boolean scrolls;

        // --- DragGestureListener methods -----------------------------------

        /**
         * a Drag gesture has been recognized
         */
        public void dragGestureRecognized(@NotNull final DragGestureEvent dge) {
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
                } catch (RuntimeException re) {
                    c.setAutoscrolls(scrolls);
                }
            }

            th.exportDone(c, t, NONE);
        }

        // --- DragSourceListener methods -----------------------------------

        /**
         * as the hotspot enters a platform dependent drop site
         */
        public void dragEnter(final DragSourceDragEvent dsde) {
        }

        /**
         * as the hotspot moves over a platform dependent drop site
         */
        public void dragOver(final DragSourceDragEvent dsde) {
        }

        public void dropActionChanged(final DragSourceDragEvent dsde) {
        }

        /**
         * as the hotspot exits a platform dependent drop site
         */
        public void dragExit(final DragSourceEvent dsde) {
        }

        /**
         * as the operation completes
         */
        public void dragDropEnd(@NotNull final DragSourceDropEvent dsde) {
            DragSourceContext dsc = dsde.getDragSourceContext();
            JComponent c = (JComponent) dsc.getComponent();
            if (dsde.getDropSuccess()) {
                ((TabbedPaneTransferHandler) c.getTransferHandler()).exportDone(c, dsc.getTransferable(),
                                                                                dsde.getDropAction());
            } else {
                ((TabbedPaneTransferHandler) c.getTransferHandler()).exportDone(c, dsc.getTransferable(), NONE);
            }
            c.setAutoscrolls(scrolls);

            var ui = supportsIndicator(currentTransferable.transferData.sourceTabbedPane);
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
