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
    private static final Rectangle EMPTY_RECT = new Rectangle(0, 0, 0, 0);
    private static TabbedPaneDragGestureRecognizer recognizer = null;

    private int lastTab = -1;
    private DataFlavor tabFlavor;

    /**
     * The location of the mouse cursor throughout the drag-and-drop.
     * This is here because of a deficiency in TransferHandler's design; you
     * have no way of knowing the exact drop location in the component with a
     * plain TransferHandler unless you implement DropTargetListener and get
     * it that way.
     */
    protected Point mouseLocation;

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
     * Overridden to include a check for a TabData flavor.
     */
    @Override
    public boolean canImport(final JComponent c, final DataFlavor[] flavors) {
        return hasTabFlavor(flavors);
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

    protected void createDragImage(@NotNull final JTabbedPane tabbedPane, final DarkTabbedPaneUI ui) {
        Image tabImage = ImageUtil.imageFromComponent(tabbedPane, currentTransferable.transferData.tabBounds);
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


    @Override
    public void dragEnter(final DropTargetDragEvent e) {
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

    @Contract("null -> null")
    private DarkTabbedPaneUI supportsIndicator(final Component c) {
        if (c instanceof JComponent && ((JComponent) c).getUI() instanceof DarkTabbedPaneUI) {
            return ((DarkTabbedPaneUI) ((JComponent) c).getUI());
        }
        return null;
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
                int tab = getDroppedTabIndex(t, destTabbedPane, mouseLocation);

                if (tab == -1) {
                    lastTab = tab;
                    ui.clearDropIndicator();
                    return;
                }
                int tabPlacement = destTabbedPane.getTabPlacement();
                Rectangle dropRect = t.getTabBounds();
                Rectangle destRect = destTabbedPane.getBoundsAt(Math.min(tab, destTabbedPane.getTabCount() - 1));
                JTabbedPane source = t.transferData.sourceTabbedPane;
                int sourceIndex = t.transferData.tabIndex;


                if (ui.scrollableTabLayoutEnabled()) {
                    boolean lastInSource = false;
                    if (destTabbedPane == source && (tab == sourceIndex || (sourceIndex == source.getTabCount() - 1
                                                                            && tab == source.getTabCount()))) {
                        lastInSource = true;
                        destRect.width = dropRect.width;
                        destRect.height = dropRect.height;
                    }

                    switch (tabPlacement) {
                        case TOP:
                        case BOTTOM:
                            if (destTabbedPane.getComponentOrientation().isLeftToRight()) {
                                if (tab >= destTabbedPane.getTabCount() && !lastInSource) {
                                    destRect.x += destRect.width;
                                }
                                dropRect.x = destRect.x;
                                if (lastTab != -1 && lastTab < tab) {
                                    dropRect.x -= dropRect.width;
                                }
                            } else {
                                if (tab >= destTabbedPane.getTabCount()) {
                                    destRect.x -= 2 * dropRect.width;
                                    if (lastTab < tab) {
                                        destRect.x += destRect.width;
                                    }
                                }
                                dropRect.x = destRect.x + dropRect.width;
                                if (lastTab != -1 && lastTab > tab) {
                                    dropRect.x -= dropRect.width;
                                }
                            }
                            dropRect.y = destRect.y + destRect.height - dropRect.height;
                            break;
                        case LEFT:
                        case RIGHT:
                            if (tab >= destTabbedPane.getTabCount()) {
                                destRect.y += destRect.height;
                            }
                            dropRect.y = destRect.y;
                            dropRect.x = destRect.x + destRect.width - dropRect.width;
                            if (lastTab != -1 && lastTab < tab) {
                                dropRect.y -= dropRect.height;
                            }
                            break;
                    }
                } else {
                    if (source == destTabbedPane && (tab == sourceIndex || tab == sourceIndex + 1)) {
                        dropRect.setRect(0, 0, 0, 0);
                    } else {
                        int placement = destTabbedPane.getTabPlacement();
                        if (placement == TOP || placement == BOTTOM) {
                            var b = ui.getTabAreaBounds();
                            if (tab == destTabbedPane.getTabCount()) {
                                dropRect.x = destRect.x + destRect.width / 2;
                                dropRect.width = Math.min(b.x + b.width - dropRect.x, dropRect.width);
                            } else if (tab == 0) {
                                dropRect.x = destRect.x;
                                dropRect.width = Math.min(dropRect.width / 2, destRect.width / 2);
                            } else {
                                var prev = destTabbedPane.getBoundsAt(tab - 1);
                                if (destRect.y + destRect.height <= mouseLocation.y &&
                                    prev.y <= mouseLocation.y && mouseLocation.y <= prev.y + prev.height) {
                                    destRect.x = prev.x + prev.width;
                                    destRect.y = prev.y;
                                    destRect.height = prev.height;
                                }

                                dropRect.x = destRect.x;
                                dropRect.setLocation(destRect.getLocation());
                                dropRect.x -= dropRect.width / 2;
                                if (dropRect.x < b.x) {
                                    int diff = b.x - dropRect.x;
                                    dropRect.width -= diff;
                                    dropRect.x = b.x;
                                }
                                if (dropRect.x + dropRect.width > b.x + b.width) {
                                    int diff = dropRect.width + dropRect.x - b.width - b.x;
                                    dropRect.width -= diff;
                                }
                            }
                            dropRect.y = destRect.y + destRect.height - dropRect.height;
                        } else if (placement == LEFT || placement == RIGHT) {
                        }
                    }
                }
                ui.setDnDIndicatorRect(dropRect.x, dropRect.y, dropRect.width, dropRect.height,
                                       tab, source == destTabbedPane);
                lastTab = tab;
            }
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


    @Override
    public void dropActionChanged(final DropTargetDragEvent e) {
    }


    /**
     * Returns the index at which to add a tab if it is dropped at the mouse
     * location specified by <code>p</code>.
     *
     * @param tabbedPane The tabbed pane who would be receiving the tab.
     * @param p          The mouse location.
     * @return The index at which to add the tab.
     */
    protected int getDroppedTabIndex(final TabTransferable t, @NotNull final JTabbedPane tabbedPane,
                                     @NotNull final Point p) {
        var tab = tabbedPane.indexAtLocation(p.x, p.y);
        var ui = supportsIndicator(tabbedPane);
        if (ui != null) {
            if (tab == -1) {
                var bounds = ui.getTabAreaBounds();
                if (bounds.contains(p)) {
                    if (tabbedPane.getTabCount() > 0) {
                        var minb = ui.getTabBounds(tabbedPane, 0);
                        var maxb = ui.getTabBounds(tabbedPane, tabbedPane.getTabCount() - 1);
                        if (tabbedPane.getComponentOrientation().isLeftToRight()) {
                            int x = Math.max(bounds.x, minb.x);
                            bounds.width = Math.min(bounds.x + bounds.width - x, maxb.x + maxb.width - x);
                            bounds.x = x;
                        } else {
                            int x = Math.max(bounds.x, maxb.x);
                            bounds.width = Math.min(bounds.x + bounds.width - x, minb.x + minb.width - x);
                            bounds.x = x;
                        }
                        int y = Math.max(bounds.y, minb.y);
                        bounds.height = Math.min(bounds.y + bounds.height - y, maxb.x + maxb.height - y);
                    }

                    int tabPlacement = tabbedPane.getTabPlacement();
                    if (tabPlacement == TOP || tabPlacement == BOTTOM) {
                        if (tabbedPane.getComponentOrientation().isLeftToRight()) {
                            tab = p.x <= bounds.x + bounds.width / 2 ? 0 : tabbedPane.getTabCount();
                        } else {
                            tab = p.x >= bounds.x + bounds.width / 2 ? 1 : tabbedPane.getTabCount();
                        }
                    } else if (tabPlacement == LEFT || tabPlacement == RIGHT) {
                        tab = p.y <= bounds.y + bounds.height / 2 ? 0 : tabbedPane.getTabCount();
                    }
                }
            } else {
                if (tab < tabbedPane.getTabCount()) {
                    var b = tabbedPane.getBoundsAt(tab);

                    if (tab >= 1 && !ui.scrollableTabLayoutEnabled()) {
                        var prev = tabbedPane.getBoundsAt(tab - 1);
                        if (b.y + b.height < mouseLocation.y &&
                            prev.y <= mouseLocation.y && mouseLocation.y <= prev.y + prev.height) {
                            b = prev;
                        }
                    }


                    var sb = (ui.scrollableTabLayoutEnabled()) ? t.getTabBounds() : EMPTY_RECT;
                    switch (tabbedPane.getTabPlacement()) {
                        case TOP:
                        case BOTTOM:
                            if (tabbedPane.getComponentOrientation().isLeftToRight()) {
                                if (p.x >= b.x + sb.width + (b.width - sb.width) / 2) {
                                    tab += 1;
                                }
                            } else {
                                if (p.x <= b.x + (b.width - sb.width) / 2) {
                                    tab += 1;
                                }
                            }
                            break;
                        case LEFT:
                        case RIGHT:
                            if (p.y >= b.y + sb.height + (b.height - sb.height) / 2) {
                                tab += 1;
                            }
                            break;
                    }
                }
            }
        } else if (tab == -1) {
            tab = tabbedPane.getTabCount();
        }
        return tab;
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
                int tab = getDroppedTabIndex(currentTransferable, tabbedPane, mouseLocation);
                TabTransferable.TabTransferData td = (TabTransferable.TabTransferData) t.getTransferData(tabFlavor);
                JTabbedPane sourcePane = td.sourceTabbedPane;
                int sourceIndex = td.tabIndex;

                if (tabbedPane == sourcePane && sourceIndex == tab) {
                    //Nothing to do. Just select the tab to be sure.
                    selectTab(sourcePane, sourceIndex);
                    return true;
                }
                if (tab < 0 || tab > tabbedPane.getTabCount()) {
                    return false;
                }

                String tabName = sourcePane.getTitleAt(sourceIndex);
                Icon icon = sourcePane.getIconAt(sourceIndex);
                Component comp = sourcePane.getComponentAt(sourceIndex);
                String toolTip = sourcePane.getToolTipTextAt(sourceIndex);
                Color foreground = sourcePane.getForegroundAt(sourceIndex);
                Component tabComp = sourcePane.getTabComponentAt(sourceIndex);

                tabbedPane.insertTab(tabName, icon, comp, toolTip, tab);

                int index = tab;
                if (tabbedPane == sourcePane) {
                    if (sourceIndex < index) index--;
                }

                if (tabComp != null) {
                    tabbedPane.setTabComponentAt(index, tabComp);
                }
                tabbedPane.setForegroundAt(index, foreground);
                selectTab(tabbedPane, index);

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
     * Selects the specified tab in the specified tabbed pane.  This method
     * can be overridden by subclasses to do more stuff than simply select
     * the tab.
     *
     * @param tabbedPane The tabbed pane.
     * @param index      The index of the tab to select.
     */
    protected void selectTab(final JTabbedPane tabbedPane, final int index) {
        SwingUtilities.invokeLater(() -> {
            tabbedPane.setSelectedIndex(index);
            tabbedPane.requestFocus();
        });
    }


    /**
     * Transferable representing a tab from a tabbed pane and its contents.
     */
    public class TabTransferable implements Transferable {

        private final TabTransferData transferData;

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

        public TabTransferable(@NotNull final JTabbedPane tabbedPane) {
            int index = tabbedPane.getSelectedIndex();
            transferData = new TabTransferData(tabbedPane, index);
        }

        public Rectangle getTabBounds() {
            return new Rectangle(transferData.tabBounds);
        }

        @Override
        public Object getTransferData(final DataFlavor flavor) throws UnsupportedFlavorException {
            if (!isDataFlavorSupported(flavor)) {
                throw new UnsupportedFlavorException(flavor);
            }
            return transferData;
        }

        @Override
        public DataFlavor[] getTransferDataFlavors() {
            return new DataFlavor[]{tabFlavor};
        }

        @Override
        public boolean isDataFlavorSupported(final DataFlavor flavor) {
            return tabFlavor.equals(flavor);
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
                selectTab(currentTransferable.transferData.sourceTabbedPane,
                          currentTransferable.transferData.tabIndex);
            }
            currentTransferable = null;
        }

        public void dropActionChanged(final DragSourceDragEvent dsde) {
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
}
