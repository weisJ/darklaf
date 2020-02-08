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
package com.github.weisj.darklaf.ui.tabframe;

import com.github.weisj.darklaf.components.alignment.Alignment;
import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabFramePopup;
import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import com.github.weisj.darklaf.components.tabframe.TabFrameUI;
import com.github.weisj.darklaf.util.ImageUtil;
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


/**
 * @author Jannis Weis
 */
public class TabFrameTransferHandler extends TransferHandler implements DropTargetListener, SwingConstants {

    private static final String MIME_TYPE = DataFlavor.javaJVMLocalObjectMimeType
            + ";class=com.github.weisj.darklaf.components.tabframe.JTabFrame";
    private static TabbedPaneDragGestureRecognizer recognizer = null;
    private final Timer timer;
    private final Timer startTimer;
    /**
     * The location of the mouse cursor throughout the drag-and-drop. This is here because of a deficiency in
     * TransferHandler's design; you have no way of knowing the exact drop location in the component with a plain
     * TransferHandler unless you implement DropTargetListener and get it that way.
     */
    protected Point mouseLocation;
    private DataFlavor tabFlavor;
    private TabTransferable currentTransferable;
    private JTabFrame lastTabFrame;


    public TabFrameTransferHandler() {
        try {
            tabFlavor = new DataFlavor(MIME_TYPE);
        } catch (ClassNotFoundException ignored) {
        }
        timer = new Timer(100, e -> {
            if (lastTabFrame != null) {
                Point p = MouseInfo.getPointerInfo().getLocation();
                SwingUtilities.convertPointFromScreen(p, lastTabFrame);
                DropTargetDragEvent evt = new DropTargetDragEvent(lastTabFrame.getDropTarget().getDropTargetContext(), p, MOVE, MOVE);
                dragOver(evt);
            }
        });
        timer.setRepeats(true);
        startTimer = new Timer(200, e -> {
            /*
             * Drag Exit can be funky. Ensure that the timer is really running.
             */
            if (!timer.isRunning()) {
                timer.start();
            }
        });
        startTimer.setRepeats(false);
    }

    @Contract("null -> null")
    private TabFrameUI getUI(final Component c) {
        if (c instanceof JTabFrame) return ((JTabFrame) c).getUI();
        return null;
    }

    protected JTabFrame.TabFramePosition getDropPosition(final Point p, final JTabFrame tabFrame) {
        return getUI(tabFrame).getDropPosition(tabFrame, p);
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

    @Override
    public void dragOver(@NotNull final DropTargetDragEvent e) {
        e.getDropTargetContext().getComponent().setCursor(Cursor.getDefaultCursor());
        mouseLocation = e.getLocation();

        Component c = e.getDropTargetContext().getComponent();
        JTabFrame destTabFrame = (JTabFrame) c;

        TabFrameUI ui = getUI(destTabFrame);
        if (ui != null) {
            TabTransferable t = currentTransferable;
            if (t != null) {
                JTabFrame.TabFramePosition tab = getDropPosition(mouseLocation, destTabFrame);
                if (tab.getAlignment() == null) {
                    ui.clearTargetIndicator();
                } else {
                    try {
                        JTabFrame sourceTab = currentTransferable.transferData.sourceTabFrame;
                        int sourceIndex = currentTransferable.transferData.tabIndex;
                        Alignment sourceAlign = currentTransferable.transferData.tabAlignment;
                        int w = getUI(sourceTab).getTabWidth(sourceTab, sourceAlign, sourceIndex);
                        int h = getUI(sourceTab).getTabHeight(sourceTab, sourceAlign, sourceIndex);
                        ui.setDropSize(w, h);
                        ui.setTargetIndicator(tab.getAlignment(), tab.getIndex());
                    } catch (IndexOutOfBoundsException ex) {
                        ui.clearTargetIndicator();
                    }
                }
            }
        }
        lastTabFrame = destTabFrame;
        startTimer.restart();
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

    protected void selectTab(final JTabFrame tabbedPane, final Alignment a, final int index) {
        SwingUtilities.invokeLater(() -> tabbedPane.toggleTab(a, index, true));
    }

    @Override
    public void dragExit(@NotNull final DropTargetEvent e) {
        Component c = e.getDropTargetContext().getComponent();
        TabFrameUI ui = getUI(c);
        if (ui != null) {
            ui.clearTargetIndicator();
        }
        lastTabFrame = (JTabFrame) c;
        startTimer.start();
    }

    @Override
    public void drop(@NotNull final DropTargetDropEvent e) {
        Component c = e.getDropTargetContext().getComponent();
        TabFrameUI ui = getUI(c);
        if (ui != null) {
            ui.clearTargetIndicator();
        }
        timer.stop();
        startTimer.stop();
    }

    @Override
    public void dragEnter(final DropTargetDragEvent e) {
        timer.stop();
        startTimer.stop();
    }

    /**
     * Called when the drag-and-drop operation has just completed.  This creates a new tab identical to the one
     * "dragged" and places it in the destination <code>JTabbedPane</code>.
     *
     * @param c The component receiving the "drop" (the instance of
     *          <code>JTabbedPane</code>).
     * @param t The data being transfered (information about the tab and the component contained by the tab).
     * @return Whether or not the import was successful.
     */
    @Override
    public boolean importData(final JComponent c, @NotNull final Transferable t) {
        boolean successful = false;
        if (hasTabFlavor(t.getTransferDataFlavors()) && mouseLocation != null) {
            try {
                JTabFrame tabFrame = (JTabFrame) c;
                JTabFrame.TabFramePosition tab = getDropPosition(mouseLocation, tabFrame);
                Alignment a = tab.getAlignment();
                int index = tab.getIndex();
                TabTransferable.TabTransferData td = (TabTransferable.TabTransferData) t.getTransferData(tabFlavor);

                if (tabFrame == td.sourceTabFrame && td.tabAlignment == a) {
                    if (index >= td.tabIndex) {
                        index--;
                    }
                }
                index++;

                if (tabFrame == td.sourceTabFrame && a == td.tabAlignment && index == td.tabIndex) {
                    //Nothing to do. Just select the tab to be sure.
                    if (td.wasSelected) {
                        selectTab(td.sourceTabFrame, a, index);
                    }
                    return false;
                }
                if (a == null || index < 0 || index > tabFrame.getTabCountAt(a)) {
                    return false;
                }
                TabFrameTab tabComp = td.sourceTabFrame.getTabComponentAt(td.tabAlignment, td.tabIndex);
                Component popupComp = td.sourceTabFrame.getPopupComponentAt(td.tabAlignment, td.tabIndex);
                td.sourceTabFrame.removeTab(td.tabAlignment, td.tabIndex);
                tabFrame.insertTab((TabFramePopup) popupComp, tabComp, a, index);
                if (td.wasSelected) {
                    tabFrame.toggleTab(a, index, true);
                }
                SwingUtilities.invokeLater(() -> td.tab.getComponent().repaint());

                successful = true;
                TabFrameUI ui = getUI(c);
                if (ui != null) {
                    ui.clearTargetIndicator();
                }
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return successful;
    }

    @Override
    public void dropActionChanged(final DropTargetDragEvent e) {
    }

    protected Transferable createTransferable(final JComponent c, @NotNull final DragGestureEvent dge) {
        JTabFrame tabFrame = (JTabFrame) c;
        if (tabFrame.isInTransfer()) {
            currentTransferable = new TabTransferable(tabFrame, tabFrame.getTransferInfo());
        } else {
            JTabFrame.TabFramePosition ind = getUI(tabFrame).getTabIndexAt(tabFrame, dge.getDragOrigin());
            tabFrame.initTransfer(ind.getAlignment(), ind.getIndex());
            currentTransferable = new TabTransferable(tabFrame, ind);
        }
        TabFrameUI ui = getUI(c);
        createDragImage(ui);
        Alignment a = currentTransferable.transferData.tabAlignment;
        int index = currentTransferable.transferData.tabIndex;
        ui.setSourceIndicator(a, index);
        startTimer.start();
        lastTabFrame = currentTransferable.transferData.sourceTabFrame;
        return currentTransferable;
    }

    protected void createDragImage(@NotNull final TabFrameUI ui) {
        Component comp = currentTransferable.transferData.tab.getComponent();
        Image tabImage = ImageUtil.scaledImageFromComponent(comp, new Rectangle(0, 0, comp.getWidth(),
                                                                                comp.getHeight()));
        int w = tabImage.getWidth(null);
        int h = tabImage.getHeight(null);
        Graphics g = tabImage.getGraphics();

        g.setColor(ui.getDragBorderColor());

        int lw = 2;
        g.fillRect(0, 0, w, lw);
        g.fillRect(0, 0, lw, h);
        g.fillRect(w - lw, 0, lw, h);
        g.fillRect(0, h - lw, w, lw);
        g.dispose();

        setDragImageOffset(new Point(w / 2, h / 2));
        setDragImage(tabImage);
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

    public static class UIResource extends TabFrameTransferHandler {

    }

    /**
     * Transferable representing a tab from a tabbed pane and its contents.
     */
    public class TabTransferable implements Transferable {

        private final TabTransferData transferData;

        public TabTransferable(@NotNull final JTabFrame tabFrame, @NotNull final JTabFrame.TabFramePosition ind) {
            transferData = new TabTransferData(tabFrame, ind.getAlignment(), ind.getIndex());
        }

        public TabFrameTab getTab() {
            return transferData.tab;
        }

        @Override
        public DataFlavor[] getTransferDataFlavors() {
            if (tabFlavor == null) return new DataFlavor[0];
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

            private final JTabFrame sourceTabFrame;
            private final int tabIndex;
            private final Alignment tabAlignment;
            private final TabFrameTab tab;
            private final boolean wasSelected;

            @Contract(pure = true)
            public TabTransferData(@NotNull final JTabFrame tabbedPane, final Alignment tabAlignment,
                                   final int tabIndex) {
                this.sourceTabFrame = tabbedPane;
                this.tabAlignment = tabAlignment;
                this.tabIndex = tabIndex;
                this.tab = tabbedPane.getTabComponentAt(tabAlignment, tabIndex);
                this.wasSelected = tab.isSelected();
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
            TabFrameTransferHandler th = (TabFrameTransferHandler) c.getTransferHandler();
            Transferable t = th.createTransferable(c, dge);
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
                ((TabFrameTransferHandler) c.getTransferHandler()).exportDone(c, dsc.getTransferable(),
                                                                              dsde.getDropAction());
            } else {
                ((TabFrameTransferHandler) c.getTransferHandler()).exportDone(c, dsc.getTransferable(), NONE);
            }
            c.setAutoscrolls(scrolls);

            TabFrameUI ui = getUI(currentTransferable.transferData.sourceTabFrame);
            if (ui != null) {
                ui.clearSourceIndicator();
            }
            if (!dsde.getDropSuccess() && currentTransferable.transferData.wasSelected) {
                selectTab(currentTransferable.transferData.sourceTabFrame,
                          currentTransferable.transferData.tabAlignment,
                          currentTransferable.transferData.tabIndex);
            }
            currentTransferable.transferData.sourceTabFrame.endTransfer();
            currentTransferable = null;
        }
    }
}
