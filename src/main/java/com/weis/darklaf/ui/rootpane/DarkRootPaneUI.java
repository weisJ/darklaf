package com.weis.darklaf.ui.rootpane;

/*
 * Copyright 2000-2014 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.sun.jna.platform.win32.WinDef;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicRootPaneUI;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.HierarchyListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.security.AccessController;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;


/**
 * @author Konstantin Bulenkov
 */
public class DarkRootPaneUI extends BasicRootPaneUI {
    private Cursor lastCursor;

    private enum CursorState {EXITED, ENTERED, NIL}

    private static final int CORNER_DRAG_WIDTH = 16;
    private static final int BORDER_DRAG_THICKNESS = 5;
    private Window window;
    private DarkTitlePane titlePane;
    private MouseInputListener mouseInputListener;
    private MouseInputListener titleMouseInputListener;
    private LayoutManager layoutManager;
    private LayoutManager oldLayout;
    protected JRootPane rootPane;
    protected WindowListener windowListener;
    protected Window currentWindow;
    protected HierarchyListener hierarchyListener;
    protected ComponentListener windowComponentListener;
    protected GraphicsConfiguration currentRootPaneGC;
    protected PropertyChangeListener propertyChangeListener;


    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent comp) {
        return new DarkRootPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        rootPane = (JRootPane) c;
        int style = rootPane.getWindowDecorationStyle();

        if (style != JRootPane.NONE) {
            installClientDecorations(rootPane);
        }
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallClientDecorations(rootPane);

        layoutManager = null;
        mouseInputListener = null;

        rootPane = null;
    }

    public void installBorder(@NotNull final JRootPane root) {
        int style = root.getWindowDecorationStyle();

        if (style == JRootPane.NONE) {
            LookAndFeel.uninstallBorder(root);
        } else {
            LookAndFeel.uninstallBorder(root);
//            LookAndFeel.installBorder(root, "RootPane.border");
        }
    }

    private static void uninstallBorder(final JRootPane root) {
        LookAndFeel.uninstallBorder(root);
    }


    private void installWindowListeners(final JRootPane root, final Component parent) {
        if (parent instanceof Window) {
            window = (Window) parent;
        } else {
            window = SwingUtilities.getWindowAncestor(parent);
        }

        if (window != null) {
            if (mouseInputListener == null) {
                mouseInputListener = createWindowMouseInputListener(root);
            }
            window.addMouseListener(mouseInputListener);
            window.addMouseMotionListener(mouseInputListener);

            if (titlePane != null) {
                if (titleMouseInputListener == null) {
                    titleMouseInputListener = new TitleMouseInputHandler();
                }
                titlePane.addMouseMotionListener(titleMouseInputListener);
                titlePane.addMouseListener(titleMouseInputListener);
            }
            setMaximized();
        }
    }

    private void uninstallWindowListeners(final JRootPane root) {
        if (window != null) {
            window.removeMouseListener(mouseInputListener);
            window.removeMouseMotionListener(mouseInputListener);
        }
        if (titlePane != null) {
            titlePane.removeMouseListener(titleMouseInputListener);
            titlePane.removeMouseMotionListener(titleMouseInputListener);
        }
    }

    private void installLayout(final JRootPane root) {
        if (layoutManager == null) {
            layoutManager = createLayoutManager();
        }
        oldLayout = root.getLayout();
        root.setLayout(layoutManager);
    }

    @Override
    protected void installListeners(final JRootPane root) {
        super.installListeners(root);
        hierarchyListener = e -> {
            Component parent = root.getParent();
            if (parent == null) {
                return;
            }
            if (parent.getClass().getName().startsWith("org.jdesktop.jdic.tray")
                || (parent.getClass().getName().compareTo("javax.swing.Popup$HeavyWeightWindow") == 0)) {

                //noinspection SSBasedInspection
                SwingUtilities.invokeLater(() -> {
                    root.removeHierarchyListener(hierarchyListener);
                    hierarchyListener = null;
                });
            }

            Window currWindow;
            if (parent instanceof Window) {
                currWindow = (Window) parent;
            } else {
                currWindow = SwingUtilities.getWindowAncestor(parent);
            }
            if (windowListener != null) {
                currentWindow.removeWindowListener(windowListener);
                windowListener = null;
            }
            if (windowComponentListener != null) {
                currentWindow.removeComponentListener(windowComponentListener);
                windowComponentListener = null;
            }
            if (currWindow != null) {
                windowListener = new WindowAdapter() {
                    @Override
                    public void windowClosed(final WindowEvent e) {
                        //noinspection SSBasedInspection
                        SwingUtilities.invokeLater(() -> {
                            Frame[] frames = Frame.getFrames();
                            for (Frame frame : frames) {
                                if (frame.isDisplayable()) {
                                    return;
                                }
                            }
                        });
                    }
                };

                if (!(parent instanceof JInternalFrame)) {
                    currWindow.addWindowListener(windowListener);
                }

                windowComponentListener = new ComponentAdapter() {
                    @Override
                    public void componentMoved(final ComponentEvent e) {
                        processNewPosition();
                    }

                    @Override
                    public void componentResized(final ComponentEvent e) {
                        processNewPosition();
                    }

                    private void processNewPosition() {
                        //noinspection SSBasedInspection
                        SwingUtilities.invokeLater(() -> {
                            if (window == null) {
                                return;
                            }

                            if (!window.isShowing()
                                || !window.isDisplayable()) {
                                currentRootPaneGC = null;
                                return;
                            }

                            GraphicsEnvironment ge = GraphicsEnvironment
                                                             .getLocalGraphicsEnvironment();
                            GraphicsDevice[] gds = ge
                                                           .getScreenDevices();
                            if (gds.length == 1) {
                                return;
                            }
                            Point midLoc = new Point(window.getLocationOnScreen().x + window.getWidth() / 2,
                                                     window.getLocationOnScreen().y
                                                     + window.getHeight() / 2);

                            for (GraphicsDevice gd : gds) {
                                GraphicsConfiguration gc = gd.getDefaultConfiguration();
                                Rectangle bounds = gc.getBounds();
                                if (bounds.contains(midLoc)) {
                                    if (gc != currentRootPaneGC) {
                                        currentRootPaneGC = gc;
                                        setMaximized();
                                    }
                                    break;
                                }
                            }
                        });
                    }
                };

                if (parent instanceof JFrame) {
                    currWindow.addComponentListener(windowComponentListener);
                }

                window = currWindow;
            }
            currentWindow = currWindow;
        };
        root.addHierarchyListener(hierarchyListener);
        root.addPropertyChangeListener(propertyChangeListener);
    }

    @Override
    protected void uninstallListeners(final JRootPane root) {
        if (window != null) {
            window.removeWindowListener(windowListener);
            windowListener = null;
            window
                    .removeComponentListener(windowComponentListener);
            windowComponentListener = null;
        }
        root.removeHierarchyListener(hierarchyListener);
        hierarchyListener = null;

        root.removePropertyChangeListener(propertyChangeListener);
        propertyChangeListener = null;

        super.uninstallListeners(root);
    }

    /**
     * Uninstalls the previously installed <code>LayoutManager</code>.
     *
     * @param root Root pane.
     */
    private void uninstallLayout(final JRootPane root) {
        if (oldLayout != null) {
            root.setLayout(oldLayout);
            oldLayout = null;
        }
    }

    /**
     * Installs the necessary state onto the JRootPane to render client
     * decorations. This is ONLY invoked if the <code>JRootPane</code> has a
     * decoration style other than <code>JRootPane.NONE</code>.
     *
     * @param root Root pane.
     */
    private void installClientDecorations(final JRootPane root) {
        installBorder(root);
        DarkTitlePane titlePane = createTitlePane(root);
        setTitlePane(root, titlePane);
        installWindowListeners(root, root.getParent());
        installLayout(root);
        if (window != null) {
            ((Frame) window).setUndecorated(false);
            root.revalidate();
            root.repaint();
        }
    }

    private void uninstallClientDecorations(final JRootPane root) {
        uninstallBorder(root);
        uninstallWindowListeners(root);
        setTitlePane(root, null);
        uninstallLayout(root);
        int style = root.getWindowDecorationStyle();
        if (style == JRootPane.NONE) {
            root.repaint();
            root.revalidate();
        }
        if (titlePane != null) {
            titlePane.uninstall();
            titlePane = null;
        }

        if (window != null) {
            window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
        window = null;
    }

    protected DarkTitlePane createTitlePane(final JRootPane root) {
        return new DarkTitlePane(root, this);
    }

    @NotNull
    @Contract(value = "_ -> new", pure = true)
    private MouseInputListener createWindowMouseInputListener(final JRootPane root) {
        return new DarkRootPaneUI.MouseInputHandler();
    }

    protected LayoutManager createLayoutManager() {
        return new SubstanceRootLayout();
    }

    private void setTitlePane(@NotNull final JRootPane root, final DarkTitlePane titlePane) {
        JLayeredPane layeredPane = root.getLayeredPane();
        JComponent oldTitlePane = getTitlePane();

        if (oldTitlePane != null) {
            layeredPane.remove(oldTitlePane);
        }
        if (titlePane != null) {
            layeredPane.add(titlePane, JLayeredPane.FRAME_CONTENT_LAYER);
            titlePane.setVisible(true);
        }
        this.titlePane = titlePane;
    }

    public void setMaximized() {
        Component tla = rootPane.getTopLevelAncestor();
        GraphicsConfiguration gc = (currentRootPaneGC != null) ? currentRootPaneGC
                                                               : tla.getGraphicsConfiguration();
        var mode = GraphicsEnvironment.getLocalGraphicsEnvironment().getDefaultScreenDevice().getDisplayMode();
        Insets screenInsets = Toolkit.getDefaultToolkit().getScreenInsets(gc);
        Rectangle maxBounds = new Rectangle(screenInsets.left, screenInsets.top,
                                            mode.getWidth() - ((screenInsets.left + screenInsets.right)),
                                            mode.getHeight() - ((screenInsets.top + screenInsets.bottom)));
        if (tla instanceof JFrame) {
            ((JFrame) tla).setMaximizedBounds(maxBounds);
        }
    }

    public JComponent getTitlePane() {
        return titlePane;
    }

    protected JRootPane getRootPane() {
        return rootPane;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);

        String propertyName = e.getPropertyName();
        if (propertyName == null) {
            return;
        }

        if (propertyName.equals("windowDecorationStyle")) {
            JRootPane root = (JRootPane) e.getSource();
            int style = root.getWindowDecorationStyle();

            uninstallClientDecorations(root);
            if (style != JRootPane.NONE) {
                installClientDecorations(root);
            }
        }
        if (propertyName.equals("ancestor")) {
            uninstallWindowListeners(rootPane);
            if (((JRootPane) e.getSource()).getWindowDecorationStyle() != JRootPane.NONE) {
                installWindowListeners(rootPane, rootPane.getParent());
            }
        }
    }

    protected static class SubstanceRootLayout implements LayoutManager2 {
        public Dimension preferredLayoutSize(@NotNull final Container parent) {
            Dimension cpd, mbd, tpd;
            int cpWidth = 0;
            int cpHeight = 0;
            int mbWidth = 0;
            int mbHeight = 0;
            int tpWidth = 0;
            int tpHeight = 0;
            Insets i = parent.getInsets();
            JRootPane root = (JRootPane) parent;

            if (root.getContentPane() != null) {
                cpd = root.getContentPane().getPreferredSize();
            } else {
                cpd = root.getSize();
            }
            if (cpd != null) {
                cpWidth = cpd.width;
                cpHeight = cpd.height;
            }

            if (root.getJMenuBar() != null) {
                mbd = root.getJMenuBar().getPreferredSize();
                if (mbd != null) {
                    mbWidth = mbd.width;
                    mbHeight = mbd.height;
                }
            }

            if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
                JComponent titlePane = ((DarkRootPaneUI) root.getUI()).getTitlePane();
                if (titlePane != null) {
                    tpd = titlePane.getPreferredSize();
                    if (tpd != null) {
                        tpWidth = tpd.width;
                        tpHeight = tpd.height;
                    }
                }
            }

            return new Dimension(Math.max(Math.max(cpWidth, mbWidth), tpWidth)
                                 + i.left + i.right, cpHeight + mbHeight + tpHeight + i.top + i.bottom);
        }

        public Dimension minimumLayoutSize(@NotNull final Container parent) {
            Dimension cpd, mbd, tpd;
            int cpWidth = 0;
            int cpHeight = 0;
            int mbWidth = 0;
            int mbHeight = 0;
            int tpWidth = 0;
            int tpHeight = 0;
            Insets i = parent.getInsets();
            JRootPane root = (JRootPane) parent;

            if (root.getContentPane() != null) {
                cpd = root.getContentPane().getMinimumSize();
            } else {
                cpd = root.getSize();
            }
            if (cpd != null) {
                cpWidth = cpd.width;
                cpHeight = cpd.height;
            }

            if (root.getJMenuBar() != null) {
                mbd = root.getJMenuBar().getMinimumSize();
                if (mbd != null) {
                    mbWidth = mbd.width;
                    mbHeight = mbd.height;
                }
            }
            if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
                JComponent titlePane = ((DarkRootPaneUI) root.getUI())
                                               .getTitlePane();
                if (titlePane != null) {
                    tpd = titlePane.getMinimumSize();
                    if (tpd != null) {
                        tpWidth = tpd.width;
                        tpHeight = tpd.height;
                    }
                }
            }

            return new Dimension(Math.max(Math.max(cpWidth, mbWidth), tpWidth)
                                 + i.left + i.right, cpHeight + mbHeight + tpHeight + i.top
                                                     + i.bottom);
        }

        public Dimension maximumLayoutSize(@NotNull final Container target) {
            Dimension cpd, mbd, tpd;
            int cpWidth = Integer.MAX_VALUE;
            int cpHeight = Integer.MAX_VALUE;
            int mbWidth = Integer.MAX_VALUE;
            int mbHeight = Integer.MAX_VALUE;
            int tpWidth = Integer.MAX_VALUE;
            int tpHeight = Integer.MAX_VALUE;
            Insets i = target.getInsets();
            JRootPane root = (JRootPane) target;

            if (root.getContentPane() != null) {
                cpd = root.getContentPane().getMaximumSize();
                if (cpd != null) {
                    cpWidth = cpd.width;
                    cpHeight = cpd.height;
                }
            }

            if (root.getJMenuBar() != null) {
                mbd = root.getJMenuBar().getMaximumSize();
                if (mbd != null) {
                    mbWidth = mbd.width;
                    mbHeight = mbd.height;
                }
            }

            if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
                JComponent titlePane = ((DarkRootPaneUI) root.getUI())
                                               .getTitlePane();
                if (titlePane != null) {
                    tpd = titlePane.getMaximumSize();
                    if (tpd != null) {
                        tpWidth = tpd.width;
                        tpHeight = tpd.height;
                    }
                }
            }

            int maxHeight = Math.max(Math.max(cpHeight, mbHeight), tpHeight);
            if (maxHeight != Integer.MAX_VALUE) {
                maxHeight = cpHeight + mbHeight + tpHeight + i.top + i.bottom;
            }

            int maxWidth = Math.max(Math.max(cpWidth, mbWidth), tpWidth);

            if (maxWidth != Integer.MAX_VALUE) {
                maxWidth += i.left + i.right;
            }

            return new Dimension(maxWidth, maxHeight);
        }

        public void layoutContainer(final Container parent) {
            JRootPane root = (JRootPane) parent;
            Rectangle b = root.getBounds();
            Insets i = root.getInsets();
            int nextY = 0;
            int w = b.width - i.right - i.left;
            int h = b.height - i.top - i.bottom;

            if (root.getLayeredPane() != null) {
                root.getLayeredPane().setBounds(i.left, i.top, w, h);
            }
            if (root.getGlassPane() != null) {
                root.getGlassPane().setBounds(i.left, i.top, w, h);
            }

            if ((root.getWindowDecorationStyle() != JRootPane.NONE)
                && (root.getUI() instanceof DarkRootPaneUI)) {
                JComponent titlePane = ((DarkRootPaneUI) root.getUI())
                                               .getTitlePane();
                if (titlePane != null) {
                    Dimension tpd = titlePane.getPreferredSize();
                    if (tpd != null) {
                        int tpHeight = tpd.height;
                        titlePane.setBounds(0, 0, w, tpHeight);
                        nextY += tpHeight;
                    }
                }
            }
            if (root.getJMenuBar() != null) {
                Dimension mbd = root.getJMenuBar().getPreferredSize();
                root.getJMenuBar().setBounds(0, nextY, w, mbd.height);
                nextY += mbd.height;
            }
            if (root.getContentPane() != null) {

                root.getContentPane().setBounds(0, nextY, w, h < nextY ? 0 : h - nextY);
            }
        }

        public void addLayoutComponent(final String name, final Component comp) {
        }

        public void removeLayoutComponent(final Component comp) {
        }

        public void addLayoutComponent(final Component comp, final Object constraints) {
        }

        public float getLayoutAlignmentX(final Container target) {
            return 0.0f;
        }

        public float getLayoutAlignmentY(final Container target) {
            return 0.0f;
        }

        public void invalidateLayout(final Container target) {
        }
    }

    private static final int[] cursorMapping = new int[]{
            Cursor.NW_RESIZE_CURSOR, Cursor.NW_RESIZE_CURSOR,
            Cursor.N_RESIZE_CURSOR, Cursor.NE_RESIZE_CURSOR,
            Cursor.NE_RESIZE_CURSOR, Cursor.NW_RESIZE_CURSOR, 0, 0, 0,
            Cursor.NE_RESIZE_CURSOR, Cursor.W_RESIZE_CURSOR, 0, 0, 0,
            Cursor.E_RESIZE_CURSOR, Cursor.SW_RESIZE_CURSOR, 0, 0, 0,
            Cursor.SE_RESIZE_CURSOR, Cursor.SW_RESIZE_CURSOR,
            Cursor.SW_RESIZE_CURSOR, Cursor.S_RESIZE_CURSOR,
            Cursor.SE_RESIZE_CURSOR, Cursor.SE_RESIZE_CURSOR};

    private class MouseInputHandler implements MouseInputListener {
        private boolean isMovingWindow;
        private int dragCursor;
        private int dragOffsetX;
        private int dragOffsetY;
        private int dragWidth;
        private int dragHeight;

        private final PrivilegedExceptionAction<?> getLocationAction = () -> MouseInfo.getPointerInfo().getLocation();

        public void mousePressed(final MouseEvent ev) {
            JRootPane rootPane = getRootPane();

            if (rootPane.getWindowDecorationStyle() == JRootPane.NONE) {
                return;
            }
            Point dragWindowOffset = ev.getPoint();
            Window w = (Window) ev.getSource();
            if (w != null) {
                w.toFront();
            }
            Point convertedDragWindowOffset = SwingUtilities.convertPoint(w, dragWindowOffset, getTitlePane());

            Frame f = null;
            Dialog d = null;

            if (w instanceof Frame) {
                f = (Frame) w;
            } else if (w instanceof Dialog) {
                d = (Dialog) w;
            }

            int frameState = (f != null) ? f.getExtendedState() : 0;

            if ((getTitlePane() != null)
                && getTitlePane().contains(
                    convertedDragWindowOffset)) {
                if ((((f != null) && ((frameState & Frame.MAXIMIZED_BOTH) == 0)) || (d != null))
                    && (dragWindowOffset.y >= BORDER_DRAG_THICKNESS)
                    && (dragWindowOffset.x >= BORDER_DRAG_THICKNESS)
                    && (dragWindowOffset.x < w.getWidth() - BORDER_DRAG_THICKNESS)) {
                    isMovingWindow = true;
                    dragOffsetX = dragWindowOffset.x;
                    dragOffsetY = dragWindowOffset.y;
                }
            } else if (((f != null) && f.isResizable() && ((frameState & Frame.MAXIMIZED_BOTH) == 0))
                       || ((d != null) && d.isResizable())) {
                dragOffsetX = dragWindowOffset.x;
                dragOffsetY = dragWindowOffset.y;
                dragWidth = w.getWidth();
                dragHeight = w.getHeight();
                dragCursor = getCursor(calculateCorner(w, dragWindowOffset.x, dragWindowOffset.y));
            }
        }

        public void mouseReleased(final MouseEvent ev) {
            if ((dragCursor != 0)
                && (window != null)
                && !window.isValid()) {
                window.validate();
                getRootPane().repaint();
            }
            isMovingWindow = false;
            dragCursor = 0;
        }

        public void mouseMoved(final MouseEvent ev) {
            JRootPane root = getRootPane();

            if (root.getWindowDecorationStyle() == JRootPane.NONE) {
                return;
            }

            Window w = (Window) ev.getSource();

            Frame f = null;
            Dialog d = null;

            if (w instanceof Frame) {
                f = (Frame) w;
            } else if (w instanceof Dialog) {
                d = (Dialog) w;
            }

            int cursor = getCursor(calculateCorner(w, ev.getX(), ev.getY()));

            if ((cursor != 0)
                && (((f != null) && (f.isResizable() && ((f.getExtendedState() & Frame.MAXIMIZED_BOTH) == 0)))
                    || ((d != null) && d.isResizable()))) {
                w.setCursor(Cursor.getPredefinedCursor(cursor));
            } else {
                w.setCursor(lastCursor);
            }
        }

        private void adjust(@NotNull final Rectangle bounds, final Dimension min, final int deltaX,
                            final int deltaY, final int deltaWidth, final int deltaHeight) {
            bounds.x += deltaX;
            bounds.y += deltaY;
            bounds.width += deltaWidth;
            bounds.height += deltaHeight;
            if (min != null) {
                if (bounds.width < min.width) {
                    int correction = min.width - bounds.width;
                    if (deltaX != 0) {
                        bounds.x -= correction;
                    }
                    bounds.width = min.width;
                }
                if (bounds.height < min.height) {
                    int correction = min.height - bounds.height;
                    if (deltaY != 0) {
                        bounds.y -= correction;
                    }
                    bounds.height = min.height;
                }
            }
        }

        public void mouseDragged(@NotNull final MouseEvent ev) {
            Window w = (Window) ev.getSource();
            Point pt = ev.getPoint();

            if (isMovingWindow) {
                Point windowPt;
                try {
                    windowPt = (Point) AccessController.doPrivileged(getLocationAction);
                    windowPt.x = windowPt.x - dragOffsetX;
                    windowPt.y = windowPt.y - dragOffsetY;
                    setLocation(window, windowPt.x, windowPt.y);
                } catch (PrivilegedActionException ignored) {
                }
            } else if (dragCursor != 0) {
                Rectangle r = w.getBounds();
                Rectangle startBounds = new Rectangle(r);
                Dimension min = w.getMinimumSize();

                switch (dragCursor) {
                    case Cursor.E_RESIZE_CURSOR:
                        adjust(r, min, 0, 0, pt.x
                                             + (dragWidth - dragOffsetX) - r.width, 0);
                        break;
                    case Cursor.S_RESIZE_CURSOR:
                        adjust(r, min, 0, 0, 0, pt.y
                                                + (dragHeight - dragOffsetY) - r.height);
                        break;
                    case Cursor.N_RESIZE_CURSOR:
                        adjust(r, min, 0, pt.y - dragOffsetY, 0,
                               -(pt.y - dragOffsetY));
                        break;
                    case Cursor.W_RESIZE_CURSOR:
                        adjust(r, min, pt.x - dragOffsetX, 0,
                               -(pt.x - dragOffsetX), 0);
                        break;
                    case Cursor.NE_RESIZE_CURSOR:
                        adjust(r, min, 0, pt.y - dragOffsetY, pt.x
                                                              + (dragWidth - dragOffsetX) - r.width,
                               -(pt.y - dragOffsetY));
                        break;
                    case Cursor.SE_RESIZE_CURSOR:
                        adjust(r, min, 0, 0, pt.x
                                             + (dragWidth - dragOffsetX) - r.width,
                               pt.y + (dragHeight - dragOffsetY)
                               - r.height);
                        break;
                    case Cursor.NW_RESIZE_CURSOR:
                        adjust(r, min, pt.x - dragOffsetX, pt.y
                                                           - dragOffsetY, -(pt.x - dragOffsetX),
                               -(pt.y - dragOffsetY));
                        break;
                    case Cursor.SW_RESIZE_CURSOR:
                        adjust(r, min, pt.x - dragOffsetX, 0,
                               -(pt.x - dragOffsetX), pt.y
                                                      + (dragHeight - dragOffsetY)
                                                      - r.height);
                        break;
                    default:
                        break;
                }
                if (!r.equals(startBounds)) {
                    w.setBounds(r);
                    if (Toolkit.getDefaultToolkit().isDynamicLayoutActive()) {
                        w.validate();
                        getRootPane().repaint();
                    }
                }
            }
        }

        private DarkRootPaneUI.CursorState cursorState =
                DarkRootPaneUI.CursorState.NIL;

        public void mouseEntered(@NotNull final MouseEvent ev) {
            Window w = (Window) ev.getSource();
            if (cursorState == DarkRootPaneUI.CursorState.EXITED
                || cursorState == DarkRootPaneUI.CursorState.NIL) {
                lastCursor = w.getCursor();
            }
            cursorState = DarkRootPaneUI.CursorState.ENTERED;
            mouseMoved(ev);
        }

        public void mouseExited(@NotNull final MouseEvent ev) {
            Window w = (Window) ev.getSource();
            w.setCursor(lastCursor);
            cursorState = DarkRootPaneUI.CursorState.EXITED;
        }

        public void mouseClicked(@NotNull final MouseEvent ev) {
            Window w = (Window) ev.getSource();
            Frame f;

            if (w instanceof Frame) {
                f = (Frame) w;
            } else {
                return;
            }

            JComponent windowTitlePane = getTitlePane();
            if (windowTitlePane == null) {
                return;
            }

            Point convertedPoint = SwingUtilities.convertPoint(w, ev.getPoint(), windowTitlePane);

            int state = f.getExtendedState();
            if (windowTitlePane.contains(convertedPoint)) {
                if (((ev.getClickCount() % 2) == 0)
                    && ((ev.getModifiers() & InputEvent.BUTTON1_MASK) != 0)) {
                    if (f.isResizable()) {
                        if ((state & Frame.MAXIMIZED_BOTH) != 0) {
                            setMaximized();
                            f.setExtendedState(state & ~Frame.MAXIMIZED_BOTH);
                        } else {
                            setMaximized();
                            f.setExtendedState(state | Frame.MAXIMIZED_BOTH);
                        }
                    }
                }
            }
        }

        private int calculateCorner(@NotNull final Window w, final int x, final int y) {
            Insets insets = w.getInsets();
            int xPosition = calculatePosition(x - insets.left, w.getWidth() - insets.left - insets.right);
            int yPosition = calculatePosition(y - insets.top, w.getHeight() - insets.top - insets.bottom);

            if ((xPosition == -1) || (yPosition == -1)) {
                return -1;
            }
            return yPosition * 5 + xPosition;
        }

        @Contract(pure = true)
        private int getCursor(final int corner) {
            if (corner == -1) {
                return 0;
            }
            return cursorMapping[corner];
        }

        @Contract(pure = true)
        private int calculatePosition(final int spot, final int width) {
            if (spot < BORDER_DRAG_THICKNESS) {
                return 0;
            }
            if (spot < CORNER_DRAG_WIDTH) {
                return 1;
            }
            if (spot >= (width - BORDER_DRAG_THICKNESS)) {
                return 4;
            }
            if (spot >= (width - CORNER_DRAG_WIDTH)) {
                return 3;
            }
            return 2;
        }
    }

    private void setLocation(@NotNull final Window w, final int x, final int y) {
        WinDef.LPARAM lParam = new WinDef.LPARAM();
        lParam.setValue((y << 16) | x);
        w.setLocation(x, y);
//        WindowsFrameUtil.User32dll.INSTANCE.SendMessage(WindowsFrameUtil.getHWND(w), 3,
//                                                        new WinDef.WPARAM(0), lParam);

    }

    private class TitleMouseInputHandler extends MouseInputAdapter {
        private Point dragOffset = new Point(0, 0);

        @Override
        public void mousePressed(final MouseEvent ev) {
            JRootPane rootPane = getRootPane();

            if (rootPane.getWindowDecorationStyle() == JRootPane.NONE) {
                return;
            }

            Point dragWindowOffset = ev.getPoint();
            Component source = (Component) ev.getSource();

            Point convertedDragWindowOffset = SwingUtilities.convertPoint(
                    source, dragWindowOffset, getTitlePane());

            dragWindowOffset = SwingUtilities.convertPoint(source, dragWindowOffset, window);

            if (getTitlePane() != null && getTitlePane().contains(convertedDragWindowOffset)) {
                if (window != null) {
                    window.toFront();
                    dragOffset = dragWindowOffset;
                }
            }
        }

        @Override
        public void mouseDragged(@NotNull final MouseEvent ev) {
            Component source = (Component) ev.getSource();

            Point eventLocationOnScreen = ev.getLocationOnScreen();
            if (eventLocationOnScreen == null) {
                eventLocationOnScreen = new Point(
                        ev.getX() + source.getLocationOnScreen().x, ev.getY() + source.getLocationOnScreen().y);
            }
            if (window instanceof Frame) {
                Frame f = (Frame) window;
                int frameState = f.getExtendedState();

                if (((frameState & Frame.MAXIMIZED_BOTH) == 0)) {

                    setLocation(window, eventLocationOnScreen.x - dragOffset.x, eventLocationOnScreen.y - dragOffset.y);
                }
            } else {
                setLocation(window, eventLocationOnScreen.x - dragOffset.x, eventLocationOnScreen.y - dragOffset.y);
            }
        }

        @Override
        public void mouseClicked(final MouseEvent e) {
            Frame f;

            if (window instanceof Frame) {
                f = (Frame) window;
            } else {
                return;
            }

            Point convertedPoint = SwingUtilities.convertPoint(window, e.getPoint(), getTitlePane());

            int state = f.getExtendedState();
            if ((getTitlePane() != null) && getTitlePane().contains(convertedPoint)) {
                if (((e.getClickCount() % 2) == 0) && ((e.getModifiers() & InputEvent.BUTTON1_MASK) != 0)) {
                    if (f.isResizable()) {
                        if ((state & Frame.MAXIMIZED_BOTH) != 0) {
                            setMaximized();
                            f.setExtendedState(state & ~Frame.MAXIMIZED_BOTH);
                        } else {
                            setMaximized();
                            f.setExtendedState(state | Frame.MAXIMIZED_BOTH);
                        }
                    }
                }
            }
        }
    }
}