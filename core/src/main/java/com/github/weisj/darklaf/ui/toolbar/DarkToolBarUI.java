/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.toolbar;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicToolBarUI;

import com.github.weisj.darklaf.listener.MouseResponder;
import com.github.weisj.darklaf.util.DarkUIUtil;

/** @author Jannis Weis */
public class DarkToolBarUI extends BasicToolBarUI {

    protected static final String KEY_PREFIX = "JToolBar.";
    public static final String KEY_USE_TOOL_BAR_BACKGROUND = KEY_PREFIX + "drag.useToolbarBackground";
    private static final Robot robot = createRobot();

    private final DropPreviewPanel previewPanel = new DropPreviewPanel();
    protected Color background;
    private Dimension verticalDim = new Dimension(0, 0);
    private Dimension horizontalDim = new Dimension(0, 0);
    private final Timer timer = new Timer(5, e -> dragTo());

    protected Container dockingSource;
    protected RootPaneContainer floatingToolBar;
    protected DarkDragWindow dragWindow;
    protected boolean floating;
    protected Point floatingPos = new Point();

    public static ComponentUI createUI(final JComponent c) {
        return new DarkToolBarUI();
    }

    private static Robot createRobot() {
        try {
            return new Robot();
        } catch (AWTException e) {
            return null;
        }
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        previewPanel.setToolBar(toolBar);
    }

    protected DarkDragWindow getDragWindow() {
        if (dragWindow == null) {
            dragWindow = createDarkDragWindow(toolBar);
        }
        return dragWindow;
    }

    @Override
    protected void setBorderToRollover(final Component c) {}

    @Override
    protected void setBorderToNormal(final Component c) {}

    @Override
    protected void setBorderToNonRollover(final Component c) {}

    @Override
    public void setFloating(final boolean b, final Point p) {
        if (toolBar.isFloatable()) {
            toolBar.doLayout();
            boolean visible = false;

            Window ancestor = SwingUtilities.getWindowAncestor(toolBar);
            if (ancestor != null)
                visible = ancestor.isVisible();
            if (dragWindow != null)
                stopDrag();

            floating = b;
            RootPaneContainer floatingTB = getFloatingToolBar();

            if (floating) {
                constraintBeforeFloating = calculateConstraint();

                floatingTB.getContentPane().add(toolBar, BorderLayout.CENTER);
                if (floatingTB instanceof Window) {
                    ((Window) floatingTB).pack();
                    ((Window) floatingTB).setLocation(floatingPos.x, floatingPos.y);
                    if (visible) {
                        ((Window) floatingTB).setVisible(true);
                    } else {
                        if (ancestor != null) {
                            ancestor.addWindowListener(new WindowAdapter() {
                                public void windowOpened(final WindowEvent e) {
                                    ((Window) floatingTB).setVisible(true);
                                    ancestor.removeWindowListener(this);
                                }
                            });
                        }
                    }
                }
            } else {
                if (floatingTB instanceof Window)
                    ((Window) floatingTB).setVisible(false);
                floatingTB.getContentPane().remove(toolBar);

                String constraint = getDockingConstraint(dockingSource, p);
                if (constraint == null)
                    constraint = BorderLayout.NORTH;
                setOrientation(mapConstraintToOrientation(constraint));
                dockingSource.add(toolBar, constraint);
                updateDockingSource(dockingSource);
            }
        }
    }

    protected String calculateConstraint() {
        String constraint = null;
        LayoutManager lm = dockingSource.getLayout();
        if (lm instanceof BorderLayout) {
            constraint = (String) ((BorderLayout) lm).getConstraints(toolBar);
        }
        return (constraint != null) ? constraint : constraintBeforeFloating;
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        background = UIManager.getColor("ToolBar.background");
    }

    @Override
    protected DragWindow createDragWindow(final JToolBar toolbar) {
        throw new IllegalStateException("Created incorrect drag window");
    }

    protected DarkDragWindow createDarkDragWindow(final JToolBar toolbar) {
        Window frame = DarkUIUtil.getWindow(toolbar);
        if (floatingToolBar instanceof Window) {
            frame = (Window) floatingToolBar;
        }
        return new DarkDragWindow(frame, toolBar);
    }

    protected boolean isDockable(final Component comp, final Object constraint) {
        if (comp instanceof Container) {
            Container cont = (Container) comp;
            LayoutManager lm = cont.getLayout();
            if (lm instanceof BorderLayout) {
                BorderLayout blm = (BorderLayout) lm;
                Component c = blm.getLayoutComponent(cont, constraint);
                return (c == null || c == toolBar || c == previewPanel);
            }
        }
        return true;
    }

    protected String getDockingConstraint(final Component c, final Point p) {
        if (p == null)
            return constraintBeforeFloating;
        if (c.contains(p)) {
            if (p.y < horizontalDim.height && isDockable(c, BorderLayout.NORTH)) {
                return BorderLayout.NORTH;
            }
            if (p.y >= c.getHeight() - horizontalDim.height && isDockable(c, BorderLayout.SOUTH)) {
                return BorderLayout.SOUTH;
            }
            if (p.x >= c.getWidth() - verticalDim.width && isDockable(c, BorderLayout.EAST)) {
                return BorderLayout.EAST;
            }
            if (p.x < verticalDim.width && isDockable(c, BorderLayout.WEST)) {
                return BorderLayout.WEST;
            }
        }
        return null;
    }

    @Override
    public boolean canDock(final Component c, final Point p) {
        String constraints = getDockingConstraint(c, p);
        return (p != null && constraints != null);
    }

    @Override
    protected void dragTo(final Point position, final Point origin) {
        dragTo();
    }

    protected void dragTo() {
        if (toolBar.isFloatable()) {
            DarkDragWindow dw = getDragWindow();
            Point offset = dw.getOffset();
            PointerInfo pointerInfo = MouseInfo.getPointerInfo();
            if (pointerInfo == null)
                return;

            Point global = pointerInfo.getLocation();
            Point dragPoint = new Point(global.x - offset.x, global.y - offset.y);
            ensureDockingSource();

            Point dockingPosition = dockingSource.getLocationOnScreen();
            Point comparisonPoint = new Point(global.x - dockingPosition.x, global.y - dockingPosition.y);

            if (canDock(dockingSource, comparisonPoint)) {
                String constraint = getDockingConstraint(dockingSource, comparisonPoint);
                setOrientation(mapConstraintToOrientation(constraint));
                dockingSource.add(previewPanel, constraint);
            } else {
                setOrientation(mapConstraintToOrientation(constraintBeforeFloating));
                dockingSource.remove(previewPanel);
            }
            updateDockingSource(dockingSource);

            dw.setLocation(dragPoint.x, dragPoint.y);
            if (!dw.isVisible()) {
                showDragWindow();
            }
        }
    }

    @Override
    public void setOrientation(final int orientation) {
        toolBar.setOrientation(orientation);
        if (dragWindow != null)
            dragWindow.setOrientation(orientation);
    }

    protected int mapConstraintToOrientation(final String constraint) {
        int orientation = toolBar.getOrientation();

        if (constraint != null) {
            if (constraint.equals(BorderLayout.EAST) || constraint.equals(BorderLayout.WEST))
                orientation = JToolBar.VERTICAL;
            else if (constraint.equals(BorderLayout.NORTH) || constraint.equals(BorderLayout.SOUTH))
                orientation = JToolBar.HORIZONTAL;
        }

        return orientation;
    }

    private void ensureDockingSource() {
        if (dockingSource == null) {
            dockingSource = toolBar.getParent();
        }
    }

    protected void updateDockingSource(final Component c) {
        c.invalidate();
        Container dockingSourceParent = c.getParent();
        if (dockingSourceParent != null) {
            dockingSourceParent.validate();
        }
        c.repaint();
    }

    protected void showDragWindow() {
        DarkUIUtil.getWindow(getFloatingToolBar().getRootPane()).setVisible(false);
        DarkDragWindow dw = getDragWindow();
        dw.getContentPane().add(toolBar);
        dw.doLayout();
        updateDockingSource(dockingSource);
        dw.setVisible(true);
        // Is needed to intercept ongoing drag.
        SwingUtilities.invokeLater(() -> robot.mouseRelease(MouseEvent.BUTTON1_DOWN_MASK));

        int oldOrientation = toolBar.getOrientation();
        toolBar.setOrientation(SwingConstants.VERTICAL);
        verticalDim = toolBar.getPreferredSize();
        toolBar.setOrientation(SwingConstants.HORIZONTAL);
        horizontalDim = toolBar.getPreferredSize();
        toolBar.setOrientation(oldOrientation);
        timer.start();
    }

    public RootPaneContainer getFloatingToolBar() {
        if (floatingToolBar == null)
            floatingToolBar = createFloatingWindow(toolBar);
        return floatingToolBar;
    }

    @Override
    protected void floatAt(final Point position, final Point origin) {
        // Handles by DarkDragWindow.
    }

    @Override
    public void setFloatingLocation(final int x, final int y) {
        floatingPos.setLocation(x, y);
    }

    protected void floatAt() {
        if (toolBar.isFloatable()) {
            try {
                DarkDragWindow dw = getDragWindow();
                Point offset = dw.getOffset();
                Point global = MouseInfo.getPointerInfo().getLocation();
                setFloatingLocation(global.x - offset.x, global.y - offset.y);

                if (dockingSource != null) {
                    Point dockingPosition = dockingSource.getLocationOnScreen();
                    Point comparisonPoint = new Point(global.x - dockingPosition.x, global.y - dockingPosition.y);
                    if (canDock(dockingSource, comparisonPoint)) {
                        setFloating(false, comparisonPoint);
                    } else {
                        setFloating(true, null);
                    }
                } else {
                    setFloating(true, null);
                }
                dockingSource.remove(previewPanel);
            } catch (IllegalComponentStateException ignored) {
            }
        }
    }

    protected void stopDrag() {
        getDragWindow().setVisible(false);
        timer.stop();
    }

    public void paint(final Graphics g, final JComponent c) {
        g.setColor(background);
        g.fillRect(0, 0, c.getWidth(), c.getHeight());
    }

    protected class DarkDragWindow extends JDragWindow {

        protected DarkDragWindow(final Window w, final JToolBar toolBar) {
            super(w, toolBar);
            setBorderColor(UIManager.getColor("ToolBar.borderColor"));
            setLayout(new BorderLayout());
            setBackground(toolBar.getBackground());
            JPanel glassPane = new JPanel();
            glassPane.setOpaque(false);
            glassPane.addMouseListener(new MouseResponder(e -> {
                e.consume();
                if (e.getID() == MouseEvent.MOUSE_RELEASED) {
                    floatAt();
                }
            }));
            setGlassPane(glassPane);
            glassPane.setVisible(true);
        }

        @Override
        public void setOrientation(final int o) {
            Dimension size = toolBar.getPreferredSize();
            Insets insets = getInsets();
            size.width += insets.left + insets.right;
            size.height += insets.top + insets.bottom;
            setSize(size);
            doLayout();
        }

        @Override
        public Point getOffset() {
            return new Point(getWidth() / 2, getHeight() / 2);
        }
    }

    @Override
    protected WindowListener createFrameListener() {
        return new DarkFrameListener();
    }

    protected static class DarkFrameListener extends WindowAdapter {
    }
}
