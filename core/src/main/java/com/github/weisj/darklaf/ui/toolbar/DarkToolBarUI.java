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
package com.github.weisj.darklaf.ui.toolbar;

import com.github.weisj.darklaf.decorators.MouseResponder;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

/**
 * @author Jannis Weis
 */
public class DarkToolBarUI extends DarkToolBarUIBridge {

    private static final Robot robot = createRobot();

    private final DropPreviewPanel previewPanel = new DropPreviewPanel();
    protected Color background;
    private Dimension verticalDim = new Dimension(0, 0);
    private Dimension horizontalDim = new Dimension(0, 0);
    private Timer timer = new Timer(5, e -> dragTo());


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
        dragWindow = createDragWindow(toolBar);
        floatingToolBar = createFloatingWindow(toolBar);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
    }

    @Override
    public void setFloating(final boolean b, final Point p) {
        if (toolBar.isFloatable()) {
            boolean visible = false;

            Window ancestor = SwingUtilities.getWindowAncestor(toolBar);
            if (ancestor != null) {
                visible = ancestor.isVisible();
            }

            if (dragWindow != null) {
                stopDrag();
            }

            floating = b;

            if (b) {
                constraintBeforeFloating = calculateConstraint();
                if (propertyListener != null) {
                    UIManager.addPropertyChangeListener(propertyListener);
                }

                floatingToolBar.getContentPane().add(toolBar, BorderLayout.CENTER);
                if (floatingToolBar instanceof Window) {
                    ((Window) floatingToolBar).pack();
                    ((Window) floatingToolBar).setLocation(floatingX, floatingY);
                    if (visible) {
                        ((Window) floatingToolBar).setVisible(true);
                    } else {
                        if (ancestor != null) {
                            ancestor.addWindowListener(new WindowAdapter() {
                                public void windowOpened(final WindowEvent e) {
                                    ((Window) floatingToolBar).setVisible(true);
                                }
                            });
                        }
                    }
                }
            } else {
                if (floatingToolBar instanceof Window) {
                    ((Window) floatingToolBar).setVisible(false);
                }
                floatingToolBar.getContentPane().remove(toolBar);

                String constraint = getDockingConstraint(dockingSource, p);
                if (constraint == null) {
                    constraint = BorderLayout.NORTH;
                }
                setOrientation(mapConstraintToOrientation(constraint));
                dockingSource.add(toolBar, constraint);
                updateDockingSource();
            }
        }
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        background = UIManager.getColor("ToolBar.background");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
    }

    protected void setBorderToNonRollover(final Component c) {
    }

    @Override
    protected DragWindow createDragWindow(final JToolBar toolbar) {
        Window frame = null;
        if (toolBar != null) {
            Container p = toolbar.getParent();
            while (p != null && !(p instanceof Window)) {
                p = p.getParent();
            }
            if (p != null) {
                frame = (Window) p;
            }
        }
        if (floatingToolBar instanceof Window) { frame = (Window) floatingToolBar; }
        return new DarkDragWindow(frame);
    }

    @Override
    protected boolean isBlocked(final Component comp, final Object constraint) {
        if (comp instanceof Container) {
            Container cont = (Container) comp;
            LayoutManager lm = cont.getLayout();
            if (lm instanceof BorderLayout) {
                BorderLayout blm = (BorderLayout) lm;
                Component c = blm.getLayoutComponent(cont, constraint);
                return (c != null && c != toolBar && c != previewPanel);
            }
        }
        return false;
    }

    protected String getDockingConstraint(final Component c, final Point p) {
        if (p == null) return constraintBeforeFloating;
        if (c.contains(p)) {
            //North
            if (p.y < horizontalDim.height && !isBlocked(c, BorderLayout.NORTH)) {
                return BorderLayout.NORTH;
            }
            //South
            if (p.y >= c.getHeight() - horizontalDim.height && !isBlocked(c, BorderLayout.SOUTH)) {
                return BorderLayout.SOUTH;
            }
            // East
            if (p.x >= c.getWidth() - verticalDim.width && !isBlocked(c, BorderLayout.EAST)) {
                return BorderLayout.EAST;
            }
            // West
            if (p.x < verticalDim.width && !isBlocked(c, BorderLayout.WEST)) {
                return BorderLayout.WEST;
            }
        }
        return null;
    }

    @Override
    protected void dragTo() {
        if (toolBar.isFloatable()) {
            Point offset = dragWindow.getOffset();
            Point global = MouseInfo.getPointerInfo().getLocation();
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
            updateDockingSource();

            dragWindow.setLocation(dragPoint.x, dragPoint.y);
            startDrag();
        }
    }

    private void ensureDockingSource() {
        if (dockingSource == null) {
            dockingSource = toolBar.getParent();
        }
    }

    protected void updateDockingSource() {
        dockingSource.invalidate();
        Container dockingSourceParent = dockingSource.getParent();
        if (dockingSourceParent != null) { dockingSourceParent.validate(); }
        dockingSource.repaint();
    }

    protected void startDrag() {
        if (!dragWindow.isVisible()) {
            dragWindow.getContentPane().add(toolBar);
            updateDockingSource();
            dragWindow.setVisible(true);
            //Is needed to intercept ongoing drag.
            SwingUtilities.invokeLater(() -> robot.mouseRelease(MouseEvent.BUTTON1_DOWN_MASK));

            int oldOrientation = toolBar.getOrientation();
            toolBar.setOrientation(SwingConstants.VERTICAL);
            verticalDim = toolBar.getPreferredSize();
            toolBar.setOrientation(SwingConstants.HORIZONTAL);
            horizontalDim = toolBar.getPreferredSize();
            toolBar.setOrientation(oldOrientation);
            timer.start();
        }
    }

    @Override
    protected void floatAt() {
        if (toolBar.isFloatable()) {
            try {
                Point offset = dragWindow.getOffset();
                Point global = MouseInfo.getPointerInfo().getLocation();
                setFloatingLocation(global.x - offset.x, global.y - offset.y);

                if (dockingSource != null) {
                    Point dockingPosition = dockingSource.getLocationOnScreen();
                    Point comparisonPoint = new Point(global.x - dockingPosition.x,
                                                      global.y - dockingPosition.y);
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
        dragWindow.setVisible(false);
        timer.stop();
    }

    @Override
    protected void paintDragWindow(final Graphics g) {
        g.setColor(dragWindow.getBackground());
        int w = dragWindow.getWidth();
        int h = dragWindow.getHeight();
        g.fillRect(0, 0, w, h);
        g.setColor(dragWindow.getBorderColor());
        g.fillRect(0, 0, w, 1);
        g.fillRect(0, 0, 1, h);
        g.fillRect(w - 1, 0, 1, h);
        g.fillRect(0, h - 1, w, 1);
    }

    public void paint(final Graphics g, final JComponent c) {
        g.setColor(background);
        g.fillRect(0, 0, c.getWidth(), c.getHeight());
    }

    protected class DarkDragWindow extends DragWindow {

        protected DarkDragWindow(final Window w) {
            super(w);
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
            super.setOrientation(o);
            Dimension size = toolBar.getPreferredSize();
            size.width += 2;
            size.height += 2;
            setSize(size);
            doLayout();
        }

        @Override
        public Point getOffset() {
            return new Point(getWidth() / 2, getHeight() / 2);
        }
    }
}
