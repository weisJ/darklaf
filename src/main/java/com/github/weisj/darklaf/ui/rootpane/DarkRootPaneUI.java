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

package com.github.weisj.darklaf.ui.rootpane;

import com.github.weisj.darklaf.platform.windows.JNIDecorations;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicRootPaneUI;
import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;


/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkRootPaneUI extends BasicRootPaneUI implements HierarchyListener {
    private Window window;
    private DarkTitlePane titlePane;
    private LayoutManager layoutManager;
    private LayoutManager oldLayout;
    private JRootPane rootPane;

    private HierarchyListener hierarchyListener;
    private PropertyChangeListener propertyChangeListener;


    public static ComponentUI createUI(final JComponent comp) {
        return new DarkRootPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        rootPane = (JRootPane) c;
        if (rootPane.getWindowDecorationStyle() != JRootPane.NONE) {
            installClientDecorations(rootPane);
        }
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallClientDecorations(rootPane);
        layoutManager = null;
        rootPane = null;
    }

    @Override
    protected void installListeners(final JRootPane root) {
        super.installListeners(root);
        root.addHierarchyListener(this);
        root.addPropertyChangeListener(propertyChangeListener);
    }

    @Override
    protected void uninstallListeners(final JRootPane root) {
        root.removeHierarchyListener(hierarchyListener);
        hierarchyListener = null;

        root.removePropertyChangeListener(propertyChangeListener);
        propertyChangeListener = null;

        super.uninstallListeners(root);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        String propertyName = e.getPropertyName();
        if ("windowDecorationStyle".equals(propertyName)) {
            JRootPane root = (JRootPane) e.getSource();
            int style = root.getWindowDecorationStyle();

            uninstallClientDecorations(root);
            if (style != JRootPane.NONE) {
                installClientDecorations(root);
            }
        } else if ("ancestor".equals(propertyName)) {
            if (((JRootPane) e.getSource()).getWindowDecorationStyle() != JRootPane.NONE) {
                updateWindow(rootPane.getParent());
            }
        }
    }

    private void uninstallClientDecorations(final JRootPane root) {
        uninstallBorder(root);
        if (titlePane != null) {
            titlePane.uninstall();
            setTitlePane(root, null);
        }
        uninstallLayout(root);
        int style = root.getWindowDecorationStyle();
        if (style == JRootPane.NONE) {
            root.repaint();
            root.revalidate();
        }
        if (window != null) {
            window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
        window = null;
    }

    private static void uninstallBorder(final JRootPane root) {
        LookAndFeel.uninstallBorder(root);
    }

    private void uninstallLayout(final JRootPane root) {
        if (oldLayout != null) {
            root.setLayout(oldLayout);
            oldLayout = null;
        }
    }

    private void installClientDecorations(final JRootPane root) {
        installBorder(root);
        DarkTitlePane titlePane = createTitlePane(root);
        setTitlePane(root, titlePane);
        updateWindow(root.getParent());
        installLayout(root);
        if (window != null) {
            if (window instanceof Frame && !window.isDisplayable()) {
                ((Frame) window).setUndecorated(root.getWindowDecorationStyle() == JRootPane.NONE);
            } else if (window instanceof Dialog && !window.isDisplayable()) {
                ((Dialog) window).setUndecorated(root.getWindowDecorationStyle() == JRootPane.NONE);
            }
            root.revalidate();
            root.repaint();
        }
    }

    public void installBorder(final JRootPane root) {
        int style = root.getWindowDecorationStyle();
        if (style == JRootPane.NONE) {
            LookAndFeel.uninstallBorder(root);
        } else {
            LookAndFeel.uninstallBorder(root);
        }
    }

    protected DarkTitlePane createTitlePane(final JRootPane root) {
        return new DarkTitlePane(root);
    }

    private void setTitlePane(final JRootPane root, final DarkTitlePane titlePane) {
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

    private void updateWindow(final Component parent) {
        if (parent instanceof Window) {
            window = (Window) parent;
        } else {
            window = SwingUtilities.getWindowAncestor(parent);
        }
    }

    private void installLayout(final JRootPane root) {
        if (layoutManager == null) {
            layoutManager = createLayoutManager();
        }
        oldLayout = root.getLayout();
        root.setLayout(layoutManager);
    }


    protected JComponent getTitlePane() {
        return titlePane;
    }

    protected LayoutManager createLayoutManager() {
        return new SubstanceRootLayout();
    }

    @Override
    public void hierarchyChanged(final HierarchyEvent e) {
        if (rootPane == null) return;
        Component parent = rootPane.getParent();
        if (parent == null) {
            return;
        }
        if (parent.getClass().getName().startsWith("org.jdesktop.jdic.tray")
                || (parent.getClass().getName().compareTo("javax.swing.Popup$HeavyWeightWindow") == 0)) {
            SwingUtilities.invokeLater(() -> {
                if (rootPane != null) {
                    rootPane.removeHierarchyListener(hierarchyListener);
                }
                hierarchyListener = null;
            });
        }

        Window currWindow;
        if (parent instanceof Window) {
            currWindow = (Window) parent;
        } else {
            currWindow = SwingUtilities.getWindowAncestor(parent);
        }

        if (currWindow != null) {
            if (currWindow != window) {
                if (!JNIDecorations.isCustomDecorationSupported()) return;
                if (rootPane.getWindowDecorationStyle() == JRootPane.NONE) return;
                updateClientDecoration();
            }
            window = currWindow;
        }
    }

    protected void updateClientDecoration() {
        uninstallClientDecorations(rootPane);
        installClientDecorations(rootPane);
    }
}