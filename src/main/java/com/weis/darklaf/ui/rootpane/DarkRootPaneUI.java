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

import com.weis.darklaf.platform.windows.JNIDecorations;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

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
 */
public class DarkRootPaneUI extends BasicRootPaneUI implements HierarchyListener {
    private Window window;
    private DarkTitlePane titlePane;
    private LayoutManager layoutManager;
    private LayoutManager oldLayout;
    private JRootPane rootPane;

    private HierarchyListener hierarchyListener;
    private PropertyChangeListener propertyChangeListener;


    @NotNull
    @Contract("_ -> new")
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

    public void installBorder(@NotNull final JRootPane root) {
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

    @Contract(pure = true)
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
                rootPane.removeHierarchyListener(hierarchyListener);
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