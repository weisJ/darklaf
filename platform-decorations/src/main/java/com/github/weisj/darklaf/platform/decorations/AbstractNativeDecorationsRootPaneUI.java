/*
 * MIT License
 *
 * Copyright (c) 2022 Jannis Weis
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
 */
package com.github.weisj.darklaf.platform.decorations;

import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.basic.BasicRootPaneUI;

import com.github.weisj.darklaf.platform.CustomTitlePane;
import com.github.weisj.darklaf.platform.DecorationsConstants;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;

public abstract class AbstractNativeDecorationsRootPaneUI extends BasicRootPaneUI implements DecorationsConstants {

    private static final int TITLE_PANE_CONTENT_LAYER = JLayeredPane.FRAME_CONTENT_LAYER + 1;
    private final PropertyChangeListener decorationsPropertyChangeListener = new DecorationsPropertyChangeListener();
    private final HierarchyListener decorationsHierarchyListener = new DecorationsHierarchyListener();

    private final NativeDecorationsManager decorationsManager;

    private Window window;
    private CustomTitlePane titlePane;
    private LayoutManager layoutManager;
    private LayoutManager oldLayout;
    private JRootPane rootPane;

    protected int windowDecorationsStyle = -1;

    public AbstractNativeDecorationsRootPaneUI(final NativeDecorationsManager decorationsManager) {
        this.decorationsManager = decorationsManager;
    }

    protected CustomTitlePane titlePane() {
        return titlePane;
    }

    protected NativeDecorationsManager decorationsManager() {
        return decorationsManager;
    }

    @Override
    public void installUI(JComponent c) {
        super.installUI(c);
        rootPane = (JRootPane) c;
        windowDecorationsStyle = rootPane.getWindowDecorationStyle();
        updateClientDecoration();
        installCustomListeners(rootPane);
    }

    @Override
    protected void installDefaults(JRootPane c) {
        super.installDefaults(c);
    }

    @Override
    public void uninstallUI(JComponent c) {
        super.uninstallUI(c);
        uninstallClientDecorations(rootPane);
        uninstallCustomListeners(rootPane);
        layoutManager = null;
        rootPane = null;
    }

    private void installCustomListeners(final JRootPane root) {
        root.addPropertyChangeListener(decorationsPropertyChangeListener);
        root.addHierarchyListener(decorationsHierarchyListener);
    }

    protected void uninstallCustomListeners(final JRootPane root) {
        root.removePropertyChangeListener(decorationsPropertyChangeListener);
        root.removeHierarchyListener(decorationsHierarchyListener);
    }

    private void updateClientDecoration() {
        if (decorationsEnabled(rootPane)) {
            uninstallClientDecorations(rootPane);
            if (decorationsManager.isCustomDecorationSupported()) {
                installClientDecorations(rootPane);
            }
        }
    }

    protected abstract void onDecorationsUninstall(final JRootPane rootPane);

    protected abstract void onDecorationsInstall(final JRootPane rootPane);

    protected abstract boolean shouldRemoveDecorations();

    private void installClientDecorations(final JRootPane root) {
        updateWindow(rootPane.getParent());
        onDecorationsInstall(root);
        int style = decorationsStyleFromWindow(window,
                windowDecorationsStyle < 0 ? root.getWindowDecorationStyle() : windowDecorationsStyle);
        CustomTitlePane titlePane = decorationsManager.createTitlePane(root, style, window);
        installLayout(root);
        setTitlePane(root, titlePane);
        if (titlePane != null) titlePane.setDecorationsStyle(windowDecorationsStyle);
    }

    private void updateWindow(final Component parent) {
        window = getWindow(parent);
        windowDecorationsStyle = decorationsStyleFromWindow(window, windowDecorationsStyle);
        if (titlePane != null) titlePane.setDecorationsStyle(windowDecorationsStyle);
    }

    protected int decorationsStyleFromWindow(final Window window, final int windowDecorationsStyle) {
        if (isUndecorated(window) || PropertyUtil.getBooleanProperty(rootPane, KEY_NO_DECORATIONS))
            return JRootPane.NONE;
        if (windowDecorationsStyle != JRootPane.NONE) return windowDecorationsStyle;
        if (window instanceof JFrame) return JRootPane.FRAME;
        if (window instanceof JDialog) return JRootPane.PLAIN_DIALOG;
        return windowDecorationsStyle;
    }

    private void setTitlePane(final JRootPane root, final CustomTitlePane titlePane) {
        JLayeredPane layeredPane = root.getLayeredPane();
        JComponent oldTitlePane = titlePane();

        if (oldTitlePane != null) {
            layeredPane.remove(oldTitlePane);
        }
        if (titlePane != null) {
            layeredPane.add(titlePane, TITLE_PANE_CONTENT_LAYER);
            titlePane.setVisible(true);
        }
        this.titlePane = titlePane;
    }

    private void uninstallClientDecorations(final JRootPane root) {
        if (root != null) {
            onDecorationsUninstall(root);
            root.removeHierarchyListener(decorationsHierarchyListener);
            if (titlePane != null) {
                boolean removeDecorations = shouldRemoveDecorations();
                titlePane.uninstall(removeDecorations);
                setTitlePane(root, null);
            }
            uninstallLayout(root);
            int style = root.getWindowDecorationStyle();
            if (style == JRootPane.NONE) {
                root.repaint();
                root.revalidate();
            }
        }
        if (window != null) {
            window.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
        }
        window = null;
    }

    private void installLayout(final JRootPane root) {
        if (layoutManager == null) {
            layoutManager = new NativeDecorationsRootLayout(this);
        }
        oldLayout = root.getLayout();
        root.setLayout(layoutManager);
    }

    private void uninstallLayout(final JRootPane root) {
        if (oldLayout != null) {
            root.setLayout(oldLayout);
            oldLayout = null;
        }
    }

    private boolean decorationsEnabled(final JRootPane rootPane) {
        return !(rootPane.getParent() instanceof JInternalFrame)
                && !PropertyUtil.getBooleanProperty(rootPane, KEY_NO_DECORATIONS_UPDATE)
                && rootPane.getParent() != null;
    }

    private class DecorationsPropertyChangeListener implements PropertyChangeListener {
        @Override
        public void propertyChange(final PropertyChangeEvent e) {
            String propertyName = e.getPropertyName();
            if (PropertyKey.ANCESTOR.equals(propertyName) || KEY_NO_DECORATIONS.equals(propertyName)) {
                updateWindow(rootPane.getParent());
            }
        }
    }

    private class DecorationsHierarchyListener implements HierarchyListener {

        @Override
        public void hierarchyChanged(HierarchyEvent e) {
            if (rootPane == null) return;
            Component parent = rootPane.getParent();
            if (parent == null) {
                return;
            }
            if (parent.getClass().getName().startsWith("org.jdesktop.jdic.tray")
                    || parent.getClass().getName().equals("javax.swing.Popup$HeavyWeightWindow")) {
                SwingUtilities.invokeLater(() -> {
                    if (rootPane != null) {
                        rootPane.removeHierarchyListener(this);
                    }
                });
            }
            if (e.getChangeFlags() == HierarchyEvent.PARENT_CHANGED) {
                if (getWindow(rootPane) != window) {
                    updateClientDecoration();
                }
            }
        }
    }

    private static Window getWindow(final Component component) {
        if (component == null) {
            return null;
        }
        return component instanceof Window ? (Window) component : SwingUtilities.getWindowAncestor(component);
    }

    private static boolean isUndecorated(final Window window) {
        if (window instanceof Frame) {
            return ((Frame) window).isUndecorated();
        } else if (window instanceof Dialog) {
            return ((Dialog) window).isUndecorated();
        }
        return false;
    }
}
