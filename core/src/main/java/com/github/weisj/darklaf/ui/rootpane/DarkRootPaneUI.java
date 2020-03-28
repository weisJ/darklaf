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

import com.github.weisj.darklaf.decorations.CustomTitlePane;
import com.github.weisj.darklaf.platform.Decorations;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyKey;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicRootPaneUI;
import java.awt.*;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.beans.PropertyChangeEvent;


/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkRootPaneUI extends BasicRootPaneUI implements HierarchyListener {

    protected static final String KEY_PREFIX = "JRootPane.";
    public static final String KEY_IS_MEDIUM_WEIGHT_POPUP_ROOT = "mediumWeightPopupRoot";
    public static final String KEY_IS_POPUP = KEY_PREFIX + "isPopup";
    private Window window;
    private CustomTitlePane titlePane;
    private LayoutManager layoutManager;
    private LayoutManager oldLayout;
    private JRootPane rootPane;
    private final DisposeListener disposeListener = new DisposeListener();

    private boolean decorationStyleLock = false;
    private int windowDecorationsStyle = -1;


    public static ComponentUI createUI(final JComponent comp) {
        return new DarkRootPaneUI();
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        rootPane = (JRootPane) c;
        updateClientDecoration();
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallClientDecorations(rootPane);
        layoutManager = null;
        rootPane = null;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        String propertyName = e.getPropertyName();
        if (PropertyKey.WINDOW_DECORATIONS_STYLE.equals(propertyName)) {
            if (!decorationStyleLock) {
                windowDecorationsStyle = rootPane.getWindowDecorationStyle();
                updateClientDecoration();
                if (windowDecorationsStyle == JRootPane.PLAIN_DIALOG) {
                    /*
                     * Otherwise, the property change doesn't get fired when the dialog is a plain dialog.
                     * In this case JOptionPane#initDialog sets the window to be undecorated which should be avoided.
                     */
                    decorationStyleLock = true;
                    rootPane.setWindowDecorationStyle(JRootPane.FILE_CHOOSER_DIALOG);
                    decorationStyleLock = false;
                }
            }
        } else if (PropertyKey.ANCESTOR.equals(propertyName)) {
            updateWindow(rootPane.getParent());
        }
    }

    private void uninstallClientDecorations(final JRootPane root) {
        uninstallBorder(root);
        root.removeHierarchyListener(this);
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
            window.removeWindowListener(disposeListener);
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
        int style = windowDecorationsStyle < 0 ? root.getWindowDecorationStyle() : windowDecorationsStyle;
        updateWindow(root.getParent());
        CustomTitlePane titlePane = Decorations.createTitlePane(root, style, window);
        setWindowDecorated();
        installLayout(root);
        setTitlePane(root, titlePane);
        root.addHierarchyListener(this);
    }

    private void setWindowDecorated() {
        if (window instanceof Frame && !window.isDisplayable()) {
            ((Frame) window).setUndecorated(false);
        } else if (window instanceof Dialog && !window.isDisplayable()) {
            ((Dialog) window).setUndecorated(false);
        }
    }


    private void setTitlePane(final JRootPane root, final CustomTitlePane titlePane) {
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
        if (window != null) window.removeWindowListener(disposeListener);
        window = DarkUIUtil.getWindow(parent);
        if (window != null) window.addWindowListener(disposeListener);
    }

    private void installLayout(final JRootPane root) {
        if (layoutManager == null) {
            layoutManager = createLayoutManager();
        }
        oldLayout = root.getLayout();
        root.setLayout(layoutManager);
    }

    protected CustomTitlePane getTitlePane() {
        return titlePane;
    }

    protected LayoutManager createLayoutManager() {
        return new DarkSubstanceRootLayout();
    }

    @Override
    public void hierarchyChanged(final HierarchyEvent e) {
        if (rootPane == null) return;
        Component parent = rootPane.getParent();
        if (parent == null) {
            return;
        }
        if (parent.getClass().getName().startsWith("org.jdesktop.jdic.tray")
            || (parent.getClass().getName().equals("javax.swing.Popup$HeavyWeightWindow"))) {
            SwingUtilities.invokeLater(() -> {
                if (rootPane != null) {
                    rootPane.removeHierarchyListener(this);
                }
            });
        }
        if (e.getChangeFlags() == HierarchyEvent.PARENT_CHANGED) {
            if (DarkUIUtil.getWindow(rootPane) != window) {
                updateClientDecoration();
            }
        }
    }

    protected boolean isPopup(final JRootPane rootPane) {
        return Boolean.TRUE.equals(rootPane.getClientProperty(KEY_IS_POPUP));
    }

    protected void updateClientDecoration() {
        if (!Boolean.TRUE.equals(rootPane.getClientProperty(KEY_IS_MEDIUM_WEIGHT_POPUP_ROOT))) {
            uninstallClientDecorations(rootPane);
            if (Decorations.isCustomDecorationSupported() && !isPopup(rootPane)) {
                installClientDecorations(rootPane);
            }
        }
    }

    protected class DisposeListener extends WindowAdapter {
        @Override
        public void windowClosing(final WindowEvent e) {
            uninstallClientDecorations(rootPane);
        }
    }
}
