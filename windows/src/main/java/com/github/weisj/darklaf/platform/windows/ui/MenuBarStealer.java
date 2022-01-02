/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
package com.github.weisj.darklaf.platform.windows.ui;

import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.HierarchyEvent;
import java.awt.event.HierarchyListener;

import javax.swing.JComponent;
import javax.swing.JLayeredPane;
import javax.swing.JMenuBar;
import javax.swing.JRootPane;

import com.github.weisj.darklaf.util.PropertyUtil;

public class MenuBarStealer {

    private final JRootPane rootPane;
    private final JComponent target;
    private JMenuBar menuBar;

    private boolean unifiedMenuBar;

    private ContainerListener rootPaneContainerListener;
    private ContainerListener layeredPaneContainerListener;
    private HierarchyListener menuBarHierarchyListener;


    public MenuBarStealer(final JRootPane rootPane, final JComponent target) {
        this.rootPane = rootPane;
        this.target = target;
    }

    public void install() {
        updateMenuBar(true);
    }

    public void uninstall() {
        updateMenuBar(false);
    }

    public JMenuBar getMenuBar() {
        return menuBar;
    }

    public boolean hasMenuBar() {
        return menuBar != null;
    }

    public boolean isMenuBarEmpty() {
        return !unifiedMenuBar || menuBar == null || !menuBar.isVisible();
    }

    public void updateMenuBar(final boolean install) {
        unifiedMenuBar = PropertyUtil.getBooleanProperty(rootPane, "JRootPane.unifiedMenuBar");
        if (unifiedMenuBar && install) {
            if (rootPaneContainerListener == null) {
                rootPaneContainerListener = createRootPaneContainerListener();
            }
            if (layeredPaneContainerListener == null) {
                layeredPaneContainerListener = createLayeredPaneContainerListener();
            }
            rootPane.addContainerListener(rootPaneContainerListener);
            rootPane.getLayeredPane().addContainerListener(layeredPaneContainerListener);
            addMenuBar(rootPane.getJMenuBar());
        } else {
            rootPane.removeContainerListener(rootPaneContainerListener);
            rootPane.getLayeredPane().removeContainerListener(layeredPaneContainerListener);
            if (menuBar != null) {
                returnMenuBar();
            }
        }
        rootPane.revalidate();
    }

    private void addMenuBar(final JMenuBar bar) {
        if (bar != null && unifiedMenuBar) {
            if (bar.getParent() != rootPane.getLayeredPane()) {
                return;
            }
            if (menuBarHierarchyListener == null) {
                menuBarHierarchyListener = e -> {
                    if (((e.getChangeFlags() & HierarchyEvent.PARENT_CHANGED) != 0)
                            && menuBar != null
                            && menuBar.getParent() != null
                            && menuBar.getParent() != target) {
                        removeMenuBar();
                    }
                };
            }
            this.menuBar = bar;
            menuBar.addHierarchyListener(menuBarHierarchyListener);
            menuBar.setOpaque(false);
            target.add(menuBar);
            target.setComponentZOrder(menuBar, target.getComponentCount() - 1);
            target.revalidate();
            target.repaint();
        }
    }

    private void returnMenuBar() {
        JMenuBar oldMenuBar = menuBar;
        removeMenuBar();
        rootPane.setJMenuBar(oldMenuBar);
    }

    private void removeMenuBar() {
        target.remove(menuBar);
        menuBar.removeHierarchyListener(menuBarHierarchyListener);
        menuBarHierarchyListener = null;
        menuBar.setOpaque(true);
        menuBar = null;
    }

    private ContainerListener createLayeredPaneContainerListener() {
        return new ContainerListener() {
            @Override
            public void componentAdded(final ContainerEvent e) {
                if (e.getChild() instanceof JMenuBar) {
                    addMenuBar(rootPane.getJMenuBar());
                }
                if (rootPane.getJMenuBar() == null && menuBar != null) {
                    removeMenuBar();
                }
            }

            @Override
            public void componentRemoved(final ContainerEvent e) {}
        };
    }

    private ContainerListener createRootPaneContainerListener() {
        return new ContainerListener() {
            @Override
            public void componentAdded(final ContainerEvent e) {
                if (e.getChild() instanceof JLayeredPane) {
                    ((JLayeredPane) e.getChild()).addContainerListener(layeredPaneContainerListener);
                }
            }

            @Override
            public void componentRemoved(final ContainerEvent e) {
                if (e.getChild() instanceof JLayeredPane) {
                    ((JLayeredPane) e.getChild()).removeContainerListener(layeredPaneContainerListener);
                }
            }
        };
    }
}
