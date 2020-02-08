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
import com.github.weisj.darklaf.components.tabframe.TabFrameTabContainer;
import com.github.weisj.darklaf.decorators.HoverListener;
import com.github.weisj.darklaf.ui.panel.DarkPanelUI;
import com.github.weisj.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkTabFrameTabContainerUI extends DarkPanelUI implements PropertyChangeListener {

    protected TabFrameTabContainer tabContainer;
    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mousePressed(final MouseEvent e) {
            if (SwingUtilities.isLeftMouseButton(e)) {
                tabContainer.getTabFrame().toggleTab(tabContainer.getOrientation(), tabContainer.getIndex(),
                                                     !tabContainer.isSelected());
            }
        }
    };
    private TabDragListener dragListener;
    private HoverListener hoverListener;
    private Color selectedColor;
    private Color hoverColor;
    private boolean printing;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabFrameTabContainerUI();
    }

    @Override
    public void installUI(final JComponent c) {
        tabContainer = (TabFrameTabContainer) c;
        super.installUI(c);
        installDefaults(tabContainer);
        installListeners();
        installAccelerator(tabContainer.getTabFrame());
    }

    protected void installListeners() {
        dragListener = new TabDragListener(tabContainer);
        hoverListener = new HoverListener(tabContainer);
        tabContainer.addMouseListener(hoverListener);
        tabContainer.addPropertyChangeListener(this);
        tabContainer.addMouseListener(mouseListener);
        tabContainer.addMouseListener(dragListener);
        tabContainer.addMouseMotionListener(dragListener);
        Component cont = tabContainer.getContent();
        if (cont != null) {
            cont.addMouseListener(hoverListener);
            cont.addMouseListener(mouseListener);
            cont.addMouseListener(dragListener);
            cont.addMouseMotionListener(dragListener);
        }
    }

    protected void installAccelerator(final JTabFrame tabFrame) {
        if (tabFrame == null) return;
        int acc = tabContainer.getAccelerator();
        if (acc < 0) return;
        tabFrame.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
                .put(KeyStroke.getKeyStroke(UIManager.getString("TabFrame.acceleratorKeyCode") + " " + acc),
                     "accelerator_" + acc);
        tabFrame.getActionMap().put("accelerator_" + acc, createAcceleratorAction(tabFrame));
    }

    protected Action createAcceleratorAction(final JTabFrame tabFrame) {
        return new AbstractAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                Alignment a = tabContainer.getOrientation();
                int index = tabContainer.getIndex();
                if (!tabContainer.isSelected()) {
                    tabFrame.toggleTab(a, index, true);
                } else {
                    Component popup = tabFrame.getPopupComponentAt(a, index);
                    if (!DarkUIUtil.hasFocus(popup)) {
                        popup.requestFocus();
                    } else {
                        tabFrame.toggleTab(a, index, false);
                    }
                }
            }
        };
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallListeners();
        uninstallAccelerator(tabContainer.getTabFrame());
        tabContainer = null;
    }

    protected void uninstallListeners() {
        tabContainer.removeMouseListener(hoverListener);
        tabContainer.removeMouseListener(mouseListener);
        tabContainer.removeMouseListener(dragListener);
        tabContainer.removePropertyChangeListener(this);
        tabContainer.removeMouseMotionListener(dragListener);
        Component cont = tabContainer.getContent();
        if (cont != null) {
            cont.removeMouseListener(hoverListener);
            cont.removeMouseListener(mouseListener);
            cont.removeMouseListener(dragListener);
            cont.removeMouseMotionListener(dragListener);
        }
        dragListener = null;
        hoverListener = null;
    }

    protected void uninstallAccelerator(final JTabFrame tabFrame) {
        if (tabFrame == null) return;
        int acc = tabContainer.getAccelerator();
        String accAction = "accelerator_" + acc;
        tabFrame.getActionMap().remove(accAction);
    }

    protected void installDefaults(final JPanel p) {
        super.installDefaults(p);
        tabContainer.setOpaque(true);
        selectedColor = UIManager.getColor("TabFrameTab.selectedBackground");
        hoverColor = UIManager.getColor("TabFrameTab.hoverBackground");
    }

    @Override
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if ("content".equals(key)) {
            Object oldVal = evt.getOldValue();
            Object newVal = evt.getNewValue();
            if (oldVal instanceof Component) {
                ((Component) oldVal).removeMouseListener(mouseListener);
                ((Component) oldVal).removeMouseListener(hoverListener);
                ((Component) oldVal).removeMouseListener(dragListener);
                ((Component) oldVal).removeMouseMotionListener(dragListener);
            }
            if (newVal instanceof Component) {
                ((Component) newVal).addMouseListener(mouseListener);
                ((Component) newVal).addMouseListener(hoverListener);
                ((Component) newVal).addMouseListener(dragListener);
                ((Component) newVal).addMouseMotionListener(dragListener);
            }
        } else if ("selected".equals(key)) {
            if (tabContainer == null) return;
            tabContainer.repaint();
        } else if ("accelerator".equals(key)) {
            if (tabContainer == null) return;
            uninstallAccelerator(tabContainer.getTabFrame());
            installAccelerator(tabContainer.getTabFrame());
        } else if ("tabFrame".equals(key)) {
            if (evt.getOldValue() instanceof JTabFrame) {
                uninstallAccelerator((JTabFrame) evt.getOldValue());
            }
            if (evt.getNewValue() instanceof JTabFrame) {
                installAccelerator((JTabFrame) evt.getNewValue());
            }
        } else if ("paintingForPrint".equals(key)) {
            printing = Boolean.TRUE.equals(evt.getNewValue());
        }
    }

    @Override
    public void paint(@NotNull final Graphics g, @NotNull final JComponent c) {
        g.setColor(getBackground(tabContainer));
        g.fillRect(0, 0, c.getWidth(), c.getHeight());
        super.paint(g, c);
    }

    public Color getBackground(@NotNull final TabFrameTabContainer tab) {
        if (printing) return tab.getBackground();
        return tab.isSelected()
               ? selectedColor : hoverListener.isHover() && !tab.getTabFrame().isInTransfer()
                                 ? hoverColor : tab.getBackground();
    }
}
