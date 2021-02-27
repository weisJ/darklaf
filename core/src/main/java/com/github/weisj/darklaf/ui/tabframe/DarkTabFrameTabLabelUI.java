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
 *
 */
package com.github.weisj.darklaf.ui.tabframe;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;

import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import com.github.weisj.darklaf.components.tabframe.TabFrameTabLabel;
import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.listener.HoverListener;
import com.github.weisj.darklaf.ui.label.DarkLabelUI;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.PropertyKey;

public class DarkTabFrameTabLabelUI extends DarkLabelUI implements PropertyChangeListener {

    private TabFrameTabLabel tabComponent;
    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseClicked(final MouseEvent e) {
            if (!tabComponent.isEnabled()) return;
            if (SwingUtilities.isLeftMouseButton(e)) {
                tabComponent.getTabFrame().toggleTab(tabComponent.getOrientation(), tabComponent.getIndex(),
                        !tabComponent.isSelected());
            }
        }
    };
    private TabDragListener dragListener;
    private HoverListener hoverListener;
    private Color defaultFontColor;
    private Color selectedFontColor;
    private Color fontHoverColor;
    private Color selectedColor;
    private Color hoverColor;
    private final RotatableIcon rotatableIcon = new RotatableIcon();
    private boolean printing;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabFrameTabLabelUI();
    }

    @Override
    protected void paintBackground(final Graphics g, final JComponent c) {
        g.setColor(getBackground(tabComponent));
        g.fillRect(0, 0, tabComponent.getWidth(), tabComponent.getHeight());
    }

    @Override
    public void installUI(final JComponent c) {
        tabComponent = (TabFrameTabLabel) c;
        super.installUI(c);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        uninstallListeners(tabComponent);
        if (tabComponent.getComponentPopupMenu() instanceof UIResource) {
            tabComponent.setComponentPopupMenu(null);
        }
        hoverListener = null;
        tabComponent = null;
    }

    @Override
    protected void installDefaults(final JLabel c) {
        super.installDefaults(c);
        tabComponent.setFont(UIManager.getFont("TabFrameTab.font"));
        tabComponent.setOpaque(true);
        defaultFontColor = UIManager.getColor("TabFrameTab.foreground");
        selectedColor = UIManager.getColor("TabFrameTab.selectedBackground");
        hoverColor = UIManager.getColor("TabFrameTab.hoverBackground");
        selectedFontColor = UIManager.getColor("TabFrameTab.selectedForeground");
        fontHoverColor = UIManager.getColor("TabFrameTab.hoverForeground");
        LookAndFeel.installBorder(c, "TabFrameTab.border");
        tabComponent.setComponentPopupMenu(new DarkTabFrameComponentPopupMenu(tabComponent));
    }

    @Override
    protected void installListeners(final JLabel c) {
        super.installListeners(c);
        dragListener = new TabDragListener(tabComponent);
        hoverListener = new HoverListener(tabComponent);
        tabComponent.addMouseListener(hoverListener);
        tabComponent.addMouseListener(mouseListener);
        installAccelerator(tabComponent.getTabFrame());
        tabComponent.addMouseMotionListener(dragListener);
        tabComponent.addMouseListener(dragListener);
    }

    @Override
    protected void uninstallListeners(final JLabel c) {
        super.uninstallListeners(c);
        tabComponent.removeMouseListener(hoverListener);
        tabComponent.removeMouseListener(mouseListener);
        uninstallAccelerator(tabComponent.getTabFrame());
        tabComponent.removeMouseMotionListener(dragListener);
        tabComponent.removeMouseListener(dragListener);
        dragListener = null;
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        String key = e.getPropertyName();
        if (TabFrameTab.KEY_SELECTED.equals(key)) {
            tabComponent.setForeground(Boolean.TRUE.equals(e.getNewValue()) ? selectedFontColor : defaultFontColor);
            tabComponent.repaint();
        } else if (TabFrameTab.KEY_TITLE.equals(key)) {
            updateText();
        } else if (TabFrameTab.KEY_ACCELERATOR.equals(key)) {
            updateText();
            if (tabComponent == null) return;
            uninstallAccelerator(tabComponent.getTabFrame());
            installAccelerator(tabComponent.getTabFrame());
        } else if (TabFrameTab.KEY_ORIENTATION.equals(key)) {
            rotatableIcon.setOrientation(mapOrientation(tabComponent.getOrientation()));
            hoverListener.refresh();
        } else if (TabFrameTab.KEY_TAB_FRAME_PARENT.equals(key)) {
            if (e.getOldValue() instanceof JTabFrame) {
                uninstallAccelerator((JTabFrame) e.getOldValue());
            }
            if (e.getNewValue() instanceof JTabFrame) {
                installAccelerator((JTabFrame) e.getNewValue());
            }
        } else if (PropertyKey.PAINTING_FOR_PRINT.equals(key)) {
            printing = Boolean.TRUE.equals(e.getNewValue());
        }
    }

    protected void updateText() {
        String title = tabComponent.getTitle();
        title = title == null ? "" : title;
        int accelerator = tabComponent.getAccelerator();
        if (accelerator >= 0 && accelerator <= 9) {
            tabComponent.setText(accelerator + ":" + title);
            tabComponent.setDisplayedMnemonicIndex(0);
        } else {
            tabComponent.setText(title);
            tabComponent.setDisplayedMnemonicIndex(1);
        }
    }

    protected void installAccelerator(final JTabFrame tabFrame) {
        TabFrameUtil.installAccelerator(tabFrame, tabComponent);
    }

    protected void uninstallAccelerator(final JTabFrame tabFrame) {
        TabFrameUtil.uninstallAccelerator(tabFrame, tabComponent);
    }

    public Color getBackground(final TabFrameTabLabel tab) {
        if (printing || !tab.isEnabled()) return tab.getBackground();
        return tab.isSelected() ? selectedColor
                : hoverListener.isHover() && !tab.getTabFrame().isInTransfer() ? hoverColor : tab.getBackground();
    }

    @Override
    protected Color getEnabledForeground(final Component label) {
        return getTabForeground((TabFrameTabLabel) label);
    }

    public Color getTabForeground(final TabFrameTabLabel tab) {
        if (printing) return tab.getForeground();
        return tab.isSelected() ? selectedFontColor
                : hoverListener.isHover() && !tab.getTabFrame().isInTransfer() ? fontHoverColor : tab.getForeground();
    }

    @Override
    protected Icon getIcon(final JLabel label) {
        Icon icon = (tabComponent.isEnabled()) ? tabComponent.getIcon() : tabComponent.getDisabledIcon();
        if (icon == null) return null;
        rotatableIcon.setIcon(icon);
        if (rotatableIcon.getOrientation() == null) {
            rotatableIcon.setOrientation(mapOrientation(tabComponent.getOrientation()));
        }
        return rotatableIcon;
    }

    protected Alignment mapOrientation(final Alignment newValue) {
        switch (newValue) {
            case CENTER:
            case NORTH:
            case NORTH_EAST:
            case SOUTH:
            case SOUTH_WEST:
                return Alignment.NORTH;
            case EAST:
            case SOUTH_EAST:
                return Alignment.WEST;
            case WEST:
            case NORTH_WEST:
                return Alignment.EAST;
        }
        return Alignment.NORTH;
    }
}
