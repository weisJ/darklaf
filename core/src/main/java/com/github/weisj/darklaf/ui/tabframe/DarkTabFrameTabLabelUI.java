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

import com.github.weisj.darklaf.components.tabframe.JTabFrame;
import com.github.weisj.darklaf.components.tabframe.TabFrameTabLabel;
import com.github.weisj.darklaf.decorators.HoverListener;
import com.github.weisj.darklaf.icons.RotatableIcon;
import com.github.weisj.darklaf.ui.label.DarkLabelUI;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkTabFrameTabLabelUI extends DarkLabelUI implements PropertyChangeListener {

    private TabFrameTabLabel tabComponent;
    private final MouseListener mouseListener = new MouseAdapter() {
        @Override
        public void mouseClicked(final MouseEvent e) {
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
    private Color selectedColor;
    private Color hoverColor;
    private RotatableIcon rotatableIcon = new RotatableIcon();
    private boolean printing;


    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabFrameTabLabelUI();
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        g.setColor(getBackground(tabComponent));
        g.fillRect(0, 0, tabComponent.getWidth(), tabComponent.getHeight());

        JLabel label = (JLabel) c;
        String text = label.getText();
        Icon icon = getIcon();

        if ((icon == null) && (text == null)) {
            return;
        }

        FontMetrics fm = SwingUtilities2.getFontMetrics(label, g);
        String clippedText = layout(label, fm, c.getWidth(), c.getHeight());

        if (icon != null) {
            icon.paintIcon(c, g, paintIconR.x, paintIconR.y);
        }

        if (text != null) {
            View v = (View) c.getClientProperty(BasicHTML.propertyKey);
            if (v != null) {
                v.paint(g, paintTextR);
            } else {
                int textX = paintTextR.x;
                int textY = paintTextR.y + fm.getAscent();

                if (label.isEnabled()) {
                    paintEnabledText(label, g, clippedText, textX, textY);
                } else {
                    paintDisabledText(label, g, clippedText, textX, textY);
                }
            }
        }
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
        if ("selected".equals(key)) {
            tabComponent.setForeground(Boolean.TRUE.equals(e.getNewValue())
                                       ? selectedFontColor : defaultFontColor);
            tabComponent.repaint();
        } else if ("title".equals(key)) {
            updateText();
        } else if ("accelerator".equals(key)) {
            updateText();
            if (tabComponent == null) return;
            uninstallAccelerator(tabComponent.getTabFrame());
            installAccelerator(tabComponent.getTabFrame());
        } else if ("orientation".equals(key)) {
            rotatableIcon.setOrientation(mapOrientation(tabComponent.getOrientation()));
        } else if ("tabFrame".equals(key)) {
            if (e.getOldValue() instanceof JTabFrame) {
                uninstallAccelerator((JTabFrame) e.getOldValue());
            }
            if (e.getNewValue() instanceof JTabFrame) {
                installAccelerator((JTabFrame) e.getNewValue());
            }
        } else if ("paintingForPrint".equals(key)) {
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
        if (tabFrame == null) return;
        int acc = tabComponent.getAccelerator();
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
                Alignment a = tabComponent.getOrientation();
                int index = tabComponent.getIndex();
                if (!tabComponent.isSelected()) {
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

    protected void uninstallAccelerator(final JTabFrame tabFrame) {
        if (tabFrame == null) return;
        int acc = tabComponent.getAccelerator();
        String accAction = "accelerator_" + acc;
        tabFrame.getActionMap().remove(accAction);
    }

    public Color getBackground(final TabFrameTabLabel tab) {
        if (printing) return tab.getBackground();
        return tab.isSelected()
               ? selectedColor : hoverListener.isHover() && !tab.getTabFrame().isInTransfer()
                                 ? hoverColor : tab.getBackground();
    }

    protected Icon getIcon() {
        Icon icon = (tabComponent.isEnabled()) ? tabComponent.getIcon() : tabComponent.getDisabledIcon();
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
