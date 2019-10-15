/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.tabframe;

import com.weis.darklaf.components.alignment.Alignment;
import com.weis.darklaf.components.tabframe.TabFrame;
import com.weis.darklaf.components.tabframe.TabFrameTabLabel;
import com.weis.darklaf.decorators.HoverListener;
import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.icons.RotatableIcon;
import com.weis.darklaf.ui.label.DarkLabelUI;
import com.weis.darklaf.util.DarkUIUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.SwingUtilities2;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
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
        public void mousePressed(final MouseEvent e) {
            if (SwingUtilities.isLeftMouseButton(e)) {
                tabComponent.getTabFrame().toggleTab(tabComponent.getOrientation(), tabComponent.getIndex(),
                                                     !tabComponent.isSelected());
            }
        }
    };
    private HoverListener hoverListener;
    private Color defaultFontColor;
    private Color selectedFontColor;
    private Color selectedColor;
    private Color hoverColor;
    private RotatableIcon rotatableIcon = new RotatableIcon();
    private Rectangle paintIconR = new Rectangle();
    private Rectangle paintTextR = new Rectangle();

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabFrameTabLabelUI();
    }

    @Override
    public void paint(@NotNull final Graphics g, final JComponent c) {
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
        uninstallAccelerator(tabComponent.getTabFrame());
        tabComponent.removeMouseListener(hoverListener);
        tabComponent.removeMouseListener(mouseListener);
        hoverListener = null;
        tabComponent = null;
    }

    @Override
    protected void installDefaults(final JLabel c) {
        super.installDefaults(c);
        tabComponent.setFont(UIManager.getFont("TabFrameTab.font"));
        tabComponent.setOpaque(true);
        defaultFontColor = DarkColors.get().getTabFrameTabForeground();
        selectedColor = DarkColors.get().getTabFrameTabSelectedBackground();
        hoverColor = DarkColors.get().getTabFrameTabHoverBackground();
        selectedFontColor = DarkColors.get().getTabFrameTabSelectedForeground();
        LookAndFeel.installBorder(c, "TabFrameTab.border");
    }

    @Override
    protected void installListeners(final JLabel c) {
        super.installListeners(c);
        hoverListener = new HoverListener(tabComponent);
        tabComponent.addMouseListener(hoverListener);
        tabComponent.addMouseListener(mouseListener);
        installAccelerator(tabComponent.getTabFrame());
    }

    protected void installAccelerator(final TabFrame tabFrame) {
        if (tabFrame == null) return;
        int acc = tabComponent.getAccelerator();
        if (acc < 0) return;
        tabFrame.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT)
                .put(KeyStroke.getKeyStroke(UIManager.getString("TabFrame.acceleratorKeyCode") + " " + acc),
                     "accelerator_" + acc);
        tabFrame.getActionMap().put("accelerator_" + acc, createAcceleratorAction(tabFrame));
    }

    protected Action createAcceleratorAction(final TabFrame tabFrame) {
        return new AbstractAction() {
            @Override
            public void actionPerformed(final ActionEvent e) {
                var a = tabComponent.getOrientation();
                var index = tabComponent.getIndex();
                if (!tabComponent.isSelected()) {
                    tabFrame.toggleTab(a, index, true);
                } else {
                    var popup = tabFrame.getPopupComponentAt(a, index);
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
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        var key = e.getPropertyName();
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
            if (e.getOldValue() instanceof TabFrame) {
                uninstallAccelerator((TabFrame) e.getOldValue());
            }
            if (e.getNewValue() instanceof TabFrame) {
                installAccelerator((TabFrame) e.getNewValue());
            }
        }
    }

    protected void updateText() {
        var title = tabComponent.getTitle();
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

    protected void uninstallAccelerator(final TabFrame tabFrame) {
        if (tabFrame == null) return;
        int acc = tabComponent.getAccelerator();
        String accAction = "accelerator_" + acc;
        tabFrame.getActionMap().remove(accAction);
    }

    public Color getBackground(@NotNull final TabFrameTabLabel tab) {
        return tab.isSelected()
               ? selectedColor
               : hoverListener.isHover() ? hoverColor : tab.getBackground();
    }

    protected Icon getIcon() {
        Icon icon = (tabComponent.isEnabled()) ? tabComponent.getIcon() : tabComponent.getDisabledIcon();
        rotatableIcon.setIcon(icon);
        if (rotatableIcon.getOrientation() == null) {
            rotatableIcon.setOrientation(mapOrientation(tabComponent.getOrientation()));
        }
        return rotatableIcon;
    }

    private String layout(@NotNull final JLabel label, final FontMetrics fm,
                          final int width, final int height) {
        Insets insets = label.getInsets(null);
        String text = label.getText();
        Rectangle paintViewR = new Rectangle();
        paintViewR.x = insets.left;
        paintViewR.y = insets.top;
        paintViewR.width = width - (insets.left + insets.right);
        paintViewR.height = height - (insets.top + insets.bottom);
        paintIconR.x = paintIconR.y = paintIconR.width = paintIconR.height = 0;
        paintTextR.x = paintTextR.y = paintTextR.width = paintTextR.height = 0;
        return layoutCL(label, fm, text, getIcon(), paintViewR, paintIconR, paintTextR);
    }

    protected Alignment mapOrientation(@NotNull final Alignment newValue) {
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
