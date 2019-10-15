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

import com.weis.darklaf.components.JPanelUIResource;
import com.weis.darklaf.components.border.MutableLineBorder;
import com.weis.darklaf.components.tabframe.TabbedPopup;
import com.weis.darklaf.defaults.DarkColors;
import com.weis.darklaf.ui.tabbedpane.DarkTabbedPaneUI;
import com.weis.darklaf.ui.tabbedpane.NewTabButton;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import java.awt.*;

public class DarkTabbedPopupUI extends DarkPanelPopupUI {


    protected Color headerHoverBackground;
    protected Color headerSelectedBackground;
    protected Color headerSelectedHoverBackground;
    protected Color headerFocusHoverBackground;
    protected Color headerFocusSelectedBackground;
    protected Color headerFocusSelectedHoverBackground;
    private TabbedPopup popupComponent;
    private JTabbedPane tabbedPane;
    private MutableLineBorder border;
    private JPanel holder;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkTabbedPopupUI();
    }

    @Override
    public void installUI(final JComponent c) {
        popupComponent = (TabbedPopup) c;
        super.installUI(c);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        headerHoverBackground = DarkColors.get().getTabFramePopupHeaderHoverBackground();
        headerSelectedBackground = DarkColors.get().getTabFramePopupHeaderSelectedBackground();
        headerSelectedHoverBackground = DarkColors.get().getTabFramePopupHeaderSelectedHoverBackground();
        headerFocusHoverBackground = DarkColors.get().getTabFramePopupHeaderFocusHoverBackground();
        headerFocusSelectedBackground = DarkColors.get().getTabFramePopupHeaderFocusSelectedBackground();
        headerFocusSelectedHoverBackground = DarkColors.get().getTabFramePopupHeaderFocusSelectedHoverBackground();
    }

    @Override
    protected void installComponents() {
        closeButton = createCloseButton();
        label = createLabel();
        tabbedPane = createTabbedPane();
        setupTabbedPane();
        border = createBorder();
        holder = new JPanel(new BorderLayout());
        holder.add(tabbedPane, BorderLayout.CENTER);
        holder.setBorder(border);
        popupComponent.add(holder);
    }

    protected JTabbedPane createTabbedPane() {
        var tabbedPane = popupComponent.getTabbedPane();
        tabbedPane.setUI(new DarkTabFrameTabbedPaneUI());
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        return tabbedPane;
    }

    protected void setupTabbedPane() {
        label.setBorder(new EmptyBorder(0, 5, 0, 5));
        var buttonHolder = new JPanelUIResource();
        buttonHolder.setLayout(new BoxLayout(buttonHolder, BoxLayout.X_AXIS));
        buttonHolder.add(Box.createHorizontalStrut(1));
        buttonHolder.add(closeButton);
        buttonHolder.add(Box.createHorizontalStrut(1));
        buttonHolder.setOpaque(false);
        tabbedPane.setOpaque(false);
        tabbedPane.putClientProperty("JTabbedPane.leadingComponent", label);
        tabbedPane.putClientProperty("JTabbedPane.trailingComponent", buttonHolder);
    }

    @Override
    protected void uninstallComponents() {
        super.uninstallComponents();
        popupComponent.remove(tabbedPane);
    }

    @Override
    protected void applyBorderInsets(@NotNull final Insets insets) {
        border.setInsets(insets.top, insets.left, insets.bottom, insets.right);
    }

    @Override
    protected void setHeaderBackground(final boolean focus) {
        var oldFocus = hasFocus();
        super.setHeaderBackground(focus);
        if (oldFocus != focus) {
            holder.setBackground(focus ? headerFocusBackground : headerBackground);
            holder.repaint();
        }
    }

    protected class DarkTabFrameTabbedPaneUI extends DarkTabbedPaneUI {

        protected Color getTabBackgroundColor(final int tabIndex, final boolean isSelected, final boolean hover) {
            if (hasFocus()) {
                if (isSelected) {
                    return hover ? headerFocusSelectedHoverBackground
                                 : headerFocusSelectedBackground;
                } else {
                    return hover ? headerFocusHoverBackground
                                 : headerFocusBackground;
                }
            } else {
                if (isSelected) {
                    return hover ? headerSelectedHoverBackground
                                 : headerSelectedBackground;
                } else {
                    return hover ? headerHoverBackground
                                 : headerBackground;
                }
            }
        }

        @Override
        protected Color getTabBorderColor() {
            return DarkColors.get().getTabFramePopupBorderColor();
        }

        @Override
        protected Color getAccentColor(final boolean focus) {
            return super.getAccentColor(focus || hasFocus());
        }

        @Override
        public JComponent createNewTabButton() {
            return new TabFrameNewTabButton(this);
        }

        @Override
        public JButton createMoreTabsButton() {
            return super.createMoreTabsButton();
        }
    }

    protected class TabFrameNewTabButton extends NewTabButton {

        protected TabFrameNewTabButton(@NotNull final DarkTabbedPaneUI ui) {
            super(ui);
        }

        @Override
        protected JButton createButton() {
            var button = new HeaderButton(ui.getNewTabIcon(), DarkTabbedPopupUI.this);
            button.putClientProperty("JButton.variant", "shadow");
            button.putClientProperty("JButton.buttonType", "square");
            button.putClientProperty("JButton.thin", Boolean.TRUE);
            button.setRolloverEnabled(true);
            button.setOpaque(false);
            return button;
        }
    }
}
