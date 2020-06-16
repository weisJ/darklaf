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
 *
 */
package com.github.weisj.darklaf.ui.tabframe;

import java.awt.*;
import java.util.function.Supplier;

import javax.swing.*;
import javax.swing.border.EmptyBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.TabbedPaneUI;

import com.github.weisj.darklaf.components.border.MutableLineBorder;
import com.github.weisj.darklaf.components.tabframe.TabFrameTabbedPopupUI;
import com.github.weisj.darklaf.components.tabframe.TabbedPopup;
import com.github.weisj.darklaf.components.uiresource.JPanelUIResource;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.ui.tabbedpane.DarkTabAreaButton;
import com.github.weisj.darklaf.ui.tabbedpane.DarkTabbedPaneUI;
import com.github.weisj.darklaf.ui.tabbedpane.MoreTabsButton;
import com.github.weisj.darklaf.ui.tabbedpane.NewTabButton;

public class DarkTabbedPopupUI extends DarkPanelPopupUI implements TabFrameTabbedPopupUI {

    protected Color headerHoverBackground;
    protected Color headerSelectedBackground;
    protected Color headerSelectedHoverBackground;
    protected Color headerFocusHoverBackground;
    protected Color headerFocusSelectedBackground;
    protected Color headerFocusSelectedHoverBackground;
    protected Color tabBorderColor;
    private TabbedPopup popupComponent;
    private JTabbedPane tabbedPane;
    private MutableLineBorder border;
    private JPanel holder;
    private JButton newTabButton;

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
        headerHoverBackground = UIManager.getColor("TabFramePopup.headerHoverBackground");
        headerSelectedBackground = UIManager.getColor("TabFramePopup.headerSelectedBackground");
        headerSelectedHoverBackground = UIManager.getColor("TabFramePopup.headerSelectedHoverBackground");
        headerFocusHoverBackground = UIManager.getColor("TabFramePopup.headerFocusHoverBackground");
        headerFocusSelectedBackground = UIManager.getColor("TabFramePopup.headerFocusSelectedBackground");
        headerFocusSelectedHoverBackground = UIManager.getColor("TabFramePopup.headerFocusSelectedHoverBackground");
        tabBorderColor = UIManager.getColor("TabFramePopup.borderColor");
    }

    @Override
    protected void installComponents() {
        closeButton = createCloseButton();
        label = createLabel();
        tabbedPane = getTabbedPane();
        setupTabbedPane();
        border = createBorder();
        holder = new JPanel(new BorderLayout());
        holder.add(tabbedPane, BorderLayout.CENTER);
        holder.setBorder(border);
        popupComponent.add(holder);
    }

    protected JTabbedPane getTabbedPane() {
        JTabbedPane tabbedPane = popupComponent.getTabbedPane();
        tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
        if (tabbedPane instanceof UIDelegateTabbedPane) {
            ((UIDelegateTabbedPane) tabbedPane).setUiSupplier(DarkTabFrameTabbedPaneUI::new);
            tabbedPane.updateUI();
        } else {
            tabbedPane.setUI(new DarkTabFrameTabbedPaneUI());
        }
        return tabbedPane;
    }

    public JTabbedPane createTabbedPane() {
        return new UIDelegateTabbedPane(null);
    }

    protected void setupTabbedPane() {
        label.setBorder(new EmptyBorder(0, 5, 0, 5));
        JPanelUIResource buttonHolder = new JPanelUIResource();
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
    protected void setHeaderBackground(final boolean focus) {
        boolean oldFocus = hasFocus();
        super.setHeaderBackground(focus);
        if (oldFocus != focus) {
            if (newTabButton != null) {
                newTabButton.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, focus ? headerButtonFocusHoverBackground
                        : headerButtonHoverBackground);
                newTabButton.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, focus ? headerButtonFocusClickBackground
                        : headerButtonClickBackground);
                newTabButton.repaint();
            }
            holder.setBackground(focus ? headerFocusBackground : headerBackground);
            holder.repaint();
        }
    }

    @Override
    protected void uninstallComponents() {
        super.uninstallComponents();
        popupComponent.remove(tabbedPane);
    }

    @Override
    protected void applyBorderInsets(final Insets insets) {
        border.setInsets(insets.top, insets.left, insets.bottom, insets.right);
    }

    protected static class UIDelegateTabbedPane extends JTabbedPane {

        private Supplier<TabbedPaneUI> uiSupplier;

        protected UIDelegateTabbedPane(final Supplier<TabbedPaneUI> uiSupplier) {
            setUiSupplier(uiSupplier);
        }

        public void setUiSupplier(final Supplier<TabbedPaneUI> uiSupplier) {
            this.uiSupplier = uiSupplier;
        }

        @Override
        public void updateUI() {
            if (uiSupplier != null) {
                setUI(uiSupplier.get());
            } else {
                super.updateUI();
            }
        }
    }

    protected static class TabFrameMoreTabsButton extends MoreTabsButton {
        public TabFrameMoreTabsButton(final DarkTabbedPaneUI ui) {
            super(ui);
        }
    }

    protected class TabFrameNewTabButton extends NewTabButton {

        protected TabFrameNewTabButton(final DarkTabbedPaneUI ui) {
            super(ui);
        }

        public JButton getButton() {
            return button;
        }

        @Override
        protected JButton createButton() {
            HeaderButton button = new HeaderButton(ui.getNewTabIcon(), DarkTabbedPopupUI.this);
            button.putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS);
            button.putClientProperty(DarkButtonUI.KEY_SQUARE, true);
            button.putClientProperty(DarkButtonUI.KEY_THIN, Boolean.TRUE);
            button.setRolloverEnabled(true);
            button.setOpaque(false);
            return button;
        }
    }

    protected class DarkTabFrameTabbedPaneUI extends DarkTabbedPaneUI {

        @Override
        protected Color getAccentColor(final boolean focus) {
            return super.getAccentColor(focus || hasFocus());
        }

        @Override
        protected Color getTabBorderColor() {
            return tabBorderColor;
        }

        public Color getTabBackgroundColor(final int tabIndex, final boolean isSelected, final boolean hover) {
            if (hasFocus()) {
                if (isSelected) {
                    return hover ? headerFocusSelectedHoverBackground : headerFocusSelectedBackground;
                } else {
                    return hover ? headerFocusHoverBackground : headerFocusBackground;
                }
            } else {
                if (isSelected) {
                    return hover ? headerSelectedHoverBackground : headerSelectedBackground;
                } else {
                    return hover ? headerHoverBackground : headerBackground;
                }
            }
        }

        @Override
        protected Color getTabAreaBackground() {
            return hasFocus() ? headerFocusBackground : headerBackground;
        }

        @Override
        public JComponent createNewTabButton() {
            TabFrameNewTabButton b = new TabFrameNewTabButton(this);
            newTabButton = b.getButton();
            return b;
        }

        @Override
        public DarkTabAreaButton createMoreTabsButton() {
            return new TabFrameMoreTabsButton(this);
        }
    }
}
