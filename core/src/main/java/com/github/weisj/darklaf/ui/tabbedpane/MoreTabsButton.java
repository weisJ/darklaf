/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.button.DarkButtonUI;

public class MoreTabsButton extends DarkTabAreaButton {

    protected final static String INFINITY = "\u221e";
    protected final Icon icon;
    protected final DarkTabbedPaneUI ui;
    protected final int pad;

    public MoreTabsButton(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
        setLayout(null);
        icon = ui.getMoreTabsIcon();
        setForeground(UIManager.getColor("TabbedPane.moreTabsButton.foreground"));
        pad = UIManager.getInt("TabbedPane.moreTabsButton.pad");
        setFont(UIManager.getFont("TabbedPane.moreTabsButton.font"));
        setIcon(new MoreTabsIcon(icon, this));
        putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS);
        putClientProperty(DarkButtonUI.KEY_SQUARE, true);
        putClientProperty(DarkButtonUI.KEY_THIN, true);
        setMargin(new Insets(pad, pad, pad, pad));
        setFocusable(false);
        setOpaque(true);
    }

    @Override
    public Color getBackground() {
        if (ui == null) return super.getBackground();
        return ui.getTabAreaBackground();
    }

    protected String getLabelString() {
        int invisible =
            Math.min(ui.minVisible - 1 + ui.tabPane.getTabCount() - ui.maxVisible, ui.tabPane.getTabCount());
        return invisible >= 100 ? INFINITY : String.valueOf(invisible);
    }

    protected static class MoreTabsIcon implements Icon {

        private final Icon icon;
        private final MoreTabsButton button;

        protected MoreTabsIcon(final Icon icon, final MoreTabsButton button) {
            this.icon = icon;
            this.button = button;
        }

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
            icon.paintIcon(c, g, x, y + getIconHeight() - icon.getIconHeight());
            String text = button.getLabelString();
            g.setColor(button.getForeground());
            g.drawString(text, x + icon.getIconWidth() + button.pad, y + icon.getIconHeight());
        }

        @Override
        public int getIconWidth() {
            int textWidth = button.getFontMetrics(button.getFont()).stringWidth(button.getLabelString());
            return icon.getIconWidth() + textWidth + button.pad;
        }

        @Override
        public int getIconHeight() {
            return icon.getIconHeight() + button.pad;
        }
    }
}
