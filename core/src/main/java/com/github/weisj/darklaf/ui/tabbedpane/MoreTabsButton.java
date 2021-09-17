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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.uiresource.JButtonUIResource;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;

public class MoreTabsButton extends TabButtonContainer {

    protected static final String INFINITY = "\u221e";
    private final int pad;

    public MoreTabsButton(final DarkTabbedPaneUI ui) {
        super(ui);
        pad = UIManager.getInt("TabbedPane.moreTabsButton.pad");
        button.setMargin(new Insets(pad, pad, pad, pad));
    }

    @Override
    protected JButton createButton() {
        JButton button = new JButtonUIResource();
        button.setForeground(UIManager.getColor("TabbedPane.moreTabsButton.foreground"));
        button.setFont(UIManager.getFont("TabbedPane.moreTabsButton.font"));
        button.setIcon(new MoreTabsIcon(tabbedPaneUI.getMoreTabsIcon(), this));
        button.putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS);
        button.putClientProperty(DarkButtonUI.KEY_THIN, true);
        button.setFocusable(false);
        button.setRolloverEnabled(true);
        button.setOpaque(false);
        return button;
    }

    protected String getLabelString() {
        int invisible =
                Math.min(tabbedPaneUI.minVisible - 1 + tabbedPaneUI.tabPane.getTabCount() - tabbedPaneUI.maxVisible,
                        tabbedPaneUI.tabPane.getTabCount());
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
            String text = button.getLabelString();
            FontMetrics fm = button.getFontMetrics(button.getFont());
            int textWidth = fm.stringWidth(button.getLabelString());
            if (text.equals(INFINITY)) {
                // Ensure the text doesn't get smaller.
                textWidth = Math.max(textWidth, fm.stringWidth("00"));
            }
            return icon.getIconWidth() + textWidth + button.pad;
        }

        @Override
        public int getIconHeight() {
            return icon.getIconHeight();
        }
    }
}
