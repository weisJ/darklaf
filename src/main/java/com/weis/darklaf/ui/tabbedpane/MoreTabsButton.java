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
package com.weis.darklaf.ui.tabbedpane;

import com.weis.darklaf.icons.EmptyIcon;
import com.weis.darklaf.util.GraphicsUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;

public class MoreTabsButton extends DarkTabAreaButton {

    protected final static String INFINITY = "\u221e";
    protected final static int PAD = 2;
    protected final Icon icon;
    protected DarkTabbedPaneUI ui;

    public MoreTabsButton(final DarkTabbedPaneUI ui) {
        super(ui);
        this.ui = ui;
        icon = ui.getMoreTabsIcon();
        setIcon(EmptyIcon.create(icon.getIconWidth(), icon.getIconHeight()));
        putClientProperty("JButton.variant", "onlyLabel");
        putClientProperty("JButton.buttonType", "square");
        setFont(getFont().deriveFont(8f));
    }

    @Override
    public Color getBackground() {
        if (ui == null) return super.getBackground();
        return ui.getTabAreaBackground();
    }

    protected void paintButton(@NotNull final Graphics g) {
        FontMetrics metrics = g.getFontMetrics();
        String label = getLabelString();
        int labelWidth = metrics.stringWidth(label);
        int x = (getWidth() - (icon.getIconWidth() + labelWidth + PAD)) / 2;
        int y = (getHeight() - icon.getIconHeight()) / 2;


        var config = GraphicsUtil.setupAAPainting(g);
        /*
         * These offsets are due to the nature of the used icon. They are applied to match the baseline of
         * the label.properties text.
         * A different icon might need a different offset.
         */
        int off = 5;
        int tabPlacement = ui.tabPane.getTabPlacement();
        if (tabPlacement == TOP) {
            y += 2;
        } else if (tabPlacement == BOTTOM) {
            y -= 1;
        }

        icon.paintIcon(this, g, x, y);
        config.restore();
        config = GraphicsUtil.setupAntialiasing(g);
        g.drawString(label, x + icon.getIconWidth() + PAD, y + icon.getIconHeight() - off);
        config.restore();
    }

    @NotNull
    protected String getLabelString() {
        int invisible = Math.min(ui.minVisible - 1 + ui.tabPane.getTabCount() - ui.maxVisible, ui.tabPane.getTabCount());
        return invisible >= 100 ? INFINITY : String.valueOf(invisible);
    }

    @Override
    public Dimension getPreferredSize() {
        var size = super.getPreferredSize();
        var metrics = getFontMetrics(getFont());
        size.width += metrics.stringWidth(getLabelString()) + PAD;
        return size;
    }
}
