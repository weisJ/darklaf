/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.uiresource.JButtonUIResource;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;

public class NewTabButton extends TabButtonContainer {

    protected NewTabButton(final DarkTabbedPaneUI ui) {
        super(ui);
        int pad = UIManager.getInt("TabbedPane.newTabButton.pad");
        button.setMargin(new Insets(pad, pad, pad, pad));
        button.addActionListener(e -> {
            Action action = ui.getNewTabAction();
            if (action != null) {
                action.actionPerformed(e);
            }
        });
    }

    @Override
    protected JButton createButton() {
        JButton button = new JButtonUIResource();
        button.setIcon(tabbedPaneUI.getNewTabIcon());
        button.setFocusable(false);
        button.putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS);
        button.putClientProperty(DarkButtonUI.KEY_SQUARE, Boolean.TRUE);
        button.putClientProperty(DarkButtonUI.KEY_THIN, Boolean.TRUE);
        button.setRolloverEnabled(true);
        button.setOpaque(false);
        return button;
    }
}
