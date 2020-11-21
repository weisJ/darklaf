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
package com.github.weisj.darklaf.components.button;

import java.awt.*;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.plaf.ButtonUI;

import com.github.weisj.darklaf.ui.WidgetPopupHelper;

public class SplitButtonFallbackUI extends ButtonUIDelegate {

    private JSplitButton button;

    private final ActionListener popupListener = e -> {
        boolean split = button.getActionCount() > 1;
        if (button.getActionCount() <= 1) {
            JPopupMenu actionMenu = button.getActionMenu();
            actionMenu.setPreferredSize(null);
            Dimension size = actionMenu.getPreferredSize();
            Rectangle popupBounds =
                    WidgetPopupHelper.getPopupBounds(button, size, button.getSize(), split, !split);
            if (split) {
                actionMenu.setPreferredSize(popupBounds.getSize());
            }
            actionMenu.show(button, popupBounds.x, popupBounds.y);
        }
    };

    public SplitButtonFallbackUI(final ButtonUI delegate) {
        super(delegate);
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        button = (JSplitButton) c;
        button.addActionListener(popupListener);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        super.uninstallUI(c);
        button.removeActionListener(popupListener);
        button = null;
    }
}
