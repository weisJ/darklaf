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
package com.github.weisj.darklaf.ui.splitbutton;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.github.weisj.darklaf.components.button.JSplitButton;
import com.github.weisj.darklaf.ui.WidgetPopupHelper;
import com.github.weisj.darklaf.ui.button.ButtonConstants;

public class DarkSplitButtonListener implements ActionListener, PropertyChangeListener, ChangeListener {


    private final DarkSplitButtonUI ui;

    public DarkSplitButtonListener(final DarkSplitButtonUI ui) {
        this.ui = ui;
    }

    @Override
    public void actionPerformed(final ActionEvent e) {
        if (e.getSource() == ui.splitButton && ui.useArrowButton()) return;
        JPopupMenu actionMenu = ui.splitButton.getActionMenu();
        if (actionMenu.isVisible()) {
            actionMenu.setVisible(false);
        } else {
            boolean splitButton = e.getSource() == ui.splitButton;
            actionMenu.setPreferredSize(null);
            Dimension size = actionMenu.getPreferredSize();
            Rectangle popupBounds =
                    WidgetPopupHelper.getPopupBounds(ui.splitButton, size, ui.splitButton.getSize(), splitButton,
                            !splitButton);
            if (splitButton) {
                actionMenu.setPreferredSize(popupBounds.getSize());
            }
            actionMenu.show(ui.splitButton, popupBounds.x, popupBounds.y);
        }
    }


    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (JSplitButton.KEY_ACTION_ADDED.equals(key) || JSplitButton.KEY_ACTION_REMOVED.equals(key)) {
            ui.updateDefaultAction();
            ui.splitButton.doLayout();
            ui.splitButton.repaint();
        } else if (ButtonConstants.KEY_THIN.equals(key)) {
            ui.updateArrowMargin();
        }
    }

    @Override
    public void stateChanged(final ChangeEvent e) {
        ui.splitButton.repaint();
        if (!ui.splitButton.hasFocus() && ui.arrowButton.getModel().isPressed()) {
            ui.splitButton.requestFocusInWindow();
        }
    }
}
