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
package com.github.weisj.darklaf.ui.button;

import java.awt.*;
import java.awt.event.FocusEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;

import javax.swing.*;
import javax.swing.plaf.basic.BasicButtonListener;

public class DarkButtonListener extends BasicButtonListener {

    private final DarkButtonUI ui;

    public DarkButtonListener(final AbstractButton b, final DarkButtonUI ui) {
        super(b);
        this.ui = ui;
    }

    @Override
    public void focusGained(final FocusEvent e) {
        super.focusGained(e);
        ui.repaintNeighbours();
    }

    @Override
    public void focusLost(final FocusEvent e) {
        super.focusLost(e);
        ui.repaintNeighbours();
    }

    public void mouseEntered(final MouseEvent e) {
        AbstractButton b = (AbstractButton) e.getSource();
        ButtonModel model = b.getModel();
        if (!SwingUtilities.isLeftMouseButton(e)) {
            model.setRollover(true);
        }
        if (model.isPressed()) {
            model.setArmed(true);
        }
    }

    public void mouseExited(final MouseEvent e) {
        AbstractButton b = (AbstractButton) e.getSource();
        ButtonModel model = b.getModel();
        model.setRollover(false);
        model.setArmed(false);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        super.propertyChange(e);
        AbstractButton b = (AbstractButton) e.getSource();
        String key = e.getPropertyName();
        if (key.startsWith("JButton.")) {
            ui.updateMargins(b);
            b.doLayout();
            b.repaint();
        }
    }
}
