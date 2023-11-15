/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package com.github.weisj.darklaf.components.tabframe;

import java.awt.*;

import javax.swing.*;
import javax.swing.border.Border;

/**
 * Holder component.
 *
 * @author Jannis Weis
 */
public class PopupContainer extends JPanel {

    private Component popup;

    public PopupContainer() {
        super(new BorderLayout());
        super.setBorder(null);
        setFocusable(true);
    }


    public Component getPopup() {
        return popup;
    }

    public void setPopup(final Component component) {
        removeAll();
        if (component instanceof JComponent c) {
            setActionMap(c.getActionMap());
            setInputMap(JComponent.WHEN_FOCUSED, c.getInputMap(JComponent.WHEN_FOCUSED));
            setInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT,
                    c.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT));
            setInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW, c.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW));
        }

        add(component, BorderLayout.CENTER);
        this.popup = component;
        revalidate();
        repaint();
        component.doLayout();
    }

    @Override
    public void setBorder(final Border border) {
        super.setBorder(null);
    }

    @Override
    public Border getBorder() {
        return null;
    }
}
