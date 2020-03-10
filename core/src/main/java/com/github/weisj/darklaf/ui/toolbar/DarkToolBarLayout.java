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
 */
package com.github.weisj.darklaf.ui.toolbar;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.Serializable;

public class DarkToolBarLayout implements LayoutManager2, Serializable, PropertyChangeListener, UIResource {

    protected final JToolBar toolBar;
    protected BoxLayout lm;

    public DarkToolBarLayout(final JToolBar toolBar) {
        this.toolBar = toolBar;
        toolBar.addPropertyChangeListener(this);
        if (toolBar.getOrientation() == JToolBar.VERTICAL) {
            lm = new BoxLayout(toolBar, BoxLayout.PAGE_AXIS);
        } else {
            lm = new BoxLayout(toolBar, BoxLayout.LINE_AXIS);
        }
    }

    public void addLayoutComponent(final String name, final Component comp) {
        lm.addLayoutComponent(name, comp);
    }

    public void addLayoutComponent(final Component comp, final Object constraints) {
        lm.addLayoutComponent(comp, constraints);
    }

    public void removeLayoutComponent(final Component comp) {
        lm.removeLayoutComponent(comp);
    }

    public Dimension preferredLayoutSize(final Container target) {
        return lm.preferredLayoutSize(target);
    }

    public Dimension minimumLayoutSize(final Container target) {
        return lm.minimumLayoutSize(target);
    }

    public Dimension maximumLayoutSize(final Container target) {
        return lm.maximumLayoutSize(target);
    }

    public void layoutContainer(final Container target) {
        lm.layoutContainer(target);
        Dimension size = target.getSize();
        for (int i = 0; i < target.getComponentCount(); i++) {
            Component c = target.getComponent(i);
            Rectangle bounds = c.getBounds();
            if (c instanceof JSeparator) {
                if (toolBar.getOrientation() == JToolBar.HORIZONTAL) {
                    c.setBounds(bounds.x, size.height / 2 - bounds.height / 2, bounds.width, bounds.height);
                } else {
                    c.setBounds(size.width / 2 - bounds.width / 2, bounds.y, bounds.width, bounds.height);
                }
            }
        }
    }

    public float getLayoutAlignmentX(final Container target) {
        return lm.getLayoutAlignmentX(target);
    }

    public float getLayoutAlignmentY(final Container target) {
        return lm.getLayoutAlignmentY(target);
    }

    public void invalidateLayout(final Container target) {
        lm.invalidateLayout(target);
    }

    public void propertyChange(final PropertyChangeEvent e) {
        String name = e.getPropertyName();
        if (name.equals("orientation")) {
            int o = (Integer) e.getNewValue();
            if (o == JToolBar.VERTICAL) {
                lm = new BoxLayout(toolBar, BoxLayout.PAGE_AXIS);
            } else {
                lm = new BoxLayout(toolBar, BoxLayout.LINE_AXIS);
            }
        }
    }
}
