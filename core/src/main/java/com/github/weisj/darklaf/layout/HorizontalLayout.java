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
package com.github.weisj.darklaf.layout;

import java.awt.*;

public class HorizontalLayout implements LayoutManager {
    private int gap = 0;

    public HorizontalLayout() {}

    public HorizontalLayout(final int gap) {
        this.gap = gap;
    }

    public int getGap() {
        return gap;
    }

    public void setGap(final int gap) {
        this.gap = gap;
    }

    @Override
    public void addLayoutComponent(final String name, final Component c) {}

    @Override
    public void layoutContainer(final Container parent) {
        Insets insets = parent.getInsets();
        Dimension size = parent.getSize();
        int height = size.height - insets.top - insets.bottom;
        int width = insets.left;
        for (int i = 0, c = parent.getComponentCount(); i < c; i++) {
            Component m = parent.getComponent(i);
            if (m.isVisible()) {
                m.setBounds(width, insets.top,
                        m.getPreferredSize().width, height);
                width += m.getSize().width + gap;
            }
        }
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        return preferredLayoutSize(parent);
    }

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        Insets insets = parent.getInsets();
        Dimension pref = new Dimension(0, 0);
        for (int i = 0, c = parent.getComponentCount(); i < c; i++) {
            Component m = parent.getComponent(i);
            if (m.isVisible()) {
                Dimension componentPreferredSize =
                        parent.getComponent(i).getPreferredSize();
                pref.height = Math.max(pref.height, componentPreferredSize.height);
                pref.width += componentPreferredSize.width + gap;
            }
        }
        pref.width += insets.left + insets.right;
        pref.height += insets.top + insets.bottom;
        return pref;
    }

    @Override
    public void removeLayoutComponent(final Component c) {}
}
