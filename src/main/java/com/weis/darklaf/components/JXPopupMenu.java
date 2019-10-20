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
package com.weis.darklaf.components;

import com.weis.darklaf.util.DarkUIUtil;
import org.jdesktop.jxlayer.JXLayer;
import org.pbjar.jxlayer.plaf.ext.TransformUI;

import javax.swing.*;
import java.awt.*;

/**
 * Popup menu that is aware of {@link org.pbjar.jxlayer.plaf.ext.TransformUI}.
 */
public class JXPopupMenu extends JPopupMenu {

    public JXPopupMenu() {
        this(null);
    }

    /**
     * Constructs a <code>JPopupMenu</code> with the specified title.
     *
     * @param label the string that a UI may use to display as a title for the popup menu.
     */
    public JXPopupMenu(final String label) {
        super(label);
    }

    @Override
    public void show(final Component invoker, final int x, final int y) {
        Point p = new Point(x, y);
        if (invoker != null) {
            JXLayer layer = DarkUIUtil.getParentOfType(JXLayer.class, invoker);
            if (layer != null && layer.getUI() instanceof TransformUI) {
                var ui = (TransformUI) layer.getUI();
                p = SwingUtilities.convertPoint(invoker, p, layer);
                //noinspection unchecked
                var transform = ui.getPreferredTransform(layer.getSize(), layer);
                transform.transform(p, p);
                super.show(layer, p.x, p.y);
                return;
            }
        }
        super.show(invoker, p.x, p.y);
    }
}
