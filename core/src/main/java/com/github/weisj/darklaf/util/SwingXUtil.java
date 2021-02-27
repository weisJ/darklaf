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
package com.github.weisj.darklaf.util;

import java.awt.*;
import java.awt.geom.AffineTransform;

import javax.swing.*;

import org.jdesktop.jxlayer.JXLayer;
import org.pbjar.jxlayer.plaf.ext.TransformUI;

public final class SwingXUtil {

    @SuppressWarnings("unchecked")
    public static Point convertPointToParent(final Component source, final Point p) {
        JXLayer<? extends JComponent> layer = DarkUIUtil.getParentOfType(JXLayer.class, source);
        return convertPointToParent(source, layer, p);
    }

    public static <T extends JComponent> Point convertPointToParent(final Component source, final JXLayer<T> layer,
            final Point p) {
        if (layer != null && layer.getUI() instanceof TransformUI) {
            TransformUI ui = (TransformUI) layer.getUI();
            Point pos = SwingUtilities.convertPoint(source, p, layer);
            AffineTransform transform = ui.getPreferredTransform(layer.getSize(), layer);
            transform.transform(pos, pos);
            return pos;
        }
        return SwingUtilities.convertPoint(source, p, source.getParent());
    }
}
