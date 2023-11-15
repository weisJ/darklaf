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
package org.pbjar.jxlayer.plaf.ext.transform;

import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.Rectangle2D;

import javax.swing.*;
import javax.swing.plaf.LayerUI;

import org.pbjar.jxlayer.plaf.ext.TransformUI;

/**
 * A specialized layout manager for {@link JLayer} in combination with the {@link TransformUI}.
 * <p>
 * It extends {@link DefaultLayerLayout} and, as long as no enabled {@link TransformUI} is set to
 * {@link JLayer}, will act exactly the same as its super class.
 * <p>
 * However, when the above conditions are all true, its behavior becomes different:
 * <ol>
 * <li>Instead of setting the view's size to the layer's calculated inner area, it will set the
 * view's size to its preferred size.
 * <li>Instead of setting the view's bounds to the calculated inner area, it will center the view in
 * that inner area. This may result in some parts of the view formally obscured, or, some parts of
 * the inner area not covered by the view.
 * <li>The preferred size will first be computed by the super implementation. Then, before
 * returning, the calculated size will be transformed with the {@link AffineTransform} returned by
 * {@link TransformUI#getPreferredTransform(Dimension, JLayer)};
 * <li>The minimum size will first be computed by the super implementation. Then, before returning,
 * the calculated size will be transformed with the {@link AffineTransform} returned by
 * {@link TransformUI#getPreferredTransform(Dimension, JLayer)};
 * </ol>
 *
 * @see JLayer#getView()
 * @see JLayer#getGlassPane()
 * @see TransformUI
 */
public class TransformLayout extends DefaultLayerLayout {

    /**
     * Overridden to apply a preferred transform on the {@link Dimension} object returned from the super
     * implementation.
     */
    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        return transform(parent, super.preferredLayoutSize(parent));
    }

    /**
     * Overridden to apply a preferred transform on the {@link Dimension} object returned from the super
     * implementation.
     */
    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        return transform(parent, super.minimumLayoutSize(parent));
    }

    /**
     * Overridden to apply a different layout when the {@link LayerUI} is an instance of
     * {@link TransformUI}. If this is not the case, the super implementation will be invoked.
     */
    @Override
    public void layoutContainer(final Container parent) {
        JLayer<?> layer = (JLayer<?>) parent;
        LayerUI<?> layerUI = layer.getUI();
        if (layerUI instanceof TransformUI) {
            JComponent view = (JComponent) layer.getView();
            JComponent glassPane = layer.getGlassPane();
            if (view != null) {
                Rectangle innerArea = new Rectangle();
                SwingUtilities.calculateInnerArea(layer, innerArea);
                view.setSize(view.getPreferredSize());
                Rectangle viewRect = new Rectangle(0, 0, view.getWidth(), view.getHeight());
                int x = (int) Math.round(innerArea.getCenterX() - viewRect.getCenterX());
                int y = (int) Math.round(innerArea.getCenterY() - viewRect.getCenterY());
                viewRect.translate(x, y);
                view.setBounds(viewRect);
            }
            if (glassPane != null) {
                glassPane.setLocation(0, 0);
                glassPane.setSize(layer.getWidth(), layer.getHeight());
            }
            return;
        }
        super.layoutContainer(parent);
    }

    @SuppressWarnings("unchecked")
    private Dimension transform(final Container parent, final Dimension size) {
        JLayer<JComponent> layer = (JLayer<JComponent>) parent;
        LayerUI<?> ui = layer.getUI();
        if (ui instanceof TransformUI transformUI) {
            AffineTransform transform = transformUI.getPreferredTransform(size, layer);
            if (transform != null) {
                Area area = new Area(new Rectangle2D.Double(0, 0, size.getWidth(), size.getHeight()));
                area.transform(transform);
                Rectangle2D bounds = area.getBounds2D();
                size.setSize(bounds.getWidth(), bounds.getHeight());
            }
        }
        return size;
    }
}
