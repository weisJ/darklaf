/*
 * Copyright (c) 2009, Piet Blok
 * All rights reserved.
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * <p>
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 * Neither the name of the copyright holder nor the names of the
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
