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
import java.util.Map;

import javax.swing.*;
import javax.swing.plaf.LayerUI;

import org.pbjar.jxlayer.plaf.ext.TransformUI;

/**
 * Some convenience methods to create a populated transforming {@link JLayer}.
 *
 * @author Piet Blok
 */
public final class TransformUtils {

    private TransformUtils() {}

    /**
     * Create a Transform JLayer.
     *
     * @param component the component.
     * @return the JLayer.
     */
    public static JLayer<JComponent> createTransformJLayer(final JComponent component) {
        return createTransformJLayer(component, 1.0, null);
    }

    /**
     * Create a Transform JLayer.
     *
     * @param component the component.
     * @param scale the scaling
     * @param hints the rendering hints.
     * @return the JLayer.
     */
    public static JLayer<JComponent> createTransformJLayer(final JComponent component, final double scale,
            final Map<RenderingHints.Key, Object> hints) {
        DefaultTransformModel model = new DefaultTransformModel();
        model.setScale(scale);
        return createTransformJLayer(component, model, hints);
    }

    /**
     * Create a Transform JLayer.
     *
     * @param component the component.
     * @param model the transform model.
     * @param hints the rendering hints.
     * @return the JLayer.
     */
    public static JLayer<JComponent> createTransformJLayer(final JComponent component, final TransformModel model,
            final Map<RenderingHints.Key, Object> hints) {
        TransformUI ui = new TransformUI(model);
        ui.setRenderingHints(hints);
        return new JLayer<>(component, ui);
    }

    /**
     * Create a Transform JLayer.
     *
     * @param component the component.
     * @param scale the scaling
     * @return the JLayer.
     */
    public static JLayer<JComponent> createTransformJLayer(final JComponent component, final double scale) {
        return createTransformJLayer(component, scale, null);
    }

    /**
     * Create a Transform JLayer.
     *
     * @param component the component.
     * @param model the transform model.
     * @return the JLayer.
     */
    public static JLayer<JComponent> createTransformJLayer(final JComponent component, final TransformModel model) {
        return createTransformJLayer(component, model, null);
    }

    /**
     * Find the first ancestor {@link JLayer} with an enabled {@link TransformUI}.
     *
     * @param aComponent some component
     * @return a {@link JLayer} instance or {@code null}
     */
    @SuppressWarnings("unchecked")
    public static JLayer<? extends JComponent> findTransformJLayer(final JComponent aComponent) {
        JLayer<?> layer = (JLayer<?>) SwingUtilities.getAncestorOfClass(JLayer.class, aComponent);
        if (layer != null) {
            LayerUI<?> ui = ((JLayer<?>) layer).getUI();
            if (ui instanceof TransformUI) {
                return (JLayer<? extends JComponent>) layer;
            }
            return findTransformJLayer(layer);
        }
        return null;
    }
}
