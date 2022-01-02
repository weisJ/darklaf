/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
