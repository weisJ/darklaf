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
import java.awt.geom.AffineTransform;

import javax.swing.*;
import javax.swing.event.ChangeListener;

import org.pbjar.jxlayer.plaf.ext.TransformUI;

/**
 * The {@link TransformModel} interface specifies the methods the {@link TransformUI} will use to
 * interrogate a transformation model.
 *
 * @author Piet Blok
 */
public interface TransformModel {

    /**
     * Add a {@link ChangeListener} that will be notified when the internal state of this model changes.
     *
     * @param listener a {@link ChangeListener}
     * @see #removeChangeListener(ChangeListener)
     */
    void addChangeListener(ChangeListener listener);

    /**
     * Get a preferred {@link AffineTransform}. This method will typically be invoked by programs that
     * calculate a preferred size.
     * <p>
     * The {@code size} argument will be used to compute anchor values for some types of
     * transformations. If the {@code size} argument is {@code null} a value of (0,0) is used for the
     * anchor.
     *
     * @param size a {@link Dimension} instance to be used for an anchor or {@code null}
     * @param layer the {@link JLayer}.
     * @return a {@link AffineTransform} instance or {@code null}
     */
    AffineTransform getPreferredTransform(Dimension size, JLayer<?> layer);

    /**
     * Get a {@link AffineTransform}. This method will typically be invoked by programs that are about
     * to prepare a {@link Graphics} object.
     *
     * @param layer the {@link JLayer}
     * @return a {@link AffineTransform} or {@code null}
     */
    AffineTransform getTransform(JLayer<? extends JComponent> layer);

    /**
     * Remove a {@link ChangeListener}.
     *
     * @param listener a {@link ChangeListener}
     * @see #addChangeListener(ChangeListener)
     */
    void removeChangeListener(ChangeListener listener);
}
