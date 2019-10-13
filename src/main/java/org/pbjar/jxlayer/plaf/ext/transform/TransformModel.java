/*
 Copyright (c) 2009, Piet Blok
 All rights reserved.
 <p>
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 <p>
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.
 * Neither the name of the copyright holder nor the names of the
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.
 <p>
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.pbjar.jxlayer.plaf.ext.transform;

import org.jdesktop.jxlayer.JXLayer;
import org.jetbrains.annotations.NotNull;
import org.pbjar.jxlayer.plaf.ext.TransformUI;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import java.awt.*;
import java.awt.geom.AffineTransform;

/**
 * The {@link TransformModel} interface specifies the methods the {@link TransformUI} will use to
 * interrogate a transformation model.
 *
 * @author Piet Blok
 */
public interface TransformModel {

    /**
     * Add a {@link ChangeListener} that will be notified when the internal state of this model
     * changes.
     *
     * @param listener a {@link ChangeListener}
     * @see #removeChangeListener(ChangeListener)
     */
    void addChangeListener(ChangeListener listener);

    /**
     * Get a preferred {@link AffineTransform}. This method will typically be invoked by programs that
     * calculate a preferred size.
     *
     * <p>The {@code size} argument will be used to compute anchor values for some types of
     * transformations. If the {@code size} argument is {@code null} a value of (0,0) is used for the
     * anchor.
     *
     * @param size  a {@link Dimension} instance to be used for an anchor or {@code null}
     * @param layer the {@link JXLayer}.
     * @return a {@link AffineTransform} instance or {@code null}
     */
    @NotNull
    AffineTransform getPreferredTransform(Dimension size, JXLayer<?> layer);

    /**
     * Get a {@link AffineTransform}. This method will typically be invoked by programs that are about
     * to prepare a {@link Graphics} object.
     *
     * @param layer the {@link JXLayer}
     * @return a {@link AffineTransform} or {@code null}
     */
    @NotNull
    AffineTransform getTransform(JXLayer<? extends JComponent> layer);

    /**
     * Remove a {@link ChangeListener}.
     *
     * @param listener a {@link ChangeListener}
     * @see #addChangeListener(ChangeListener)
     */
    void removeChangeListener(ChangeListener listener);
}
