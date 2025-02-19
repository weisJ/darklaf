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

import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.*;
import javax.swing.plaf.LayerUI;

import org.pbjar.jxlayer.plaf.ext.TransformUI;
import org.pbjar.jxlayer.repaint.RepaintManagerProvider;
import org.pbjar.jxlayer.repaint.RepaintManagerUtils;
import org.pbjar.jxlayer.repaint.WrappedRepaintManager;

/**
 * A specialized {@link RepaintManager} that checks for every JComponent that is being set dirty, if
 * it has a JLayer ancestor, equipped with a TransformUI. In that case, the transformed region on
 * the JLayer is also marked dirty.
 * <p>
 *
 * @see RepaintManagerProvider
 * @see RepaintManagerUtils
 */
@TransformRPMAnnotation
public class TransformRPMFallBack extends WrappedRepaintManager {

    public TransformRPMFallBack(final RepaintManager delegate) {
        super(delegate);
    }

    /**
     * Searches upwards in the component hierarchy for a {@link JLayer} ancestor with an enabled
     * {@link TransformUI}.
     * <p>
     * If found, the dirty rectangle is transformed to a rectangle targeted at that {@link JLayer} and
     * the argument manager's {@link RepaintManager#addDirtyRegion(JComponent, int, int, int, int)} is
     * invoked.
     *
     * @param aComponent a component
     * @param x the X of the dirty region
     * @param y the Y of the dirty region
     * @param w the width of the dirty region
     * @param h the height of the dirty region
     */
    @SuppressWarnings("unchecked")
    @Override
    public void addDirtyRegion(final JComponent aComponent, final int x, final int y, final int w, final int h) {
        if (aComponent.isShowing()) {
            JLayer<?> layer = TransformUtils.findTransformJLayer(aComponent);
            if (layer != null) {
                LayerUI<?> layerUI = layer.getUI();
                TransformUI ui = (TransformUI) layerUI;
                Point point = aComponent.getLocationOnScreen();
                SwingUtilities.convertPointFromScreen(point, layer);
                Rectangle transformPortRegion = ui.transform(new Rectangle(x + point.x, y + point.y, w, h),
                        (JLayer<JComponent>) layer);
                addDirtyRegion(layer,
                        transformPortRegion.x, transformPortRegion.y,
                        transformPortRegion.width, transformPortRegion.height);
                return;
            }
        }
        super.addDirtyRegion(aComponent, x, y, w, h);
    }
}
