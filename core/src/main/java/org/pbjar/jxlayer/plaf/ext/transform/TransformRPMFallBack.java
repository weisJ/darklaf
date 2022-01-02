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

import java.awt.Point;
import java.awt.Rectangle;

import javax.swing.*;
import javax.swing.plaf.LayerUI;

import org.jdesktop.swingx.ForwardingRepaintManager;
import org.pbjar.jxlayer.plaf.ext.TransformUI;
import org.pbjar.jxlayer.repaint.RepaintManagerProvider;
import org.pbjar.jxlayer.repaint.RepaintManagerUtils;
import org.pbjar.jxlayer.repaint.WrappedRepaintManager;

/**
 * A specialized {@link RepaintManager} that checks for every JComponent that is being set dirty, if
 * it has a JLayer ancestor, equipped with a TransformUI. In that case, the transformed region on
 * the JLayer is also marked dirty.
 * <p>
 * A fall back class if the {@link ForwardingRepaintManager} cannot be instantiated because the
 * SwingX packages are not on the class path.
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
