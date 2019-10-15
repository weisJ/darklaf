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

import com.weis.darklaf.log.LogFormatter;
import org.jdesktop.jxlayer.JXLayer;
import org.jdesktop.jxlayer.plaf.LayerUI;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.pbjar.jxlayer.plaf.ext.TransformUI;

import javax.swing.*;
import java.awt.*;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

/**
 * To avoid duplicate code, this class implements the actual logic for {@link TransformRPMSwingX}
 * and {@link TransformRPMFallBack}.
 *
 * @author Piet Blok
 */
public final class TransformRPMImpl {

    private static final Logger LOGGER = Logger.getLogger(TransformRPMImpl.class.getName());
    /**
     * A flag, indicating whether or not a very dirty initialization on created {@link
     * RepaintManager}s must be performed.
     *
     * @see #hackInitialization(RepaintManager, RepaintManager)
     */
    public static boolean hack = false;

    static {
        LOGGER.setUseParentHandlers(false);
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new LogFormatter());
        LOGGER.addHandler(handler);
    }

    private TransformRPMImpl() {
    }

    /**
     * Searches upwards in the component hierarchy for a {@link JXLayer} ancestor with an enabled
     * {@link TransformUI}.
     * <p>
     * If found, the dirty rectangle is transformed to a rectangle targeted at that {@link JXLayer}
     * and the argument manager's {@link RepaintManager#addDirtyRegion(JComponent, int, int, int,
     * int)} is invoked. {@code true} is returned.
     * </p>
     * <p>
     * Else, (@code false} is returned.
     * </p>
     *
     * @param aComponent a component
     * @param x          the X of the dirty region
     * @param y          the Y of the dirty region
     * @param w          the width of the dirty region
     * @param h          the height of the dirty region
     * @param manager    the current {@link RepaintManager}
     * @return {@code true} if the call is delegated to the manager with a transformed rectangle,
     * {@code false} otherwise
     */
    @SuppressWarnings("unchecked")
    public static boolean addDirtyRegion(@NotNull final JComponent aComponent, final int x, final int y,
                                         final int w, final int h, @NotNull final RepaintManager manager) {
        if (aComponent.isShowing()) {
            JXLayer<?> layer = findJXLayer(aComponent);
            if (layer != null) {
                LayerUI<?> layerUI = layer.getUI();
                TransformUI ui = (TransformUI) layerUI;
                Point point = aComponent.getLocationOnScreen();
                SwingUtilities.convertPointFromScreen(point, layer);
                Rectangle transformPortRegion = ui.transform(new Rectangle(x + point.x, y + point.y, w, h),
                                                             (JXLayer<JComponent>) layer);
                manager.addDirtyRegion(layer,
                                       transformPortRegion.x, transformPortRegion.y,
                                       transformPortRegion.width, transformPortRegion.height);
                return true;
            }
        }
        return false;
    }

    /**
     * If {@link #hack} is {@code true}, the private fields {@code paintManager} and {@code
     * bufferStrategyType} are copied via reflection from the source manager into the destination
     * manager.
     *
     * @param sourceManager      the source manager
     * @param destinationManager the destination manager
     */
    public static void hackInitialization(final RepaintManager sourceManager,
                                          final RepaintManager destinationManager) {
        if (hack) {
            Class<RepaintManager> rpmClass = RepaintManager.class;
            try {

                Field fieldBufferStrategyType = rpmClass.getDeclaredField("bufferStrategyType");
                Field fieldPaintManager = rpmClass.getDeclaredField("paintManager");
                Method methodGetPaintManager = rpmClass.getDeclaredMethod("getPaintManager");

                fieldBufferStrategyType.setAccessible(true);
                fieldPaintManager.setAccessible(true);
                methodGetPaintManager.setAccessible(true);

                Object paintManager = methodGetPaintManager.invoke(sourceManager);
                short bufferStrategyType = (Short) fieldBufferStrategyType.get(sourceManager);

                fieldBufferStrategyType.set(destinationManager, bufferStrategyType);
                fieldPaintManager.set(destinationManager, paintManager);

                fieldBufferStrategyType.setAccessible(false);
                fieldPaintManager.setAccessible(false);
                methodGetPaintManager.setAccessible(false);

                LOGGER.warning("Copied paintManager of type: " + paintManager.getClass().getName());
                switch (bufferStrategyType) {
                    case (0):
                        LOGGER.warning("Copied bufferStrategyType "
                                               + bufferStrategyType
                                               + ": BUFFER_STRATEGY_NOT_SPECIFIED");
                        break;
                    case (1):
                        LOGGER.warning("Copied bufferStrategyType "
                                               + bufferStrategyType
                                               + ": BUFFER_STRATEGY_SPECIFIED_ON");
                        break;
                    case (2):
                        LOGGER.warning("Copied bufferStrategyType "
                                               + bufferStrategyType
                                               + ": BUFFER_STRATEGY_SPECIFIED_OFF");
                        break;
                    default:
                        LOGGER.warning("Copied bufferStrategyType "
                                               + bufferStrategyType + ": ???");
                        break;
                }
            } catch (Throwable t) {
                t.printStackTrace(System.out);
            }
        }
    }

    /**
     * Find the first ancestor {@link JXLayer} with an enabled {@link TransformUI}.
     *
     * @param aComponent some component
     * @return a {@link JXLayer} instance or {@code null}
     */
    @Nullable
    private static JXLayer<?> findJXLayer(final JComponent aComponent) {

        JXLayer<?> layer = (JXLayer<?>) SwingUtilities.getAncestorOfClass(
                JXLayer.class, aComponent);
        if (layer != null) {
            LayerUI<?> ui = ((JXLayer<?>) layer).getUI();
            if (ui instanceof TransformUI) {
                return layer;
            }
            return findJXLayer(layer);
        }
        return null;
    }
}
