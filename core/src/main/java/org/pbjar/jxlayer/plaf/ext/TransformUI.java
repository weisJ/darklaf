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
package org.pbjar.jxlayer.plaf.ext;

import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.LayoutManager;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.swing.JComponent;
import javax.swing.JLayer;
import javax.swing.RepaintManager;
import javax.swing.SwingUtilities;
import javax.swing.border.Border;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.LayerUI;
import javax.swing.text.GlyphView.GlyphPainter;
import javax.swing.text.JTextComponent;

import org.pbjar.jxlayer.plaf.ext.transform.DefaultTransformModel;
import org.pbjar.jxlayer.plaf.ext.transform.TransformLayout;
import org.pbjar.jxlayer.plaf.ext.transform.TransformModel;
import org.pbjar.jxlayer.plaf.ext.transform.TransformRPMAnnotation;
import org.pbjar.jxlayer.plaf.ext.transform.TransformRPMFallBack;
import org.pbjar.jxlayer.plaf.ext.transform.TransformUtils;
import org.pbjar.jxlayer.repaint.RepaintManagerProvider;
import org.pbjar.jxlayer.repaint.RepaintManagerUtils;
import org.pbjar.jxlayer.repaint.WrappedRepaintManager;

import com.github.weisj.darklaf.util.LogUtil;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.SystemInfo;

/**
 * This class provides for all necessary functionality when using transformations in a
 * {@link LayerUI}.
 * <p>
 * Some implementation details:
 * <ul>
 * <li>It extends {@link MouseEventUI} because, when applying transformations, the whereabouts of
 * child components on screen (device space) do not necessarily match the location according to
 * their bounds as set by layout managers (component space). So, mouse events must always be
 * redirected to the intended recipients.
 * <li>When enabled, this implementation sets a different {@link LayoutManager} to be used by
 * {@link JLayer}. Instead of setting the size of the view to {@link JLayer}'s inner area, it sets
 * the size of the view to the view's <em>preferred</em> size and centers it in the inner area.
 * Also, when calculating the preferred size of {@link JLayer}, it transforms the normally
 * calculated size with the {@link AffineTransform} returned from
 * {@link #getPreferredTransform(Dimension, JLayer)}.
 * <li>This implementation allocates a fresh {@link BufferedImage} the size of the clip area, each
 * time that the {@link #paint(Graphics, JComponent)} method is invoked.
 * <li>Rather than configuring the screen graphics object, the image's graphics object is configured
 * through {@link #configureGraphics(Graphics2D, JLayer)}.
 * <li>Regardless of whether or not the view is opaque, a background color is painted. It is
 * obtained from the first component upwards in the hierarchy starting with the view, that is
 * opaque. If an opaque component is not found, the background color of the layer is used. Painting
 * the background is necessary to prevent visual artifacts when the transformation is changed
 * dynamically.
 * <li>Rendering hints may be set with {@link #setRenderingHints(Map)},
 * {@link #addRenderingHint(RenderingHints.Key, Object)} and {@link #addRenderingHints(Map)}.
 * </ul>
 * <p>
 * Known limitations:
 * <ol>
 * <li>In Java versions <b>before Java 6u10</b>, this implementation employs a custom
 * {@link RepaintManager} in order to have descendant's repaint requests propagated up to the
 * {@link JLayer} ancestor. This {@link RepaintManager} will work well with and without other
 * {@link RepaintManager} that are subclasses of the {@link WrappedRepaintManager}. Other
 * {@link RepaintManager}s may cause conflicts.
 * <p>
 * In Java versions <b>6u10 or higher</b>, an attempt will be made to use the new RepaintManager
 * delegate facility that has been designed for JavaFX.
 * <li>Transformations will be applied on the whole of the content of the {@link JLayer}. The result
 * is that {@link Border}s and other content within {@link JLayer}'s insets will generally either be
 * invisible, or will be rendered in a very undesirable way. If you want a {@link Border} to be
 * transformed together with {@link JLayer}'s view, that border should be set on the view instead.
 * On the other hand, if you want the {@link Border} not to be transformed, that border must be set
 * on {@link JLayer}'s parent.
 * </ol>
 * <b>Note:</b> A {@link TransformUI} instance cannot be shared and can be set to a single
 * {@link JLayer} instance only.
 *
 * @author Piet Blok
 */
public class TransformUI extends MouseEventUI<JComponent> {

    public static final String BUFFERED_REPAINT_FLAG = "darklaf.useBufferedRepaintManager";
    private static final String EXPORTS_FLAG = "--add-exports java.desktop/com.sun.java.swing=ALL-UNNAMED";
    private static final LayoutManager transformLayout = new TransformLayout();
    private static final String KEY_VIEW = "view";
    private static final boolean delegatePossible;
    private static final RepaintManager wrappedManager = new TransformRepaintManager();
    private static final Logger LOGGER = LogUtil.getDetachedLogger(TransformUI.class);

    private static MethodHandle setDelegateRepaintManagerMethod;

    static {
        boolean value;
        boolean bufferFlag = PropertyUtil.getSystemFlag(BUFFERED_REPAINT_FLAG, false);
        try {
            // Use leaner java version restriction than in other places due to the effect it has.
            // When using a version < 16 then illegal access has to be declared explicitly.
            if (!SystemInfo.isJava16OrGreater || bufferFlag) {
                Class<?> swingUtilities3 = Class.forName("com.sun.java.swing.SwingUtilities3");
                setDelegateRepaintManagerMethod = MethodHandles.lookup().findStatic(
                        swingUtilities3, "setDelegateRepaintManager",
                        MethodType.methodType(void.class, JComponent.class, RepaintManager.class));
                value = true;
            } else {
                value = false;
            }
        } catch (Throwable t) {
            if (bufferFlag) {
                LOGGER.log(Level.SEVERE, "For " + BUFFERED_REPAINT_FLAG + " to work you need to start with " + EXPORTS_FLAG, t);
            }
            value = false;
        }
        delegatePossible = value;
        LOGGER.info("Java " + System.getProperty("java.version") + " " + System.getProperty("java.vm.version")
                + (delegatePossible
                        ? ": RepaintManager delegate facility for JavaFX will be used."
                        : ": RepaintManager.setCurrentManager() will be used."));
    }

    private final ChangeListener changeListener = e -> revalidateLayer();
    private final RepaintManagerProvider rpmProvider = new RepaintManagerProvider() {

        @Override
        public WrappedRepaintManager createWrappedRepaintManager(final RepaintManager delegate) {
            return new TransformRPMFallBack(delegate);
        }

        @Override
        public boolean isAdequate(final RepaintManager manager) {
            return manager.getClass().isAnnotationPresent(TransformRPMAnnotation.class);
        }
    };
    private final Map<RenderingHints.Key, Object> renderingHints = new HashMap<>();
    private final Set<JComponent> originalDoubleBuffered = new HashSet<>();
    private JComponent view;
    private final PropertyChangeListener viewChangeListener = evt -> setView((JComponent) evt.getNewValue());

    private TransformModel transformModel;
    private LayoutManager originalLayout;

    /**
     * Construct a {@link TransformUI} with a {@link DefaultTransformModel}.
     */
    public TransformUI() {
        this(new DefaultTransformModel());
    }

    /**
     * Construct a {@link TransformUI} with a specified model.
     *
     * @param model the model
     */
    public TransformUI(final TransformModel model) {
        super();
        this.setModel(model);
    }

    private void revalidateLayer() {
        JLayer<? extends JComponent> installedLayer = this.getInstalledLayer();
        if (installedLayer != null) {
            installedLayer.revalidate();
            installedLayer.repaint();
        }
    }

    /**
     * {@link JTextComponent} and its descendants have some caret position problems when used inside a
     * transformed {@link JLayer}. When you plan to use {@link JTextComponent}(s) inside the hierarchy
     * of a transformed {@link JLayer}, call this method in an early stage, before instantiating any
     * {@link JTextComponent}.
     * <p>
     * It executes the following method:
     *
     * <pre>
     * System.setProperty(&quot;i18n&quot;, Boolean.TRUE.toString());
     * </pre>
     * <p>
     * As a result, a {@link GlyphPainter} will be selected that uses floating point instead of fixed
     * point calculations.
     */
    public static void prepareForJTextComponent() {
        System.setProperty("i18n", Boolean.TRUE.toString());
    }

    /**
     * Add one rendering hint to the currently active rendering hints.
     *
     * @param key the key
     * @param value the value
     */
    public void addRenderingHint(final RenderingHints.Key key, final Object value) {
        this.renderingHints.put(key, value);
    }

    /**
     * Add new rendering hints to the currently active rendering hints.
     *
     * @param hints the new rendering hints
     */
    public void addRenderingHints(final Map<RenderingHints.Key, Object> hints) {
        this.renderingHints.putAll(hints);
    }

    /**
     * Get the {@link TransformModel}.
     *
     * @return the {@link TransformModel}
     * @see #setModel(TransformModel)
     */
    public final TransformModel getModel() {
        return transformModel;
    }

    /**
     * Set a new {@link TransformModel}. The new model may not be {@code null}.
     *
     * @param transformModel the new model
     * @throws NullPointerException if transformModel is {@code null}
     * @see #getModel()
     */
    public final void setModel(final TransformModel transformModel) throws NullPointerException {
        if (transformModel == null) {
            throw new NullPointerException("The TransformModel may not be null");
        }
        if (this.transformModel != null) {
            this.transformModel.removeChangeListener(this.changeListener);
        }
        this.transformModel = transformModel;
        this.transformModel.addChangeListener(this.changeListener);
        revalidateLayer();
    }

    /**
     * Get a preferred {@link AffineTransform}. This method will typically be invoked by programs that
     * calculate a preferred size.
     * <p>
     * The {@code size} argument will be used to compute anchor values for some types of
     * transformations. If the {@code size} argument is {@code null} a value of (0,0) is used for the
     * anchor.
     * <p>
     * In {@code enabled} state this method is delegated to the {@link TransformModel} that has been
     * set. Otherwise {@code null} will be returned.
     *
     * @param size a {@link Dimension} instance to be used for an anchor or {@code null}
     * @param layer the {@link JLayer}.
     * @return a {@link AffineTransform} instance or {@code null}
     */
    public AffineTransform getPreferredTransform(final Dimension size, final JLayer<? extends JComponent> layer) {
        return this.transformModel != null ? this.transformModel.getPreferredTransform(size, layer)
                : new AffineTransform();
    }

    /**
     * Overridden to replace the {@link LayoutManager}, to add some listeners and to ensure that an
     * appropriate {@link RepaintManager} is installed.
     *
     * @see #uninstallUI(JComponent)
     */
    @Override
    public void installUI(final JComponent component) {
        super.installUI(component);
        JLayer<? extends JComponent> installedLayer = this.getInstalledLayer();
        originalLayout = installedLayer.getLayout();
        installedLayer.addPropertyChangeListener(KEY_VIEW, this.viewChangeListener);
        setLayoutManager(transformLayout);
        setView(installedLayer.getView());
        if (!delegatePossible) {
            String warningMessage =
                "Using a fallback method for the TransformUI repaint manager. This will result in flickering during resizing\n"
                + "Consider adding\n"
                + "       -D" + BUFFERED_REPAINT_FLAG + "=true\n"
                + "       " + EXPORTS_FLAG + "\n"
                + "to your startup parameters or providing a fix for this issue";
            LOGGER.warning(warningMessage);
            RepaintManagerUtils.ensureRepaintManagerSet(installedLayer, rpmProvider);
        }
    }

    /**
     * Overridden to restore the original {@link LayoutManager} and remove some listeners.
     *
     * @param c the component.
     */
    @Override
    public void uninstallUI(final JComponent c) {
        JLayer<? extends JComponent> installedLayer = this.getInstalledLayer();
        Objects.requireNonNull(installedLayer)
                .removePropertyChangeListener(KEY_VIEW, this.viewChangeListener);
        installedLayer.setLayout(originalLayout);
        setView(null);
        super.uninstallUI(c);
    }

    private void setView(final JComponent view) {
        if (delegatePossible) {
            if (this.view != null) {
                try {
                    setDelegateRepaintManagerMethod.invokeExact(this.view, null);
                } catch (Throwable ignored) {
                }
            }
        }
        this.view = view;
        if (delegatePossible) {
            if (this.view != null) {
                try {
                    setDelegateRepaintManagerMethod.invokeExact(this.view, wrappedManager);
                } catch (Throwable ignored) {
                }
            }
        }
        setDirty(true);
    }

    /**
     * Replace the currently active rendering hints with new hints.
     *
     * @param hints the new rendering hints or {@code null} to clear all rendering hints
     */
    public void setRenderingHints(final Map<RenderingHints.Key, Object> hints) {
        this.renderingHints.clear();
        if (hints != null) {
            this.renderingHints.putAll(hints);
        }
    }

    /**
     * Primarily intended for use by {@link RepaintManager}.
     *
     * @param rect a rectangle
     * @param layer the layer
     * @return the argument rectangle if no {@link AffineTransform} is available, else a new rectangle
     */
    public final Rectangle transform(final Rectangle rect, final JLayer<? extends JComponent> layer) {
        AffineTransform at = getTransform(layer);
        if (at == null) {
            return rect;
        } else {
            Area area = new Area(rect);
            area.transform(at);
            return area.getBounds();
        }
    }

    /**
     * Mark {@link TransformUI} as dirty if the LookAndFeel was changed.
     *
     * @param layer the {@link JLayer} this {@link TransformUI} is set to
     */
    @Override
    public void updateUI(final JLayer<? extends JComponent> layer) {
        setDirty(true);
    }

    /**
     * Set a complete hierarchy to non double buffered and remember the components that were double
     * buffered.
     *
     * @param component the component.
     */
    private void setToNoDoubleBuffering(final Component component) {
        if (component instanceof JComponent) {
            JComponent comp = (JComponent) component;
            if (comp.isDoubleBuffered()) {
                originalDoubleBuffered.add(comp);
                comp.setDoubleBuffered(false);
            }
        }
        if (component instanceof Container) {
            Container container = (Container) component;
            for (int index = 0; index < container.getComponentCount(); index++) {
                setToNoDoubleBuffering(container.getComponent(index));
            }
        }
    }

    /**
     * If the view of the {@link JLayer} is (partly) obscured by its parent (this is the case when the
     * size of the view (in component space) is larger than the size of the {@link JLayer}), the
     * obscured parts will not be painted by the super implementation. Therefore, only under this
     * condition, a special painting technique is executed:
     * <ol>
     * <li>All descendants of the {@link JLayer} are temporarily set to non double buffered.
     * <li>The graphics object is translated for the X and Y coordinates of the view.
     * <li>The view is painted.
     * <li>The original double buffered property is restored for all descendants.
     * </ol>
     * <p>
     * In all other cases, the super method is invoked.
     * <p>
     * The {@code g2} argument is a graphics object obtained from a {@link BufferedImage}.
     *
     * @see #paint(Graphics, JComponent)
     */
    @Override
    protected final void paintLayer(final Graphics2D g2, final JLayer<? extends JComponent> layer) {
        JComponent view = layer.getView();
        if (view != null) {
            if (view.getX() < 0 || view.getY() < 0) {
                setToNoDoubleBuffering(view);
                g2.translate(view.getX(), view.getY());
                view.paint(g2);
                for (JComponent comp : originalDoubleBuffered) {
                    comp.setDoubleBuffered(true);
                }
                originalDoubleBuffered.clear();
                return;
            }
        }
        super.paintLayer(g2, layer);
    }

    /**
     * Get the {@link AffineTransform} customized for the {@code layer} argument.
     * <p>
     * In {@code enabled} state this method is delegated to the {@link TransformModel} that has been
     * set. Otherwise {@code null} will be returned.
     */
    @Override
    protected final AffineTransform getTransform(final JLayer<? extends JComponent> layer) {
        return transformModel != null ? transformModel.getTransform(layer)
                : new AffineTransform();
    }

    /**
     * Get the rendering hints.
     *
     * @return the rendering hints
     * @see #setRenderingHints(Map)
     * @see #addRenderingHints(Map)
     * @see #addRenderingHint(RenderingHints.Key, Object)
     */
    @Override
    protected Map<RenderingHints.Key, Object> getRenderingHints(final JLayer<? extends JComponent> layer) {
        return renderingHints;
    }

    /**
     * A delegate {@link RepaintManager} that can be set on the view of a {@link JLayer} in Java
     * versions starting with Java 6u10.
     * <p>
     * For older Java versions, {@link RepaintManager#setCurrentManager(RepaintManager)} will be used
     * with {@link TransformRPMFallBack}.
     */
    protected static final class TransformRepaintManager extends RepaintManager {

        private TransformRepaintManager() {}

        /**
         * Finds the JLayer ancestor and have ancestor marked invalid via the current
         * {@link RepaintManager}.
         */
        @Override
        public void addInvalidComponent(final JComponent invalidComponent) {
            JLayer<? extends JComponent> layer = TransformUtils.findTransformJLayer(invalidComponent);
            RepaintManager.currentManager(layer).addInvalidComponent(layer);
        }

        /**
         * Finds the JLayer ancestor and have the ancestor marked as dirty with the transformed rectangle
         * via the current {@link RepaintManager}.
         */
        @Override
        public void addDirtyRegion(final JComponent c, final int x, final int y, final int w, final int h) {
            if (c.isShowing()) {
                JLayer<? extends JComponent> layer = Objects.requireNonNull(TransformUtils.findTransformJLayer(c));
                TransformUI ui = (TransformUI) layer.getUI();
                Point point = c.getLocationOnScreen();
                SwingUtilities.convertPointFromScreen(point, layer);
                Rectangle transformPortRegion = ui.transform(new Rectangle(x + point.x, y + point.y, w, h),
                        layer);
                RepaintManager.currentManager(layer).addDirtyRegion(layer,
                        transformPortRegion.x,
                        transformPortRegion.y,
                        transformPortRegion.width,
                        transformPortRegion.height);
            }
        }
    }
}
