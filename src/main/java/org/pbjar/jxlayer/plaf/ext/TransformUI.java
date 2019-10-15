package org.pbjar.jxlayer.plaf.ext;
/*
 * Copyright (c) 2009, Piet Blok
 * All rights reserved.
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * <p>
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 * * Neither the name of the copyright holder nor the names of the
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

import com.sun.java.swing.SwingUtilities3;
import com.weis.darklaf.log.LogFormatter;
import org.jdesktop.jxlayer.JXLayer;
import org.jdesktop.jxlayer.plaf.AbstractBufferedLayerUI;
import org.jdesktop.jxlayer.plaf.LayerUI;
import org.jdesktop.swingx.ForwardingRepaintManager;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.pbjar.jxlayer.plaf.ext.transform.DefaultTransformModel;
import org.pbjar.jxlayer.plaf.ext.transform.TransformLayout;
import org.pbjar.jxlayer.plaf.ext.transform.TransformModel;
import org.pbjar.jxlayer.plaf.ext.transform.TransformRPMAnnotation;
import org.pbjar.jxlayer.plaf.ext.transform.TransformRPMFallBack;
import org.pbjar.jxlayer.plaf.ext.transform.TransformRPMSwingX;
import org.pbjar.jxlayer.repaint.RepaintManagerProvider;
import org.pbjar.jxlayer.repaint.RepaintManagerUtils;
import org.pbjar.jxlayer.repaint.WrappedRepaintManager;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.ChangeListener;
import javax.swing.text.GlyphView.GlyphPainter;
import javax.swing.text.JTextComponent;
import java.awt.*;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.image.BufferedImage;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

/**
 * This class provides for all necessary functionality when using transformations in a {@link
 * LayerUI}.
 *
 * <p>Some implementation details:
 *
 * <p>
 *
 * <ul>
 * <li>It extends {@link MouseEventUI} because, when applying transformations, the whereabouts of
 * child components on screen (device space) do not necessarily match the location according
 * to their bounds as set by layout managers (component space). So, mouse events must always
 * be redirected to the intended recipients.
 * <li>When enabled, this implementation sets a different {@link LayoutManager} to be used by
 * {@link JXLayer}. Instead of setting the size of the view to {@link JXLayer}'s inner area,
 * it sets the size of the view to the view's <em>preferred</em> size and centers it in the
 * inner area. Also, when calculating the preferred size of {@link JXLayer}, it transforms the
 * normally calculated size with the {@link AffineTransform} returned from {@link
 * #getPreferredTransform(Dimension, JXLayer)}.
 * <li>This implementation allocates a fresh {@link BufferedImage} the size of the clip area, each
 * time that the {@link #paint(Graphics, JComponent)} method is invoked. This is different
 * from the implementation of {@link AbstractBufferedLayerUI}, that maintains a cached image,
 * the size of the view. An important reason to not follow the {@link AbstractBufferedLayerUI}
 * strategy is that, when applying scaling transformations with a large scaling factor, a
 * {@link OutOfMemoryError} may be thrown because it will try to allocate a buffer of an
 * extreme size, even if not all of its contents will actually be visible on the screen.
 * <li>Rather than configuring the screen graphics object, the image's graphics object is
 * configured through {@link #configureGraphics(Graphics2D, JXLayer)}.
 * <li>Regardless of whether or not the view is opaque, a background color is painted. It is
 * obtained from the first component upwards in the hierarchy starting with the view, that is
 * opaque. If an opaque component is not found, the background color of the layer is used.
 * Painting the background is necessary to prevent visual artifacts when the transformation is
 * changed dynamically.
 * <li>Rendering hints may be set with {@link #setRenderingHints(Map)}, {@link
 * #addRenderingHint(RenderingHints.Key, Object)} and {@link #addRenderingHints(Map)}.
 * </ul>
 *
 * <p>Known limitations:
 *
 * <p>
 *
 * <ol>
 * <li>In Java versions <b>before Java 6u10</b>, this implementation employs a custom {@link
 * RepaintManager} in order to have descendant's repaint requests propagated up to the {@link
 * JXLayer} ancestor. This {@link RepaintManager} will work well with and without other {@link
 * RepaintManager} that are either subclasses of the {@link WrappedRepaintManager} or SwingX's
 * {@link ForwardingRepaintManager}. Other {@link RepaintManager}s may cause conflicts.
 * <p>In Java versions <b>6u10 or higher</b>, an attempt will be made to use the new
 * RepaintManager delegate facility that has been designed for JavaFX.
 * <li>Transformations will be applied on the whole of the content of the {@link JXLayer}. The
 * result is that {@link Border}s and other content within {@link JXLayer}'s insets will
 * generally either be invisible, or will be rendered in a very undesirable way. If you want a
 * {@link Border} to be transformed together with {@link JXLayer}'s view, that border should
 * be set on the view instead. On the other hand, if you want the {@link Border} not to be
 * transformed, that border must be set on {@link JXLayer}'s parent.
 * </ol>
 *
 * <b>Note:</b> A {@link TransformUI} instance cannot be shared and can be set to a single {@link
 * JXLayer} instance only.
 *
 * @author Piet Blok
 */
public class TransformUI extends MouseEventUI<JComponent> {


    private static final LayoutManager transformLayout = new TransformLayout();
    private static final String KEY_VIEW = "view";
    private static final boolean delegatePossible;
    private static final RepaintManager wrappedManager = new TransformRepaintManager();
    private static final Logger LOGGER = Logger.getLogger(TransformUI.class.getName());

    static {
        LOGGER.setUseParentHandlers(false);
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new LogFormatter());
        LOGGER.addHandler(handler);

        boolean value;
        try {
            SwingUtilities3.class.getMethod("setDelegateRepaintManager", JComponent.class, RepaintManager.class);
            value = true;
        } catch (Throwable t) {
            value = false;
        }
        delegatePossible = value;
        LOGGER.info("Java " + System.getProperty("java.version") + " " + System.getProperty("java.vm.version")
                            + (delegatePossible ? ": RepaintManager delegate facility for JavaFX will be used."
                                                : ": RepaintManager.setCurrentManager() will be used."));
    }

    private final ChangeListener changeListener = e -> revalidateLayer();
    private final RepaintManagerProvider rpmProvider =
            new RepaintManagerProvider() {

                @NotNull
                @Override
                public Class<? extends ForwardingRepaintManager> getForwardingRepaintManagerClass() {
                    return TransformRPMSwingX.class;
                }

                @NotNull
                @Override
                public Class<? extends WrappedRepaintManager> getWrappedRepaintManagerClass() {
                    return TransformRPMFallBack.class;
                }

                @Override
                public boolean isAdequate(@NotNull final Class<? extends RepaintManager> manager) {
                    return manager.isAnnotationPresent(TransformRPMAnnotation.class);
                }
            };
    private final Map<RenderingHints.Key, Object> renderingHints = new HashMap<>();
    private final Set<JComponent> originalDoubleBuffered = new HashSet<>();
    private JComponent view;
    private final PropertyChangeListener viewChangeListener =
            evt -> setView((JComponent) evt.getNewValue());
    @Nullable
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
        JXLayer<? extends JComponent> installedLayer = this.getInstalledLayer();
        if (installedLayer != null) {
            installedLayer.revalidate();
            installedLayer.repaint();
        }
    }

    /**
     * {@link JTextComponent} and its descendants have some caret position problems when used inside a
     * transformed {@link JXLayer}. When you plan to use {@link JTextComponent}(s) inside the
     * hierarchy of a transformed {@link JXLayer}, call this method in an early stage, before
     * instantiating any {@link JTextComponent}.
     *
     * <p>It executes the following method:
     *
     * <pre>
     * System.setProperty(&quot;i18n&quot;, Boolean.TRUE.toString());
     * </pre>
     *
     * <p>As a result, a {@link GlyphPainter} will be selected that uses floating point instead of
     * fixed point calculations.
     */
    public static void prepareForJTextComponent() {
        System.setProperty("i18n", Boolean.TRUE.toString());
    }

    /**
     * Add one rendering hint to the currently active rendering hints.
     *
     * @param key   the key
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
    public void addRenderingHints(@NotNull final Map<RenderingHints.Key, Object> hints) {
        this.renderingHints.putAll(hints);
    }

    /**
     * Get the {@link TransformModel}.
     *
     * @return the {@link TransformModel}
     * @see #setModel(TransformModel)
     */
    @Contract(pure = true)
    @Nullable
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
    @Contract("null -> fail")
    public final void setModel(@Nullable final TransformModel transformModel) throws NullPointerException {
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
     *
     * <p>The {@code size} argument will be used to compute anchor values for some types of
     * transformations. If the {@code size} argument is {@code null} a value of (0,0) is used for the
     * anchor.
     *
     * <p>In {@code enabled} state this method is delegated to the {@link TransformModel} that has
     * been set. Otherwise {@code null} will be returned.
     *
     * @param size  a {@link Dimension} instance to be used for an anchor or {@code null}
     * @param layer the {@link JXLayer}.
     * @return a {@link AffineTransform} instance or {@code null}
     */
    @NotNull
    public AffineTransform getPreferredTransform(
            final Dimension size, final JXLayer<? extends JComponent> layer) {

        return this.transformModel.getPreferredTransform(size, layer);
    }

  /*
   {@inheritDoc}
   <p>
   This implementation does the following:
   <ol>
   <li>
   A {@link BufferedImage} is created the size of the clip bounds of the
   argument graphics object.</li>
   <li>
   A Graphics object is obtained from the image.</li>
   <li>
   The image is filled with a background color.</li>
   <li>
   The image graphics is translated according to x and y of the clip bounds.
   </li>
   <li>
   The clip from the argument graphics object is set to the image graphics.</li>
   <li>
   {@link #configureGraphics(Graphics2D, JXLayer)} is invoked with the image
   graphics as an argument.</li>
   <li>
   {@link #paintLayer(Graphics2D, JXLayer)} is invoked with the image
   graphics as an argument.</li>
   <li>
   The image graphics is disposed.</li>
   <li>
   The image is drawn on the argument graphics object.</li>
   </ol>
  */
  /* @SuppressWarnings("unchecked")
  @Override
  public final void paint(Graphics g, JComponent component) {
  Graphics2D g2 = (Graphics2D) g;
  JXLayer<? extends JComponent> layer = (JXLayer<? extends JComponent>) component;
  Shape clip = g2.getClip();
  Rectangle clipBounds = g2.getClipBounds();
  BufferedImage buffer = layer.getGraphicsConfiguration()
  .createCompatibleImage(clipBounds.width, clipBounds.height,
  Transparency.OPAQUE);//
  Graphics2D g3 = buffer.createGraphics();
  try {
  g3.setColor(this.getBackgroundColor(layer));
  g3.fillRect(0, 0, buffer.getWidth(), buffer.getHeight());
  g3.translate(-clipBounds.x, -clipBounds.y);
  g3.setClip(clip);
  configureGraphics(g3, layer);
  paintLayer(g3, layer);
  } catch (Throwable t) {*/
    /*
     * Under some rare circumstances, the graphics engine may throw a
     * transformation exception like this:
     *
     * sun.dc.pr.PRError: setPenT4: invalid pen transformation
     * (singular)
     *
     * As far as I understand this happens when the result of the
     * transformation has a zero sized surface.
     *
     * It will happen for example when shear X and shear Y are both set
     * to 1.
     *
     * It will also happen when scale X or scale Y are set to 0.
     *
     * Since this Exception only seems to be thrown under the condition
     * of a zero sized painting surface, no harm is done. Therefore the
     * error logging below has been commented out, but remain in the
     * source for the case that someone wants to investigate this
     * phenomenon in more depth.
     *
     * The Exception however MUST be caught, not only to be able dispose
     * the image's graphics object, but also to prevent that JXLayer
     * enters a problematic state (the isPainting flag would not be
     * reset).
     */
    // System.err.println(t);
    // AffineTransform at = g3.getTransform();
    // System.err.println(at);
    // System.err.println("scaleX = " + at.getScaleX() + " scaleY = "
    // + at.getScaleY() + " shearX = " + at.getShearX()
    // + " shearY = " + at.getShearY());
  /*} finally {
  g3.dispose();
  }
  g2.drawImage(buffer, clipBounds.x, clipBounds.y, null);
  setDirty(false);
  }*/

    /**
     * Overridden to replace the {@link LayoutManager}, to add some listeners and to ensure that an
     * appropriate {@link RepaintManager} is installed.
     *
     * @see #uninstallUI(JComponent)
     */
    @Override
    public void installUI(@NotNull final JComponent component) {
        super.installUI(component);
        JXLayer<? extends JComponent> installedLayer = this.getInstalledLayer();
        originalLayout = installedLayer.getLayout();
        installedLayer.addPropertyChangeListener(KEY_VIEW, this.viewChangeListener);
        installedLayer.setLayout(transformLayout);
        setView(installedLayer.getView());
        if (!delegatePossible) {
            RepaintManagerUtils.ensureRepaintManagerSet(installedLayer, rpmProvider);
        }
    }

    /**
     * Overridden to restore the original {@link LayoutManager} and remove some listeners.
     *
     * @param c the component.
     */
    @Override
    public void uninstallUI(@NotNull final JComponent c) {
        JXLayer<? extends JComponent> installedLayer = this.getInstalledLayer();
        Objects.requireNonNull(installedLayer)
               .removePropertyChangeListener(KEY_VIEW, this.viewChangeListener);
        installedLayer.setLayout(originalLayout);
        setView(null);
        super.uninstallUI(c);
    }

    private void setView(final JComponent view) {
        if (delegatePossible) {
            if (this.view != null) {
                SwingUtilities3.setDelegateRepaintManager(this.view, null);
            }
        }
        this.view = view;
        if (delegatePossible) {
            if (this.view != null) {
                SwingUtilities3.setDelegateRepaintManager(this.view, wrappedManager);
            }
        }
        setDirty(true);
    }

    /**
     * Replace the currently active rendering hints with new hints.
     *
     * @param hints the new rendering hints or {@code null} to clear all rendering hints
     */
    public void setRenderingHints(@Nullable final Map<RenderingHints.Key, Object> hints) {
        this.renderingHints.clear();
        if (hints != null) {
            this.renderingHints.putAll(hints);
        }
    }

    /**
     * Primarily intended for use by {@link RepaintManager}.
     *
     * @param rect  a rectangle
     * @param layer the layer
     * @return the argument rectangle if no {@link AffineTransform} is available, else a new rectangle
     */
    public final Rectangle transform(@NotNull final Rectangle rect, final JXLayer<? extends JComponent> layer) {
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
     * @param layer the {@link JXLayer} this {@link TransformUI} is set to
     */
    @Override
    public void updateUI(final JXLayer<? extends JComponent> layer) {
        setDirty(true);
    }

    /*
     * Get the most suitable background color.
     */
    private Color getBackgroundColor(@NotNull final JXLayer<? extends JComponent> layer) {
        Container colorProvider = layer.getView() == null ? layer : layer.getView();
        while (colorProvider != null && !colorProvider.isOpaque()) {
            colorProvider = colorProvider.getParent();
        }
        return colorProvider == null ? SystemColor.desktop : colorProvider.getBackground();
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
     * If the view of the {@link JXLayer} is (partly) obscured by its parent (this is the case when
     * the size of the view (in component space) is larger than the size of the {@link JXLayer}), the
     * obscured parts will not be painted by the super implementation. Therefore, only under this
     * condition, a special painting technique is executed:
     *
     * <ol>
     * <li>All descendants of the {@link JXLayer} are temporarily set to non double buffered.
     * <li>The graphics object is translated for the X and Y coordinates of the view.
     * <li>The view is painted.
     * <li>The original double buffered property is restored for all descendants.
     * </ol>
     *
     * <p>In all other cases, the super method is invoked.
     *
     * <p>The {@code g2} argument is a graphics object obtained from a {@link BufferedImage}.
     *
     * @see #paint(Graphics, JComponent)
     */
    @Override
    protected final void paintLayer(
            @NotNull final Graphics2D g2, @NotNull final JXLayer<? extends JComponent> layer) {
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
     *
     * <p>In {@code enabled} state this method is delegated to the {@link TransformModel} that has
     * been set. Otherwise {@code null} will be returned.
     */
    @NotNull
    @Override
    protected final AffineTransform getTransform(final JXLayer<? extends JComponent> layer) {
        return transformModel.getTransform(layer);
    }

    /**
     * Get the rendering hints.
     *
     * @return the rendering hints
     * @see #setRenderingHints(Map)
     * @see #addRenderingHints(Map)
     * @see #addRenderingHint(RenderingHints.Key, Object)
     */
    @NotNull
    @Override
    protected Map<RenderingHints.Key, Object> getRenderingHints(final JXLayer<? extends JComponent> layer) {
        return renderingHints;
    }

    /**
     * A delegate {@link RepaintManager} that can be set on the view of a {@link JXLayer} in Java
     * versions starting with Java 6u10.
     *
     * <p>For older Java versions, {@link RepaintManager#setCurrentManager(RepaintManager)} will be
     * used with either {@link TransformRPMFallBack} or {@link TransformRPMSwingX}.
     */
    protected static final class TransformRepaintManager extends RepaintManager {

        private TransformRepaintManager() {
        }

        /**
         * Finds the JXLayer ancestor and have ancestor marked invalid via the current {@link
         * RepaintManager}.
         */
        @Override
        public void addInvalidComponent(final JComponent invalidComponent) {
            JXLayer<? extends JComponent> layer = findJXLayer(invalidComponent);
            RepaintManager.currentManager(layer).addInvalidComponent(layer);
        }

        /**
         * Finds the JXLayer ancestor and have the ancestor marked as dirty with the transformed
         * rectangle via the current {@link RepaintManager}.
         */
        @Override
        public void addDirtyRegion(@NotNull final JComponent c, final int x, final int y, final int w, final int h) {
            if (c.isShowing()) {
                JXLayer<? extends JComponent> layer = findJXLayer(c);
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

        /**
         * Find the ancestor {@link JXLayer} instance.
         *
         * @param c a component
         * @return the ancestor {@link JXLayer} instance
         */
        @NotNull
        @SuppressWarnings("unchecked")
        private JXLayer<? extends JComponent> findJXLayer(final JComponent c) {
            JXLayer<?> layer = (JXLayer<?>) SwingUtilities.getAncestorOfClass(JXLayer.class, c);
            if (layer != null) {
                LayerUI<?> layerUI = layer.getUI();
                if (layerUI instanceof TransformUI) {
                    return (JXLayer<? extends JComponent>) layer;
                } else {
                    return findJXLayer(layer);
                }
            }
            throw new Error("No parent JXLayer with TransformUI found");
        }
    }
}
