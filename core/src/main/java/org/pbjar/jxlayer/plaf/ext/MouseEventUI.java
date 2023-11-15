/*
 * MIT License
 *
 * Copyright (c) 2019-2023 Jannis Weis
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
package org.pbjar.jxlayer.plaf.ext;

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.NoninvertibleTransformException;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.LayerUI;

/**
 * This class provides for {@link MouseEvent} re-dispatching. It may be used to set a tool tip on
 * {@link JLayer}'s glass pane and still have the child components receive {@link MouseEvent}s.
 * <p>
 * <b>Note:</b> A {@link MouseEventUI} instance cannot be shared and can be set to a single
 * {@link JLayer} instance only.
 */
public class MouseEventUI<V extends JComponent> extends AbstractLayerUI<V> {

    private Component lastEnteredTarget, lastPressedTarget;
    private boolean dispatchingMode = false;

    private JLayer<? extends V> installedLayer;

    /**
     * Overridden to override the {@link LayerUI} implementation that only consults the view.
     * <p>
     * This implementation is a copy of the {@link ComponentUI#contains(JComponent, int, int)} method.
     */
    @SuppressWarnings("deprecation")
    @Override
    public boolean contains(final JComponent c, final int x, final int y) {
        return c.inside(x, y);
    }

    /**
     * Overridden to check if this {@link LayerUI} has not been installed already, and to set the
     * argument {@code
     * component} as the installed {@link JLayer}.
     *
     * @throws IllegalStateException when this {@link LayerUI} has been installed already
     * @see #getInstalledLayer()
     */
    @SuppressWarnings("unchecked")
    @Override
    public void installUI(final JComponent component) throws IllegalStateException {
        super.installUI(component);
        if (installedLayer != null) {
            throw new IllegalStateException(this.getClass().getName()
                    + " cannot be shared between multiple layers");
        }
        installedLayer = (JLayer<? extends V>) component;
    }

    /**
     * Overridden to remove the installed {@link JLayer}.
     */
    @Override
    public void uninstallUI(final JComponent c) {
        installedLayer = null;
        super.uninstallUI(c);
    }

    /**
     * Overridden to only get the following event types: {@link AWTEvent#MOUSE_EVENT_MASK},
     * {@link AWTEvent#MOUSE_MOTION_EVENT_MASK} and {@link AWTEvent#MOUSE_WHEEL_EVENT_MASK}.
     */
    @Override
    public long getLayerEventMask() {
        return AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK
                | AWTEvent.MOUSE_WHEEL_EVENT_MASK;
    }

    /**
     * Overridden to allow for re-dispatching of mouse events to their intended (visual) recipients,
     * rather than to the components according to their bounds.
     */
    @Override
    public void eventDispatched(final AWTEvent event, final JLayer<? extends V> layer) {
        if (event instanceof MouseEvent mouseEvent) {
            if (!dispatchingMode) {
                // Process an original mouse event
                dispatchingMode = true;
                try {
                    redispatch(mouseEvent, layer);
                } finally {
                    dispatchingMode = false;
                }
            } else {
                // Process a generated mouse event
                /*
                 * Added a check, because on mouse entered or exited, the cursor may be set to specific dragging
                 * cursors.
                 */
                if (MouseEvent.MOUSE_ENTERED == mouseEvent.getID()
                        || MouseEvent.MOUSE_EXITED == mouseEvent.getID()) {
                    layer.getGlassPane().setCursor(null);
                } else {
                    Component component = mouseEvent.getComponent();
                    layer.getGlassPane().setCursor(component.getCursor());
                }
            }
        }
    }

    /**
     * Re-dispatches the event to the first component in the hierarchy that has a
     * {@link MouseWheelListener} registered.
     */
    @Override
    protected void processMouseWheelEvent(final MouseWheelEvent event,
            final JLayer<? extends V> jxlayer) {
        /*
         * Only process an event if it is not already consumed. This may be the case if this LayerUI is
         * contained in a wrapped hierarchy.
         */
        if (!event.isConsumed()) {
            /*
             * Since we will create a new event, the argument event must be consumed.
             */
            event.consume();
            /*
             * Find a target up in the hierarchy that has MouseWheelEventListeners registered.
             */
            Component target = event.getComponent();
            Component newTarget = findWheelListenerComponent(target);
            if (newTarget == null) {
                newTarget = jxlayer.getParent();
            }
            /*
             * Convert the location relative to the new target
             */
            Point point = SwingUtilities.convertPoint(event.getComponent(),
                    event.getPoint(), newTarget);
            /*
             * Create a new event and dispatch it.
             */
            newTarget.dispatchEvent(createMouseWheelEvent(event, point,
                    newTarget));
        }
    }

    private Point calculateTargetPoint(final JLayer<? extends V> layer,
            final MouseEvent mouseEvent) {
        Point point = mouseEvent.getPoint();
        SwingUtilities.convertPointToScreen(point, mouseEvent.getComponent());
        SwingUtilities.convertPointFromScreen(point, layer);

        return transformPoint(layer, point);
    }

    private MouseWheelEvent createMouseWheelEvent(final MouseWheelEvent mouseWheelEvent, final Point point,
            final Component target) {
        return new MouseWheelEvent(target,
                mouseWheelEvent.getID(),
                mouseWheelEvent.getWhen(),
                mouseWheelEvent.getModifiersEx(),
                point.x,
                point.y,
                mouseWheelEvent.getClickCount(),
                mouseWheelEvent.isPopupTrigger(),
                mouseWheelEvent.getScrollType(),
                mouseWheelEvent.getScrollAmount(),
                mouseWheelEvent.getWheelRotation());
    }

    private void dispatchMouseEvent(final MouseEvent mouseEvent) {
        if (mouseEvent != null) {
            Component target = mouseEvent.getComponent();
            target.dispatchEvent(mouseEvent);
        }
    }

    private Component findWheelListenerComponent(final Component target) {
        if (target == null) {
            return null;
        } else if (target.getMouseWheelListeners().length == 0) {
            return findWheelListenerComponent(target.getParent());
        } else {
            return target;
        }
    }

    private void generateEnterExitEvents(final JLayer<? extends V> layer,
            final MouseEvent originalEvent, final Component newTarget,
            final Point realPoint) {
        if (lastEnteredTarget != newTarget) {
            dispatchMouseEvent(transformMouseEvent(layer, originalEvent,
                    lastEnteredTarget, realPoint, MouseEvent.MOUSE_EXITED));
            lastEnteredTarget = newTarget;
            dispatchMouseEvent(transformMouseEvent(layer, originalEvent,
                    lastEnteredTarget, realPoint, MouseEvent.MOUSE_ENTERED));
        }
    }

    @SuppressWarnings("DuplicatedCode")

    private Component getListeningComponent(final MouseEvent event, final Component component) {
        Component comp = switch (event.getID()) {
            case MouseEvent.MOUSE_CLICKED, MouseEvent.MOUSE_ENTERED, MouseEvent.MOUSE_EXITED, MouseEvent.MOUSE_PRESSED, MouseEvent.MOUSE_RELEASED -> getMouseListeningComponent(
                    component);
            case MouseEvent.MOUSE_DRAGGED, MouseEvent.MOUSE_MOVED -> getMouseMotionListeningComponent(component);
            case MouseEvent.MOUSE_WHEEL -> getMouseWheelListeningComponent(component);
            default -> null;
        };
        return comp;
    }

    private Component getMouseListeningComponent(final Component component) {
        if (component.getMouseListeners().length > 0) {
            return component;
        } else {
            Container parent = component.getParent();
            if (parent != null) {
                return getMouseListeningComponent(parent);
            } else {
                return null;
            }
        }
    }

    private Component getMouseMotionListeningComponent(final Component component) {
        /*
         * Mouse motion events may result in MOUSE_ENTERED and MOUSE_EXITED.
         *
         * Therefore, components with MouseListeners registered should be returned as well.
         */
        if (component.getMouseMotionListeners().length > 0
                || component.getMouseListeners().length > 0) {
            return component;
        } else {
            Container parent = component.getParent();
            if (parent != null) {
                return getMouseMotionListeningComponent(parent);
            } else {
                return null;
            }
        }
    }

    private Component getMouseWheelListeningComponent(final Component component) {
        if (component.getMouseWheelListeners().length > 0) {
            return component;
        } else {
            Container parent = component.getParent();
            if (parent != null) {
                return getMouseWheelListeningComponent(parent);
            } else {
                return null;
            }
        }
    }

    private Component getTarget(final JLayer<? extends V> layer, final Point targetPoint) {
        Component view = layer.getView();
        if (view == null) {
            return null;
        } else {
            Point viewPoint = SwingUtilities.convertPoint(layer, targetPoint, view);
            return SwingUtilities.getDeepestComponentAt(view, viewPoint.x, viewPoint.y);
        }
    }

    @SuppressWarnings("Duplicates")
    private void redispatch(final MouseEvent originalEvent,
            final JLayer<? extends V> layer) {
        if (layer.getView() != null) {
            if (originalEvent.getComponent() != layer.getGlassPane()) {
                originalEvent.consume();
            }
            MouseEvent newEvent = null;

            Point realPoint = calculateTargetPoint(layer, originalEvent);
            Component realTarget = getTarget(layer, realPoint);
            if (realTarget != null) {
                realTarget = getListeningComponent(originalEvent, realTarget);
            }

            switch (originalEvent.getID()) {
                case MouseEvent.MOUSE_PRESSED:
                    newEvent = transformMouseEvent(layer, originalEvent, realTarget, realPoint);
                    if (newEvent != null) {
                        lastPressedTarget = newEvent.getComponent();
                    }
                    break;
                case MouseEvent.MOUSE_RELEASED:
                    newEvent = transformMouseEvent(layer, originalEvent, lastPressedTarget, realPoint);
                    lastPressedTarget = null;
                    break;
                case MouseEvent.MOUSE_ENTERED:
                case MouseEvent.MOUSE_EXITED:
                    generateEnterExitEvents(layer, originalEvent, realTarget, realPoint);
                    break;
                case MouseEvent.MOUSE_MOVED:
                    newEvent = transformMouseEvent(layer, originalEvent, realTarget, realPoint);
                    generateEnterExitEvents(layer, originalEvent, realTarget, realPoint);
                    break;
                case MouseEvent.MOUSE_DRAGGED:
                    newEvent = transformMouseEvent(layer, originalEvent, lastPressedTarget, realPoint);
                    generateEnterExitEvents(layer, originalEvent, realTarget, realPoint);
                    break;
                case MouseEvent.MOUSE_CLICKED:
                    newEvent = transformMouseEvent(layer, originalEvent, realTarget, realPoint);
                    break;
                case MouseEvent.MOUSE_WHEEL:
                    redispatchMouseWheelEvent((MouseWheelEvent) originalEvent, realTarget, layer);
                    break;
            }
            dispatchMouseEvent(newEvent);
        }
    }

    private void redispatchMouseWheelEvent(final MouseWheelEvent mouseWheelEvent,
            final Component target, final JLayer<? extends V> layer) {
        MouseWheelEvent newEvent = this.transformMouseWheelEvent(mouseWheelEvent, target, layer);
        processMouseWheelEvent(newEvent, layer);
    }

    private MouseEvent transformMouseEvent(final JLayer<? extends V> layer,
            final MouseEvent mouseEvent, final Component target, final Point realPoint) {
        return transformMouseEvent(layer, mouseEvent, target, realPoint,
                mouseEvent.getID());
    }

    private MouseEvent transformMouseEvent(final JLayer<? extends V> layer,
            final MouseEvent mouseEvent, final Component target, final Point targetPoint,
            final int id) {
        if (target == null) {
            return null;
        } else {
            Point newPoint = new Point(targetPoint);
            SwingUtilities.convertPointToScreen(newPoint, layer);
            SwingUtilities.convertPointFromScreen(newPoint, target);
            return new MouseEvent(target,
                    id,
                    mouseEvent.getWhen(),
                    mouseEvent.getModifiersEx(),
                    newPoint.x,
                    newPoint.y,
                    mouseEvent.getClickCount(),
                    mouseEvent.isPopupTrigger(),
                    mouseEvent.getButton());
        }
    }

    private MouseWheelEvent transformMouseWheelEvent(final MouseWheelEvent mouseWheelEvent, final Component t,
            final JLayer<? extends V> layer) {
        Component target = t;
        if (target == null) {
            target = layer;
        }
        Point point = SwingUtilities.convertPoint(mouseWheelEvent.getComponent(),
                mouseWheelEvent.getPoint(), target);
        return createMouseWheelEvent(mouseWheelEvent,
                point, target);
    }

    private Point transformPoint(final JLayer<? extends V> layer, final Point point) {
        AffineTransform transform = this.getTransform(layer);
        if (transform != null) {
            try {
                transform.inverseTransform(point, point);
            } catch (NoninvertibleTransformException e) {
                throw new IllegalArgumentException(e);
            }
        }
        return point;
    }

    protected JLayer<? extends V> getInstalledLayer() {
        return installedLayer;
    }
}
