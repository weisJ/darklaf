/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.ui;

import java.awt.dnd.DragSource;
import java.awt.event.MouseEvent;

import javax.swing.*;

import org.jdesktop.swingx.SwingXUtilities;
// import sun.awt.dnd.SunDragSourceContextPeer;
// import sun.awt.AppContext;

/**
 * Drag gesture recognition support for classes that have a <code>TransferHandler</code>. The
 * gesture for a drag in this class is a mouse press followed by movement by
 * <code>DragSource.getDragThreshold()</code> pixels. An instance of this class is maintained per
 * AppContext, and the public static methods call into the appropriate instance.
 * <p>
 * This is a c and p of core (package private) needed for BasicXListUI. It differs from core in that
 * references to sun packages have been replaced.
 * <ul>
 * <li>a static method of SunDragSourceContextPeer has been copied into SwingXUtilities and is used
 * here
 * <li>the shared instance of this class is maintained in the UIManager instead of per appContext.
 * </ul>
 *
 * @author  Shannon Hickey
 * @version 1.2 11/17/05
 */
public class DragRecognitionSupport {
    private int motionThreshold;
    private MouseEvent dndArmedEvent;
    private JComponent component;

    /**
     * Returns whether or not the event is potentially part of a drag sequence.
     *
     * @param  me the MouseEvent.
     * @return    true if mouse is pressed.
     */
    public static boolean mousePressed(final MouseEvent me) {
        return getDragRecognitionSupport().mousePressedImpl(me);
    }

    /**
     * Returns whether or not the event is potentially part of a drag sequence.
     */
    private boolean mousePressedImpl(final MouseEvent me) {
        component = (JComponent) me.getSource();

        if (mapDragOperationFromModifiers(me, component.getTransferHandler()) != TransferHandler.NONE) {

            motionThreshold = DragSource.getDragThreshold();
            dndArmedEvent = me;
            return true;
        }

        clearState();
        return false;
    }

    /**
     * Returns the DragRecognitionSupport for the caller's AppContext.
     */
    private static DragRecognitionSupport getDragRecognitionSupport() {
        // DragRecognitionSupport support =
        // (DragRecognitionSupport)AppContext.getAppContext().
        // get(DragRecognitionSupport.class);
        //
        // if (support == null) {
        // support = new DragRecognitionSupport();
        // AppContext.getAppContext().put(DragRecognitionSupport.class, support);
        // }

        DragRecognitionSupport support =
            (DragRecognitionSupport) UIManager.get("sharedInstance.dragRecognitionSupport");
        if (support == null) {
            support = new DragRecognitionSupport();
            UIManager.put("sharedInstance.dragRecognitionSupport", support);
        }
        return support;
    }

    private int mapDragOperationFromModifiers(final MouseEvent me, final TransferHandler th) {

        if (th == null || !SwingUtilities.isLeftMouseButton(me)) {
            return TransferHandler.NONE;
        }
        // PENDING JW: c'p from SunDragSourceContextPeer
        return SwingXUtilities.convertModifiersToDropAction(me.getModifiersEx(), th.getSourceActions(component));
    }

    private void clearState() {
        dndArmedEvent = null;
        component = null;
    }

    /**
     * If a dnd recognition has been going on, return the MouseEvent that started the recognition.
     * Otherwise, return null.
     *
     * @param  me the MouseEvent.
     * @return    true if mouse has been released.
     */
    public static MouseEvent mouseReleased(final MouseEvent me) {
        return getDragRecognitionSupport().mouseReleasedImpl(me);
    }

    /**
     * If a dnd recognition has been going on, return the MouseEvent that started the recognition.
     * Otherwise, return null.
     */
    private MouseEvent mouseReleasedImpl(final MouseEvent me) {
        /* no recognition has been going on */
        if (dndArmedEvent == null) {
            return null;
        }

        MouseEvent retEvent = null;

        if (me.getSource() == component) {
            retEvent = dndArmedEvent;
        } // else component has changed unexpectedly, so return null

        clearState();
        return retEvent;
    }

    /**
     * Returns whether or not a drag gesture recognition is ongoing.
     *
     * @param  me the me
     * @param  bd the bd
     * @return    the boolean
     */
    public static boolean mouseDragged(final MouseEvent me, final BeforeDrag bd) {
        return getDragRecognitionSupport().mouseDraggedImpl(me, bd);
    }

    /**
     * Returns whether or not a drag gesture recognition is ongoing.
     */
    private boolean mouseDraggedImpl(final MouseEvent me, final BeforeDrag bd) {
        /* no recognition is in progress */
        if (dndArmedEvent == null) {
            return false;
        }

        /* component has changed unexpectedly, so bail */
        if (me.getSource() != component) {
            clearState();
            return false;
        }

        int dx = Math.abs(me.getX() - dndArmedEvent.getX());
        int dy = Math.abs(me.getY() - dndArmedEvent.getY());
        if ((dx > motionThreshold) || (dy > motionThreshold)) {
            TransferHandler th = component.getTransferHandler();
            int action = mapDragOperationFromModifiers(me, th);
            if (action != TransferHandler.NONE) {
                /* notify the BeforeDrag instance */
                if (bd != null) {
                    bd.dragStarting(dndArmedEvent);
                }
                th.exportAsDrag(component, dndArmedEvent, action);
                clearState();
            }
        }

        return true;
    }

    /**
     * This interface allows us to pass in a handler to mouseDragged, so that we can be notified
     * immediately before a drag begins.
     */
    public interface BeforeDrag {
        void dragStarting(MouseEvent me);
    }
}
