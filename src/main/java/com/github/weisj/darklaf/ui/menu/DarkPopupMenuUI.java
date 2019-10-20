/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
package com.github.weisj.darklaf.ui.menu;

import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.awt.AppContext;
import sun.awt.SunToolkit;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicPopupMenuUI;
import java.applet.Applet;
import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.lang.reflect.InvocationTargetException;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.List;

/**
 * This implementation for PopupMenuUI is almost identical to the one of BasicPopupMenuUI. The key difference is that it
 * allows components to specify HIDE_POPUP_KEY and set a property to prevent scroll events from closing a popup. This
 * allows for more versatile PopupComponents.
 *
 * @author Jannis Weis
 */
public class DarkPopupMenuUI extends BasicPopupMenuUI {

    public static final StringBufferWrapper HIDE_POPUP_KEY = new StringBufferWrapper(new StringBuffer(
            "doNotCancelPopup"));
    public static final StringBuilder MOUSE_GRABBER_KEY = new StringBuilder(
            "javax.swing.plaf.basic.BasicPopupMenuUI.MouseGrabber");

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent x) {
        return new DarkPopupMenuUI();
    }

    @NotNull
    static List<JPopupMenu> getPopups() {
        MenuSelectionManager msm = MenuSelectionManager.defaultManager();
        MenuElement[] p = msm.getSelectedPath();

        List<JPopupMenu> list = new ArrayList<>(p.length);
        for (MenuElement element : p) {
            if (element instanceof JPopupMenu) {
                list.add((JPopupMenu) element);
            }
        }
        return list;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        removeOldMouseGrabber();
        AppContext context = AppContext.getAppContext();
        synchronized (MOUSE_GRABBER_KEY) {
            MouseGrabber mouseGrabber = (MouseGrabber) context.get(
                    MOUSE_GRABBER_KEY);
            if (mouseGrabber == null) {
                mouseGrabber = new MouseGrabber();
                context.put(MOUSE_GRABBER_KEY, mouseGrabber);
            }
        }
    }

    /**
     * This Method is responsible for removing the old MouseGrabber from the AppContext, to be able to add our own
     * implementation for it that is a bit more generous with closing the popup.
     */
    private void removeOldMouseGrabber() {
        AppContext context = AppContext.getAppContext();
        try {
            var field = BasicPopupMenuUI.class.getDeclaredField("MOUSE_GRABBER_KEY");
            field.setAccessible(true);
            var value = field.get(null);
            var mouseGrabber = context.get(value);
            if (mouseGrabber != null) {
                var method = mouseGrabber.getClass().getDeclaredMethod("uninstall");
                method.setAccessible(true);
                method.invoke(mouseGrabber);
            }
            context.put(value, null);
        } catch (NoSuchFieldException | IllegalAccessException | NoSuchMethodException | InvocationTargetException e) {
            e.printStackTrace();
        }
    }

    public static class MouseGrabber implements ChangeListener,
            AWTEventListener, ComponentListener, WindowListener {

        Window grabbedWindow;
        MenuElement[] lastPathSelected;

        public MouseGrabber() {
            MenuSelectionManager msm = MenuSelectionManager.defaultManager();
            msm.addChangeListener(this);
            this.lastPathSelected = msm.getSelectedPath();
            if (this.lastPathSelected.length != 0) {
                grabWindow(this.lastPathSelected);
            }
        }

        void grabWindow(@NotNull final MenuElement[] newPath) {
            // A grab needs to be added
            final Toolkit tk = Toolkit.getDefaultToolkit();
            java.security.AccessController.doPrivileged(
                    (PrivilegedAction<Object>) () -> {
                        tk.addAWTEventListener(MouseGrabber.this,
                                               AWTEvent.MOUSE_EVENT_MASK
                                                       | AWTEvent.MOUSE_MOTION_EVENT_MASK
                                                       | AWTEvent.MOUSE_WHEEL_EVENT_MASK
                                                       | AWTEvent.WINDOW_EVENT_MASK
                                                       | SunToolkit.GRAB_EVENT_MASK);
                        return null;
                    }
            );

            Component invoker = newPath[0].getComponent();
            if (invoker instanceof JPopupMenu) {
                invoker = ((JPopupMenu) invoker).getInvoker();
            }
            grabbedWindow = (invoker == null)
                            ? null
                            : ((invoker instanceof Window)
                               ? (Window) invoker
                               : SwingUtilities.getWindowAncestor(invoker));
            if (grabbedWindow != null) {
                if (tk instanceof sun.awt.SunToolkit) {
                    ((sun.awt.SunToolkit) tk).grab(grabbedWindow);
                } else {
                    grabbedWindow.addComponentListener(this);
                    grabbedWindow.addWindowListener(this);
                }
            }
        }

        public void uninstall() {
            synchronized (MOUSE_GRABBER_KEY) {
                MenuSelectionManager.defaultManager().removeChangeListener(this);
                ungrabWindow();
                AppContext.getAppContext().remove(MOUSE_GRABBER_KEY);
            }
        }

        void ungrabWindow() {
            final Toolkit tk = Toolkit.getDefaultToolkit();
            // The grab should be removed
            java.security.AccessController.doPrivileged(
                    (PrivilegedAction<Object>) () -> {
                        tk.removeAWTEventListener(MouseGrabber.this);
                        return null;
                    }
            );
            realUngrabWindow();
        }

        void realUngrabWindow() {
            Toolkit tk = Toolkit.getDefaultToolkit();
            if (grabbedWindow != null) {
                if (tk instanceof sun.awt.SunToolkit) {
                    ((sun.awt.SunToolkit) tk).ungrab(grabbedWindow);
                } else {
                    grabbedWindow.removeComponentListener(this);
                    grabbedWindow.removeWindowListener(this);
                }
                grabbedWindow = null;
            }
        }

        public void stateChanged(final ChangeEvent e) {
            MenuSelectionManager msm = MenuSelectionManager.defaultManager();
            MenuElement[] p = msm.getSelectedPath();

            if (lastPathSelected.length == 0 && p.length != 0) {
                grabWindow(p);
            }

            if (lastPathSelected.length != 0 && p.length == 0) {
                ungrabWindow();
            }

            lastPathSelected = p;
        }

        public void eventDispatched(final AWTEvent ev) {
            if (ev instanceof sun.awt.UngrabEvent) {
                // Popup should be canceled in case of ungrab event
                cancelPopupMenu();
                return;
            }
            if (!(ev instanceof MouseEvent)) {
                // We are interested in MouseEvents only
                return;
            }
            MouseEvent me = (MouseEvent) ev;
            Component src = me.getComponent();
            // If the scroll is done inside a combobox, menuitem,
            // or inside a Popup#HeavyWeightWindow or inside a frame
            // popup should not close which is the standard behaviour
            switch (me.getID()) {/*
             * Changed here: Make doNotCancelPopup accessible to all component.
             *               Allows for more versatile PopupMenus.
             */
                case MouseEvent.MOUSE_PRESSED:
                    if (isInPopup(src) ||
                            (src instanceof JMenu && ((JMenu) src).isSelected())) {
                        return;
                    }
                    if (!(src instanceof JComponent) ||
                            !HIDE_POPUP_KEY.equals(((JComponent) src).getClientProperty("doNotCancelPopup"))) {
                        // Cancel popup only if this property was not set.
                        // If this property is set to TRUE component wants
                        // to deal with this event by himself.
                        cancelPopupMenu();
                        // Ask UIManager about should we consume event that closes
                        // popup. This made to match native apps behaviour.
                        boolean consumeEvent = UIManager.getBoolean("PopupMenu.consumeEventOnClose");
                        // Consume the event so that normal processing stops.
                        if (consumeEvent && !(src instanceof MenuElement)) {
                            me.consume();
                        }
                    }
                    break;
                case MouseEvent.MOUSE_RELEASED:
                    if (!(src instanceof MenuElement)) {
                        // Do not forward event to MSM, let component handle it
                        if (isInPopup(src)) {
                            break;
                        }
                    }
                    if (src instanceof JMenu || !(src instanceof JMenuItem)) {
                        MenuSelectionManager.defaultManager().processMouseEvent(me);
                    }
                    break;
                case MouseEvent.MOUSE_DRAGGED:
                    if (!(src instanceof MenuElement)) {
                        // For the MOUSE_DRAGGED event the src is
                        // the Component in which mouse button was pressed.
                        // If the src is in popupMenu,
                        // do not forward event to MSM, let component handle it.
                        if (isInPopup(src)) {
                            break;
                        }
                    }
                    MenuSelectionManager.defaultManager().processMouseEvent(me);
                    break;
                case MouseEvent.MOUSE_WHEEL:
                    if (isInPopup(src)
                            || ((src instanceof JComboBox) && ((JComboBox) src).isPopupVisible())
                            || ((src instanceof JWindow) && src.isVisible())
                            || ((src instanceof JMenuItem) && src.isVisible())
                            || (src instanceof JFrame)
                            || (src instanceof JDialog)) {
                        return;
                    }
                    cancelPopupMenu();
                    break;
            }
        }

        void cancelPopupMenu() {
            // We should ungrab window if a user code throws
            // an unexpected runtime exception. See 6495920.
            try {
                // 4234793: This action should call firePopupMenuCanceled but it's
                // a protected method. The real solution could be to make
                // firePopupMenuCanceled public and call it directly.
                List<JPopupMenu> popups = getPopups();
                for (JPopupMenu popup : popups) {
                    popup.putClientProperty("JPopupMenu.firePopupMenuCanceled", Boolean.TRUE);
                }
                MenuSelectionManager.defaultManager().clearSelectedPath();
            } catch (RuntimeException | Error ex) {
                realUngrabWindow();
                throw ex;
            }
        }

        @SuppressWarnings("deprecation")
        boolean isInPopup(final Component src) {
            for (Component c = src; c != null; c = c.getParent()) {
                if (c instanceof Applet || c instanceof Window) {
                    break;
                } else if (c instanceof JPopupMenu) {
                    return true;
                } else if (c instanceof JComponent
                        && Boolean.TRUE.equals(((JComponent) c).getClientProperty("doNotCancelOnScroll"))) {
                    /*
                     * Change here: allows scrollable components that contain the popupMenu.
                     */
                    return true;
                }
            }
            return false;
        }

        public void componentResized(final ComponentEvent e) {
            cancelPopupMenu();
        }

        public void componentMoved(final ComponentEvent e) {
            cancelPopupMenu();
        }

        public void componentShown(final ComponentEvent e) {
            cancelPopupMenu();
        }

        public void componentHidden(final ComponentEvent e) {
            cancelPopupMenu();
        }

        public void windowOpened(final WindowEvent e) {
        }

        public void windowClosing(final WindowEvent e) {
            cancelPopupMenu();
        }

        public void windowClosed(final WindowEvent e) {
            cancelPopupMenu();
        }

        public void windowIconified(final WindowEvent e) {
            cancelPopupMenu();
        }

        public void windowDeiconified(final WindowEvent e) {
        }

        public void windowActivated(final WindowEvent e) {
        }

        public void windowDeactivated(final WindowEvent e) {
            cancelPopupMenu();
        }
    }

    protected static class StringBufferWrapper {
        private final StringBuffer buffer;

        @Contract(pure = true)
        protected StringBufferWrapper(final StringBuffer buffer) {
            this.buffer = buffer;
        }

        @Contract(value = "null -> false", pure = true)
        @Override
        public boolean equals(final Object obj) {
            if (obj instanceof StringBuffer) {
                return ((StringBuffer) obj).toString().equals(buffer.toString());
            } else if (obj instanceof StringBufferWrapper) {
                //noinspection EqualsBetweenInconvertibleTypes,EqualsOnSuspiciousObject
                return equals(((StringBufferWrapper) obj).buffer);
            }
            return false;
        }
    }
}
