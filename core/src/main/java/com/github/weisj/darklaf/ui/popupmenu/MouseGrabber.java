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
package com.github.weisj.darklaf.ui.popupmenu;

import java.applet.Applet;
import java.awt.*;
import java.awt.event.*;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.List;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import sun.awt.SunToolkit;
import sun.awt.UngrabEvent;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

public class MouseGrabber implements ChangeListener, AWTEventListener, ComponentListener, WindowListener {

    Window grabbedWindow;
    MenuElement[] lastPathSelected;

    public void install() {
        MenuSelectionManager msm = MenuSelectionManager.defaultManager();
        msm.addChangeListener(this);
        this.lastPathSelected = msm.getSelectedPath();
        if (this.lastPathSelected.length != 0) {
            grabWindow(this.lastPathSelected);
        }
    }

    protected void grabWindow(final MenuElement[] newPath) {
        // A grab needs to be added
        final Toolkit tk = Toolkit.getDefaultToolkit();
        AccessController.doPrivileged((PrivilegedAction<Object>) () -> {
            tk.addAWTEventListener(MouseGrabber.this, AWTEvent.MOUSE_EVENT_MASK | AWTEvent.MOUSE_MOTION_EVENT_MASK
                    | AWTEvent.MOUSE_WHEEL_EVENT_MASK | AWTEvent.WINDOW_EVENT_MASK | SunToolkit.GRAB_EVENT_MASK);
            return null;
        });

        Component invoker = newPath[0].getComponent();
        if (invoker instanceof JPopupMenu) {
            invoker = ((JPopupMenu) invoker).getInvoker();
        }
        grabbedWindow = DarkUIUtil.getWindow(invoker);
        if (grabbedWindow != null) {
            if (tk instanceof SunToolkit) {
                ((SunToolkit) tk).grab(grabbedWindow);
            } else {
                grabbedWindow.addComponentListener(this);
                grabbedWindow.addWindowListener(this);
            }
        }
    }

    public void uninstall() {
        MenuSelectionManager.defaultManager().removeChangeListener(this);
        ungrabWindow();
    }

    protected void ungrabWindow() {
        final Toolkit tk = Toolkit.getDefaultToolkit();
        // The grab should be removed
        java.security.AccessController.doPrivileged((PrivilegedAction<Object>) () -> {
            tk.removeAWTEventListener(MouseGrabber.this);
            return null;
        });
        realUngrabWindow();
    }

    protected void realUngrabWindow() {
        Toolkit tk = Toolkit.getDefaultToolkit();
        if (grabbedWindow != null) {
            if (tk instanceof SunToolkit) {
                ((SunToolkit) tk).ungrab(grabbedWindow);
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

        repaintIfNecessary(e);
        lastPathSelected = p;
    }

    protected void repaintIfNecessary(final ChangeEvent e) {
        Object source = e.getSource();
        if (source instanceof MenuSelectionManager) {
            MenuSelectionManager manager = (MenuSelectionManager) source;
            MenuElement[] path = manager.getSelectedPath();
            repaintPath(path);
            repaintPath(lastPathSelected);
        }
    }

    private void repaintPath(final MenuElement[] path) {
        if (path != null && path.length > 0) {
            MenuElement menuElement = path[path.length - 1];
            Component comp = menuElement.getComponent();
            if (comp.isVisible() && !(comp instanceof JPopupMenu)) {
                OverlayScrollPane sp = DarkUIUtil.getParentOfType(OverlayScrollPane.class, comp, 5);
                if (sp != null) {
                    DarkUIUtil.repaint(sp, SwingUtilities.convertRectangle(comp, comp.getBounds(), sp));
                }
            }
        }
    }

    public void eventDispatched(final AWTEvent ev) {
        if (ev instanceof UngrabEvent) {
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
        switch (me.getID()) {
            /*
             * Changed here: Make doNotCancelPopup accessible to all component. Allows for more versatile
             * PopupMenus.
             */
            case MouseEvent.MOUSE_PRESSED:
                if (isInPopup(src) || (src instanceof JMenu && ((JMenu) src).isSelected())) {
                    return;
                }
                if (!PropertyUtil.isPropertyEqual(src, DarkPopupMenuUI.KEY_DO_NOT_CANCEL_POPUP,
                        DarkPopupMenuUI.HIDE_POPUP_VALUE)) {
                    // Cancel popup only if this property was not set.
                    // If this property is set to TRUE component wants
                    // to deal with this event by himself.
                    cancelPopupMenu();
                    // Ask UIManager about should we consume event that closes
                    // popup. This made to match native apps behaviour.
                    boolean consumeEvent = UIManager.getBoolean("PopupMenu.consumeEventOnClose");
                    // Consume the event so that normal processing stops.
                    /*
                     * Changed here: Enable event consumption for specific sources through client property.
                     */
                    if ((consumeEvent && !(src instanceof MenuElement)
                            || PropertyUtil.getBooleanProperty(src, DarkPopupMenuUI.KEY_CONSUME_EVENT_ON_CLOSE))) {
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
                if (isInPopup(src) || ((src instanceof JComboBox) && ((JComboBox<?>) src).isPopupVisible())
                        || ((src instanceof JWindow) && src.isVisible())
                        || ((src instanceof JMenuItem) && src.isVisible()) || (src instanceof JFrame)
                        || (src instanceof JDialog)) {
                    return;
                }
                cancelPopupMenu();
                break;
        }
    }

    protected void cancelPopupMenu() {
        // We should ungrab window if a user code throws
        // an unexpected runtime exception. See 6495920.
        try {
            // 4234793: This action should call firePopupMenuCanceled but it's
            // a protected method. The real solution could be to make
            // firePopupMenuCanceled public and call it directly.
            List<JPopupMenu> popups = DarkPopupMenuUI.getPopups();
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
    protected boolean isInPopup(final Component src) {
        for (Component c = src; c != null; c = c.getParent()) {
            if (c instanceof Applet || c instanceof Window) {
                break;
            } else if (c instanceof JPopupMenu) {
                return true;
            } else if (PropertyUtil.getBooleanProperty(c, DarkPopupMenuUI.KEY_DO_NOT_CANCEL_ON_SCROLL)) {
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

    public void windowOpened(final WindowEvent e) {}

    public void windowClosing(final WindowEvent e) {
        cancelPopupMenu();
    }

    public void windowClosed(final WindowEvent e) {
        cancelPopupMenu();
    }

    public void windowIconified(final WindowEvent e) {
        cancelPopupMenu();
    }

    public void windowDeiconified(final WindowEvent e) {}

    public void windowActivated(final WindowEvent e) {}

    public void windowDeactivated(final WindowEvent e) {
        cancelPopupMenu();
    }
}
