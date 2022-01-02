/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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
package com.github.weisj.darklaf.ui.popupmenu;

import java.awt.*;
import java.awt.event.AWTEventListener;
import java.awt.event.ComponentListener;
import java.awt.event.WindowListener;

import javax.swing.*;
import javax.swing.event.ChangeListener;

import com.github.weisj.darklaf.ui.util.DarkUIUtil;

public final class MouseGrabberUtil {

    private MouseGrabberUtil() {}

    private static MouseGrabber mouseGrabber;

    public static void uninstallMouseGrabber() {
        if (mouseGrabber != null) {
            mouseGrabber.uninstall();
            mouseGrabber = null;
        }
    }

    public static void installMouseGrabber() {
        if (mouseGrabber == null) {
            uninstallOldMouseGrabber(getOldMouseGrabber());
            mouseGrabber = new MouseGrabber();
            mouseGrabber.install();
        }
    }

    private static ChangeListener getOldMouseGrabber() {
        MenuSelectionManager menuSelectionManager = MenuSelectionManager.defaultManager();
        for (ChangeListener listener : menuSelectionManager.getChangeListeners()) {
            if (listener == null) continue;
            Class<?> listenerClass = listener.getClass();
            if (listenerClass == null) continue;
            Class<?> enclosingClass = listenerClass.getEnclosingClass();
            if (enclosingClass == null) continue;
            if (listenerClass.getName().endsWith("MouseGrabber")
                    && enclosingClass.getName().endsWith("BasicPopupMenuUI")) {
                return listener;
            }
        }
        return null;
    }

    /**
     * This Method is responsible for removing the old MouseGrabber from the AppContext, to be able to
     * add our own implementation for it that is a bit more generous with closing the popup.
     */
    private static void uninstallOldMouseGrabber(final ChangeListener oldMouseGrabber) {
        if (oldMouseGrabber == null) return;
        MenuSelectionManager menuSelectionManager = MenuSelectionManager.defaultManager();
        menuSelectionManager.removeChangeListener(oldMouseGrabber);
        if (oldMouseGrabber instanceof AWTEventListener) {
            Toolkit.getDefaultToolkit().removeAWTEventListener((AWTEventListener) oldMouseGrabber);
        }
        MenuElement[] path = menuSelectionManager.getSelectedPath();
        if (path.length != 0 && path[0] != null) {
            Component invoker = path[0].getComponent();
            if (invoker instanceof JPopupMenu) {
                invoker = ((JPopupMenu) invoker).getInvoker();
            }
            Window grabbedWindow = DarkUIUtil.getWindow(invoker);
            if (oldMouseGrabber instanceof WindowListener) {
                grabbedWindow.removeWindowListener((WindowListener) oldMouseGrabber);
            }
            if (oldMouseGrabber instanceof ComponentListener) {
                grabbedWindow.removeComponentListener((ComponentListener) oldMouseGrabber);
            }
        }
    }
}
