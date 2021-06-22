/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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

import java.awt.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicPopupMenuUI;

import com.github.weisj.darklaf.components.ScrollPopupMenu;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.PropertyUtil;

/**
 * This implementation for PopupMenuUI is almost identical to the one of BasicPopupMenuUI. The key
 * difference is that it allows components to specify HIDE_POPUP_KEY and set a property to prevent
 * scroll events from closing a popup. This allows for more versatile PopupComponents.
 *
 * @author Jannis Weis
 */
public class DarkPopupMenuUI extends BasicPopupMenuUI {

    public static final String KEY_DO_NOT_CANCEL_POPUP = "doNotCancelPopup";
    public static final String KEY_CONSUME_EVENT_ON_CLOSE = "consumeEventOnClose";
    public static final String KEY_DO_NOT_CANCEL_ON_SCROLL = "doNotCancelOnScroll";
    public static final String HIDE_POPUP_VALUE = "doNotCancelPopup";
    public static final String KEY_DEFAULT_LIGHTWEIGHT_POPUPS = "PopupMenu.defaultLightWeightPopups";
    public static final String KEY_MAX_POPUP_SIZE = "maxPopupSize";
    final SizeLock sizeLock = new SizeLock();
    private PopupMenuContainer popupMenuContainer;

    public static ComponentUI createUI(final JComponent x) {
        return new DarkPopupMenuUI();
    }

    public DarkPopupMenuUI() {
        EventHelperUtil.installEventHelper();
    }

    @Override
    public void paint(final Graphics g, final JComponent c) {
        Window window = SwingUtilities.getWindowAncestor(c);
        if (window != null && window.getOpacity() != 1) {
            window.setOpacity(1);
        }
        super.paint(g, c);
    }

    public static List<JPopupMenu> getPopups() {
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
    public void installDefaults() {
        super.installDefaults();
        popupMenu.putClientProperty(DarkPopupFactory.KEY_START_HIDDEN, true);
    }

    private PopupMenuContainer getPopupMenuContainer() {
        if (popupMenuContainer == null && !(popupMenu instanceof ScrollPopupMenu)) {
            popupMenuContainer = new PopupMenuContainer();
        }
        return popupMenuContainer;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        MouseGrabberUtil.installMouseGrabber();
    }

    private Dimension getConstraintSizes(final JComponent popup, final Point pos) {
        Dimension maxSize = PropertyUtil.getObject(popup, KEY_MAX_POPUP_SIZE, Dimension.class);
        if (maxSize == null) {
            maxSize = new Dimension(-1, -1);
        }
        if (pos != null && (maxSize.height <= 0 || maxSize.width <= 0)) {
            Rectangle screenSize = DarkUIUtil.getScreenBounds(popup, pos.x, pos.y, false);
            if (maxSize.height <= 0) maxSize.height = screenSize.height;
            if (maxSize.width <= 0) maxSize.width = screenSize.width;
        }
        return maxSize;
    }

    @Override
    public Popup getPopup(final JPopupMenu popup, final int x, final int y) {
        PopupMenuContainer container = getPopupMenuContainer();
        if (container == null) return super.getPopup(popup, x, y);
        Dimension constraintSize = getConstraintSizes(popup, new Point(x, y));
        try (SizeLock l = sizeLock.lock()) {
            Dimension prefSize = popup.getPreferredSize();
            return container.createPopup(popup, prefSize, x, y, constraintSize.width, constraintSize.height);
        }
    }

    @Override
    public Dimension getPreferredSize(JComponent c) {
        if (sizeLock.isLocked()) return null;
        try (SizeLock l = sizeLock.lock()) {
            Dimension prefSize = c.getPreferredSize();
            Dimension constraintSize = getConstraintSizes(c, null);
            return getPopupMenuContainer().adjustSize(prefSize, constraintSize.width, constraintSize.height);
        }
    }

    @Override
    public Dimension getMinimumSize(JComponent c) {
        if (sizeLock.isLocked()) return null;
        try (SizeLock l = sizeLock.lock()) {
            Dimension prefSize = c.getMinimumSize();
            Dimension constraintSize = getConstraintSizes(c, null);
            return getPopupMenuContainer().adjustSize(prefSize, constraintSize.width, constraintSize.height);
        }
    }

    @Override
    public Dimension getMaximumSize(JComponent c) {
        if (sizeLock.isLocked()) return null;
        try (SizeLock l = sizeLock.lock()) {
            Dimension prefSize = c.getMaximumSize();
            Dimension constraintSize = getConstraintSizes(c, null);
            return getPopupMenuContainer().adjustSize(prefSize, constraintSize.width, constraintSize.height);
        }
    }

    static class SizeLock extends AtomicBoolean implements AutoCloseable {

        boolean isLocked() {
            return get();
        }

        SizeLock lock() {
            set(true);
            return this;
        }

        @Override
        public void close() {
            set(false);
        }
    }
}
