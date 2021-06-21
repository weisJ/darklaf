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
package com.github.weisj.darklaf.components;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.ui.popupmenu.PopupMenuContainer;
import com.github.weisj.darklaf.util.PropertyKey;

/** @author Jannis Weis */
public class ScrollPopupMenu extends JPopupMenu {

    private final PopupMenuContainer popupMenuContainer;
    private int maxHeight;
    private Popup popup;
    private int posX;
    private int posY;
    private boolean isVisible;

    public ScrollPopupMenu(final int maxHeight) {
        popupMenuContainer = new PopupMenuContainer();
        this.maxHeight = maxHeight;
    }

    /**
     * Set the maximum height of the popup. If the size is larger than the specified maximum height the
     * content will be wrapped inside a scroll pane.
     *
     * <p>
     * Note: A value of less than or equal to 0 indicates that the height should not be limited.
     *
     * @param maxHeight the max height to use.
     */
    public void setMaxHeight(final int maxHeight) {
        this.maxHeight = maxHeight;
    }

    protected void showPopup() {
        isVisible = true;
        popup = createPopup();
        popup.show();
    }

    /**
     * Get the scroll pane of the popup.
     *
     * @return scroll pane;
     */
    public JScrollPane getScrollPane() {
        return popupMenuContainer.getScrollPane();
    }

    @Override
    public boolean isVisible() {
        return isVisible;
    }

    @Override
    public void setLocation(final int x, final int y) {
        posX = x;
        posY = y;
    }

    @Override
    public void setVisible(final boolean b) {
        if (b == isVisible()) {
            return;
        }
        if (b) {
            if (isPopupMenu()) {
                MenuElement[] menuElements = new MenuElement[1];
                if (getSubElements().length > 0) {
                    menuElements = new MenuElement[2];
                    menuElements[1] = getSubElements()[0];
                }
                menuElements[0] = this;
                MenuSelectionManager.defaultManager().setSelectedPath(menuElements);
            }
            firePopupMenuWillBecomeVisible();
            showPopup();
            firePropertyChange(PropertyKey.VISIBLE, Boolean.FALSE, Boolean.TRUE);
        } else {
            hidePopup();
        }
    }

    protected void hidePopup() {
        if (popup != null) {
            firePopupMenuWillBecomeInvisible();
            popup.hide();
            isVisible = false;
            popup = null;
            firePropertyChange(PropertyKey.VISIBLE, Boolean.TRUE, Boolean.FALSE);
            if (isPopupMenu()) {
                MenuSelectionManager.defaultManager().clearSelectedPath();
            }
        }
    }

    private Popup createPopup() {
        return popupMenuContainer.createPopup(this, posX, posY, -1, maxHeight);
    }

    @Override
    public void pack() {}

    private boolean isPopupMenu() {
        Component invoker = getInvoker();
        return ((invoker != null) && !(invoker instanceof JMenu));
    }
}
