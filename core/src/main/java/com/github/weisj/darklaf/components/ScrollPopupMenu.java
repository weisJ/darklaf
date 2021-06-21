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

import java.awt.Dimension;

import javax.swing.JPopupMenu;

import com.github.weisj.darklaf.ui.popupmenu.DarkPopupMenuUI;

/** @author Jannis Weis */
public class ScrollPopupMenu extends JPopupMenu {

    private final Dimension maxSize = new Dimension(-1, -1);

    {
        putClientProperty(DarkPopupMenuUI.KEY_MAX_POPUP_SIZE, maxSize);
    }

    public ScrollPopupMenu(final int maxHeight) {
        maxSize.height = maxHeight;
    }

    public ScrollPopupMenu(final int maxWidth, final int maxHeight) {
        maxSize.setSize(maxWidth, maxHeight);
    }

    public int getMaxHeight() {
        return maxSize.height;
    }

    public int getMaxWidth() {
        return maxSize.width;
    }

    public void setMaxHeight(final int maxHeight) {
        maxSize.height = maxHeight;
    }

    public void setMaxWidth(final int maxWidth) {
        maxSize.width = maxWidth;
    }
}
