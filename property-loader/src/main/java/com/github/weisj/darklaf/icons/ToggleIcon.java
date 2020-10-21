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
package com.github.weisj.darklaf.icons;

import java.awt.*;

import javax.swing.*;

/** @author Jannis Weis */
public class ToggleIcon implements Icon {

    private final Icon activeIcon;
    private final Icon inactiveIcon;
    private boolean active = true;

    public ToggleIcon(final Icon active, final Icon inactive) {
        this.activeIcon = active;
        this.inactiveIcon = inactive;
    }

    public void setChooseAlternativeIcon(final boolean chooseAlternative) {
        setActive(!chooseAlternative);
    }

    public boolean isChooseAlternativeIcon() {
        return !isActive();
    }

    public void setActive(final boolean active) {
        this.active = active;
    }

    public boolean isActive() {
        return active;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        currentIcon().paintIcon(c, g, x, y);
    }

    private Icon currentIcon() {
        return active ? activeIcon : inactiveIcon;
    }

    @Override
    public int getIconWidth() {
        return currentIcon().getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return currentIcon().getIconHeight();
    }
}
