/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package com.github.weisj.darklaf.icons;

import java.awt.*;
import java.util.List;

import javax.swing.*;

/**
 * Icon that should be used with JCheckBox or JRadioButton. All icons should have the same size.
 */
public class StateIcon implements Icon {

    private final Icon icon;
    private final Icon disabledIcon;
    private final Icon focusedIcon;
    private final Icon selectedIcon;
    private final Icon selectedDisabledIcon;
    private final Icon selectedFocusedIcon;

    public StateIcon(final List<Icon> icons) {
        this(icons.toArray(new Icon[0]));
    }

    public StateIcon(final Icon[] icons) {
        if (icons == null || icons.length < 6) {
            throw new IllegalArgumentException("Not enough icons given: count="
                                               + (icons == null ? "null" : icons.length));
        }
        this.icon = icons[0];
        this.disabledIcon = icons[1];
        this.focusedIcon = icons[2];
        this.selectedIcon = icons[3];
        this.selectedDisabledIcon = icons[4];
        this.selectedFocusedIcon = icons[5];
    }

    public StateIcon(final Icon icon, final Icon disabledIcon,
                     final Icon focusedIcon, final Icon selectedIcon,
                     final Icon selectedDisabledIcon, final Icon selectedFocusedIcon) {
        this.icon = icon;
        this.disabledIcon = disabledIcon;
        this.focusedIcon = focusedIcon;
        this.selectedIcon = selectedIcon;
        this.selectedDisabledIcon = selectedDisabledIcon;
        this.selectedFocusedIcon = selectedFocusedIcon;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        getIcon(c).paintIcon(c, g, x, y);
    }

    public Icon getIcon(final Component c) {
        if (!(c instanceof AbstractButton)) return icon != null ? icon : EmptyIcon.create(0);
        AbstractButton b = (AbstractButton) c;
        boolean selected = b.isSelected();
        boolean enabled = b.isEnabled();
        boolean hasFocus = b.isFocusPainted() && b.hasFocus();
        Icon icn = selected ? enabled ? hasFocus ? selectedFocusedIcon : selectedIcon
                : selectedDisabledIcon
                : enabled ? hasFocus ? focusedIcon : icon
                : disabledIcon;
        return icn != null ? icn : EmptyIcon.create(0);
    }

    @Override
    public int getIconWidth() {
        return icon.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return icon.getIconHeight();
    }
}
