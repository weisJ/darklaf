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
package com.github.weisj.darklaf.ui.togglebutton.checkbox;

import java.awt.geom.RectangularShape;
import java.awt.geom.RoundRectangle2D;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.ui.togglebutton.radiobutton.DarkRadioButtonUI;

/** @author Jannis Weis */
public class DarkCheckBoxUI extends DarkRadioButtonUI {

    protected JCheckBox checkBox;
    protected int arcSize;
    private Icon stateIcon;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkCheckBoxUI();
    }

    @Override
    protected String getPropertyPrefix() {
        return "CheckBox.";
    }

    @Override
    public void installDefaults(final AbstractButton b) {
        super.installDefaults(b);
        arcSize = UIManager.getInt("CheckBox.arc");
    }

    @Override
    protected void installIcons() {
        stateIcon = UIManager.getIcon("CheckBox.icon");
    }

    @Override
    protected Icon getStateIcon(final AbstractButton b) {
        return stateIcon;
    }

    @Override
    protected RectangularShape calculateHitArea() {
        return new RoundRectangle2D.Float(Math.max(iconRect.x, 0), Math.max(iconRect.y, 0), iconRect.width,
                iconRect.height, arcSize, arcSize);
    }
}
