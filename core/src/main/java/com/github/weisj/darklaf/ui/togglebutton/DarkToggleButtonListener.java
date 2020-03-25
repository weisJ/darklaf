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
 */
package com.github.weisj.darklaf.ui.togglebutton;

import com.github.weisj.darklaf.ui.button.DarkButtonListener;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import com.github.weisj.darklaf.util.PropertyKey;

import javax.swing.*;
import java.beans.PropertyChangeEvent;

public class DarkToggleButtonListener extends DarkButtonListener implements ToggleButtonConstants {


    public DarkToggleButtonListener(final AbstractButton b, final DarkButtonUI ui) {
        super(b, ui);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        AbstractButton b = (AbstractButton) evt.getSource();
        String key = evt.getPropertyName();
        if (ToggleButtonConstants.KEY_VARIANT.equals(key)) {
            Object oldVal = evt.getOldValue();
            Object newVal = evt.getNewValue();
            if (oldVal != null && oldVal.equals(newVal)) {
                return;
            }
            if (VARIANT_SLIDER.equals(newVal)) {
                b.setBorderPainted(false);
            } else {
                b.setBorderPainted(true);
            }
        } else if (PropertyKey.COMPONENT_ORIENTATION.equals(key)) {
            b.doLayout();
            b.repaint();
        }
    }
}
