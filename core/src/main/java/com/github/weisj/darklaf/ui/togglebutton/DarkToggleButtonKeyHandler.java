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

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Set;

public class DarkToggleButtonKeyHandler implements KeyListener {

    // This listener checks if the key event is a focus traversal key event
    // on a radio button, consume the event if so and move the focus
    // to next/previous component
    public void keyPressed(final KeyEvent e) {
        AWTKeyStroke stroke = AWTKeyStroke.getAWTKeyStrokeForEvent(e);
        if (stroke != null && e.getSource() instanceof JRadioButton) {
            JRadioButton source = (JRadioButton) e.getSource();
            boolean next = isFocusTraversalKey(source,
                                               KeyboardFocusManager.FORWARD_TRAVERSAL_KEYS,
                                               stroke);
            if (next || isFocusTraversalKey(source,
                                            KeyboardFocusManager.BACKWARD_TRAVERSAL_KEYS,
                                            stroke)) {
                e.consume();
                ButtonGroupInfo btnGroupInfo = new ButtonGroupInfo(source);
                btnGroupInfo.jumpToNextComponent(next);
            }
        }
    }

    private boolean isFocusTraversalKey(final JComponent c, final int id, final AWTKeyStroke stroke) {
        Set<AWTKeyStroke> keys = c.getFocusTraversalKeys(id);
        return keys != null && keys.contains(stroke);
    }

    public void keyReleased(final KeyEvent e) {
    }

    public void keyTyped(final KeyEvent e) {
    }
}
