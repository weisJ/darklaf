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
package com.github.weisj.darklaf.ui.button;

import javax.swing.*;
import javax.swing.plaf.basic.BasicButtonListener;
import java.awt.event.MouseEvent;

public class DarkButtonListener extends BasicButtonListener {

    public DarkButtonListener(final AbstractButton b) {
        super(b);
    }

    public void mouseEntered(final MouseEvent e) {
        AbstractButton b = (AbstractButton) e.getSource();
        ButtonModel model = b.getModel();
        if (!SwingUtilities.isLeftMouseButton(e)) {
            model.setRollover(true);
        }
        if (model.isPressed()) {
            model.setArmed(true);
        }
    }

    public void mouseExited(final MouseEvent e) {
        AbstractButton b = (AbstractButton) e.getSource();
        ButtonModel model = b.getModel();
        model.setRollover(false);
        model.setArmed(false);
    }
//
//    public static void loadActionMap(final LazyActionMap map) {
//        map.put(new Actions(Actions.PRESS));
//        map.put(new Actions(Actions.RELEASE));
//    }
//
//
//    /**
//     * Actions for Buttons. Two types of action are supported:
//     * pressed: Moves the button to a pressed state
//     * released: Disarms the button.
//     */
//    protected static class Actions extends UIAction {
//        private static final String PRESS = "pressed";
//        private static final String RELEASE = "released";
//
//        protected Actions(final String name) {
//            super(name);
//        }
//
//        public void actionPerformed(final ActionEvent e) {
//            AbstractButton b = (AbstractButton)e.getSource();
//            if (DarkButtonUI.doConvertToShadow(b) || DarkButtonUI.isShadowVariant(b)) return;
//            String key = getName();
//            if (PRESS.equals(key)) {
//                ButtonModel model = b.getModel();
//                model.setArmed(true);
//                model.setPressed(true);
//                if(!b.hasFocus()) {
//                    b.requestFocus();
//                }
//            }
//            else if (RELEASE.equals(key)) {
//                ButtonModel model = b.getModel();
//                model.setPressed(false);
//                model.setArmed(false);
//            }
//        }
//
//        @Override
//        public boolean accept(final Object sender) {
//            return !((sender instanceof AbstractButton) &&
//                !((AbstractButton)sender).getModel().isEnabled());
//        }
//    }
}
