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
package com.github.weisj.darklaf.ui.text.action;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.text.Caret;
import javax.swing.text.DefaultEditorKit;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.ui.text.DarkCaret;

public class DarkKeyTypedAction extends DefaultEditorKit.DefaultKeyTypedAction {

    @Override
    public void actionPerformed(final ActionEvent e) {
        JTextComponent target = getTextComponent(e);
        if ((target != null) && (e != null)) {
            if ((!target.isEditable()) || (!target.isEnabled())) {
                return;
            }
            String content = e.getActionCommand();
            Caret c = target.getCaret();
            if (c instanceof DarkCaret) {
                boolean isDelete = !content.isEmpty();
                if (isDelete) {
                    char key = content.charAt(0);
                    isDelete = key == KeyEvent.VK_DELETE || key == KeyEvent.VK_BACK_SPACE;
                }
                if (((DarkCaret) c).isInsertMode()) {
                    ((DarkCaret) c).setExpandMode(!isDelete);
                }
                super.actionPerformed(e);
                ((DarkCaret) c).setExpandMode(false);
                return;
            }
        }
        super.actionPerformed(e);
    }
}
