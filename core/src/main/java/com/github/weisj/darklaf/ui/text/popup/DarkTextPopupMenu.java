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
package com.github.weisj.darklaf.ui.text.popup;

import java.awt.*;
import java.util.ResourceBundle;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.text.JTextComponent;

import com.github.weisj.darklaf.util.ResourceUtil;

public class DarkTextPopupMenu extends JPopupMenu implements UIResource {

    private final JMenuItem cut;
    private final JMenuItem copy;
    private final JMenuItem paste;

    public DarkTextPopupMenu(final JTextComponent editor) {
        ResourceBundle bundle = ResourceUtil.getResourceBundle("actions", editor);
        cut = new CutMenuItem(bundle.getString("Actions.cut"), editor);
        copy = new CopyMenuItem(bundle.getString("Actions.copy"), editor);
        paste = new PasteMenuItem(bundle.getString("Actions.paste"), editor);
        add(cut);
        add(copy);
        add(paste);
    }

    @Override
    public void show(final Component invoker, final int x, final int y) {
        updateMenuItems();
        if (!(cut.isEnabled() || copy.isEnabled() || paste.isEnabled())) {
            // No action available. Don't show the popup.
            return;
        }
        super.show(invoker, x, y);
    }

    protected void updateMenuItems() {
        updateMenuItem(cut);
        updateMenuItem(copy);
        updateMenuItem(paste);
    }

    protected void updateMenuItem(final JMenuItem item) {
        item.setEnabled(item.isEnabled());
    }
}
