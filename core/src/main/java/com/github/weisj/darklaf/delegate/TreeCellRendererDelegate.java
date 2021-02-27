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
package com.github.weisj.darklaf.delegate;

import java.awt.*;

import javax.swing.*;
import javax.swing.tree.TreeCellRenderer;

public class TreeCellRendererDelegate implements TreeCellRenderer {

    private TreeCellRenderer delegate;

    public TreeCellRendererDelegate(final TreeCellRenderer renderer) {
        this.delegate = renderer;
    }

    public TreeCellRenderer getDelegate() {
        return delegate;
    }

    public void setDelegate(final TreeCellRenderer delegate) {
        this.delegate = delegate;
    }

    @Override
    public Component getTreeCellRendererComponent(final JTree tree, final Object value, final boolean selected,
            final boolean expanded, final boolean leaf, final int row, final boolean hasFocus) {
        return getDelegate().getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus);
    }
}
