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
package com.github.weisj.darklaf.ui.cell.hint;

import java.awt.Component;
import java.awt.Dimension;

import javax.swing.JComponent;

import com.github.weisj.darklaf.ui.HasRendererPane;

public abstract class AbstractIndexedCellContainer<T extends JComponent, I, UI extends HasRendererPane>
        implements IndexedCellContainer<T, I> {

    protected final UI ui;

    protected AbstractIndexedCellContainer(final UI ui) {
        this.ui = ui;
    }

    @Override
    public void addRenderer(final Component renderer) {
        ui.getRendererPane().add(renderer);
    }

    @Override
    public Dimension getRequiredCellSize(final I lastIndex, final Component comp) {
        // Components without a parent may report incorrect preferred/minimum size as the
        // associated FontMetrics have a missing default transform. This is bad for
        // determining the actual needed size on screen.
        // We add the component to the renderer pane to circumvent the issue.
        boolean hasParent = comp.getParent() != null;
        if (!hasParent) {
            addRenderer(comp);
        }
        return comp.getMinimumSize();
    }
}
