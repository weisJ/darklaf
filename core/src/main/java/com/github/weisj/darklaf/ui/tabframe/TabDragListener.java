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
package com.github.weisj.darklaf.ui.tabframe;

import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.*;

import org.jdesktop.jxlayer.JXLayer;

import com.github.weisj.darklaf.components.tabframe.TabFrameTab;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.SwingXUtil;

public class TabDragListener extends MouseAdapter {

    private final TabFrameTab tabComponent;
    protected Point origin;

    public TabDragListener(final TabFrameTab tabComponent) {
        this.tabComponent = tabComponent;
    }

    @Override
    public void mousePressed(final MouseEvent e) {
        origin = e.getPoint();
    }

    @Override
    public void mouseReleased(final MouseEvent e) {
        origin = null;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void mouseDragged(final MouseEvent e) {
        if (!tabComponent.isEnabled()) return;
        if (origin == null) origin = e.getPoint();
        if (distance(origin, e.getPoint()) < 100) return;
        TransferHandler th = tabComponent.getTabFrame().getTransferHandler();
        if (th != null && tabComponent.getTabFrame().isDndEnabled()) {
            Point p = e.getPoint();
            JXLayer<? extends JComponent> layer =
                    DarkUIUtil.getParentOfType(JXLayer.class, tabComponent.getComponent(), 3);
            p = SwingXUtil.convertPointToParent(tabComponent.getComponent(), layer, p);
            p = SwingUtilities.convertPoint(layer != null ? layer : tabComponent.getComponent().getParent(), p,
                    tabComponent.getTabFrame());
            tabComponent.getTabFrame().initTransfer(tabComponent.getOrientation(), tabComponent.getIndex());
            th.exportAsDrag(
                    tabComponent.getTabFrame(), new MouseEvent(tabComponent.getTabFrame(), e.getID(), e.getWhen(),
                            e.getModifiersEx(), p.x, p.y, e.getClickCount(), e.isPopupTrigger(), e.getButton()),
                    TransferHandler.MOVE);
        }
    }

    protected int distance(final Point p1, final Point p2) {
        return (p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y);
    }
}
