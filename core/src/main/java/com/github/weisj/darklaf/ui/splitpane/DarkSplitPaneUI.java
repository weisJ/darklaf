/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.splitpane;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;

import com.github.weisj.darklaf.delegate.LayoutManagerDelegate;
import com.github.weisj.darklaf.util.PropertyKey;

/** @author Jannis Weis */
public class DarkSplitPaneUI extends BasicSplitPaneUI implements PropertyChangeListener, SplitPaneConstants {

    private static final int DIVIDER_DRAG_SIZE = 10;
    private static final int DIVIDER_DRAG_OFFSET = 5;
    private DividerStyle style;
    private Color dividerLine;

    protected DarkSplitPaneUI(final DividerStyle style) {
        this.style = style;
    }

    public static ComponentUI createUI(final JComponent c) {
        return new DarkSplitPaneUI(DividerStyle.get(UIManager.getString("SplitPane.defaultDividerStyle")));
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        splitPane.setContinuousLayout(true);
        splitPane.setComponentZOrder(getDivider(), 0);
    }

    @Override
    protected void installDefaults() {
        DividerStyle oldStyle = DividerStyle.getNullableStyle(splitPane.getClientProperty(KEY_STYLE));
        if (oldStyle != null) {
            style = oldStyle;
        }
        super.installDefaults();
        dividerLine = UIManager.getColor("SplitPane.dividerLineColor");
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        splitPane.addPropertyChangeListener(this);
    }

    @Override
    public void uninstallUI(final JComponent c) {
        if (splitPane.getLayout() instanceof LayoutManagerDelegate) {
            splitPane.setLayout(null);
        }
        super.uninstallUI(c);
        divider = null;
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        splitPane.removePropertyChangeListener(this);
    }

    @Override
    public BasicSplitPaneDivider createDefaultDivider() {
        if (!style.isThin()) {
            return new DarkSplitPaneDivider(this);
        } else {
            return new ThinSplitPaneDivider(this);
        }
    }

    @Override
    protected Component createDefaultNonContinuousLayoutDivider() {
        return new DarkNonContinuousSplitPaneDivider(this);
    }

    @Override
    public void finishedPaintingChildren(final JSplitPane sp, final Graphics g) {
        int lastDragLocation = getLastDragLocation();
        if (sp == splitPane && lastDragLocation != -1 && !isContinuousLayout() && !draggingHW) {
            Dimension size = splitPane.getSize();

            Component divider = getNonContinuousLayoutDivider();
            Rectangle dividerBounds = divider.getBounds();

            int x = 0;
            int y = 0;
            if (getOrientation() == JSplitPane.HORIZONTAL_SPLIT) {
                x = lastDragLocation;
                divider.setSize(getDivider().getWidth(), size.height);
            } else {
                y = lastDragLocation;
                divider.setSize(size.width, getDivider().getHeight());
            }
            g.translate(x, y);
            divider.paint(g);
            divider.setBounds(dividerBounds);
            g.translate(-x, -y);
        }
    }

    @Override
    protected void resetLayoutManager() {
        super.resetLayoutManager();
        splitPane.setLayout(new LayoutManagerDelegate(splitPane.getLayout()) {
            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (style.isThin()) {
                    Rectangle bounds = getDivider().getBounds();
                    if (getOrientation() == JSplitPane.HORIZONTAL_SPLIT) {
                        bounds.x -= DIVIDER_DRAG_OFFSET;
                        bounds.width = DIVIDER_DRAG_SIZE;
                    } else {
                        bounds.y -= DIVIDER_DRAG_OFFSET;
                        bounds.height = DIVIDER_DRAG_SIZE;
                    }
                    getDivider().setBounds(bounds);
                }
            }
        });
    }

    @Override
    public void propertyChange(final PropertyChangeEvent evt) {
        String key = evt.getPropertyName();
        if (KEY_STYLE.equals(key)) {
            Object val = evt.getNewValue();
            DividerStyle oldStyle = style;
            style = DividerStyle.get(val);
            if (oldStyle != style) {
                if (oldStyle.isThin() != style.isThin()) {
                    splitPane.setUI(new DarkSplitPaneUI(style));
                    return;
                } else {
                    splitPane.doLayout();
                }
            }
            splitPane.repaint();
        } else if (PropertyKey.ORIENTATION.equals(key)) {
            splitPane.revalidate();
        }
    }

    public Color getDividerLineColor() {
        return dividerLine;
    }

    public int getDividerDragOffset() {
        return DIVIDER_DRAG_OFFSET;
    }

    public int getDividerDragSize() {
        return DIVIDER_DRAG_SIZE;
    }

    public DividerStyle getStyle() {
        return style;
    }

    @Override
    public int getMinimumDividerLocation(final JSplitPane jc) {
        int loc = super.getMinimumDividerLocation(jc);
        if (getDivider() instanceof ThinSplitPaneDivider) {
            loc += getDividerDragOffset();
        }
        return loc;
    }
}
