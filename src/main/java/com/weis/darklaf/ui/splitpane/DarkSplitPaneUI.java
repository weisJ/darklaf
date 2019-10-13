/*
 * MIT License
 *
 * Copyright (c) 2019 Jannis Weis
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
package com.weis.darklaf.ui.splitpane;

import com.weis.darklaf.decorators.LayoutManagerDelegate;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicSplitPaneDivider;
import javax.swing.plaf.basic.BasicSplitPaneUI;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkSplitPaneUI extends BasicSplitPaneUI implements PropertyChangeListener {

    private static final int DIVIDER_DRAG_SIZE = 10;
    private static final int DIVIDER_DRAG_OFFSET = 5;
    private Style style;

    protected DarkSplitPaneUI(final Style style) {
        this.style = style;
    }

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkSplitPaneUI(Style.DEFAULT);
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        splitPane.setContinuousLayout(true);
        splitPane.setComponentZOrder(getDivider(), 0);
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
        if (style == Style.DEFAULT) {
            return new DarkSplitPaneDivider(this);
        } else {
            return new ThinDivider(this);
        }
    }

    @Override
    protected void resetLayoutManager() {
        super.resetLayoutManager();
        splitPane.setLayout(new LayoutManagerDelegate(splitPane.getLayout()) {
            @Override
            public void layoutContainer(final Container parent) {
                super.layoutContainer(parent);
                if (style != Style.DEFAULT) {
                    var bounds = getDivider().getBounds();
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
    public void propertyChange(@NotNull final PropertyChangeEvent evt) {
        var key = evt.getPropertyName();
        if ("JSplitPane.style".equals(key)) {
            var val = evt.getNewValue();
            Style oldStyle = style;
            if (Style.INVISIBLE.name.equals(val)) {
                style = Style.INVISIBLE;
            } else if (Style.LINE.name.equals(val)) {
                style = Style.LINE;
            } else {
                style = Style.DEFAULT;
            }
            if (oldStyle != style) {
                if (style == Style.DEFAULT || oldStyle == Style.DEFAULT) {
                    splitPane.setUI(new DarkSplitPaneUI(style));
                }
            }
        }
    }

    protected Color getDividerLineColor() {
        return UIManager.getColor("SplitPane.dividerLineColor");
    }

    private enum Style {
        DEFAULT("default"),
        LINE("line"),
        INVISIBLE("invisible");

        final private String name;

        @Contract(pure = true)
        Style(final String name) {
            this.name = name;
        }
    }

    private final class ThinDivider extends BasicSplitPaneDivider {

        private ThinDivider(@NotNull final BasicSplitPaneUI ui) {
            super(ui);
        }

        @Contract(pure = true)
        @Override
        public int getDividerSize() {
            return style == Style.LINE ? 1 : 0;
        }

        @Override
        public void paint(@NotNull final Graphics g) {
            if (style == Style.LINE) {
                g.setColor(getDividerLineColor());
                if (orientation == JSplitPane.HORIZONTAL_SPLIT) {
                    g.drawLine(DIVIDER_DRAG_OFFSET, 0, DIVIDER_DRAG_OFFSET, getHeight());
                } else {
                    g.drawLine(0, DIVIDER_DRAG_OFFSET, getWidth(), DIVIDER_DRAG_OFFSET);
                }
            }
        }

        @Override
        protected void dragDividerTo(final int location) {
            super.dragDividerTo(location + DIVIDER_DRAG_OFFSET);
        }

        @Override
        protected void finishDraggingTo(final int location) {
            super.finishDraggingTo(location + DIVIDER_DRAG_OFFSET);
        }
    }
}
