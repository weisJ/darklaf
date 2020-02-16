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
package com.github.weisj.darklaf.components.tooltip;

import com.github.weisj.darklaf.components.alignment.Alignment;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseEvent;

public class TooltipAwareButton extends JButton implements ToolTipAware {

    private final ToolTipContext context = new ToolTipContext(this)
            .setAlignment(Alignment.CENTER)
            .setCenterAlignment(Alignment.SOUTH);

    public TooltipAwareButton() {
        this(null, null);
    }

    public TooltipAwareButton(final String text, final Icon icon) {
        super(text, icon);
    }

    public TooltipAwareButton(final Icon icon) {
        this(null, icon);
    }

    public TooltipAwareButton(final String text) {
        this(text, null);
    }

    public TooltipAwareButton(final Action a) {
        super(a);
    }

    @Override
    public Point getToolTipLocation(final MouseEvent event) {
        return context.getToolTipLocation(event);
    }

    @Override
    public JToolTip createToolTip() {
        return context.getToolTip();
    }

    @Override
    public ToolTipContext getToolTipContext() {
        return context;
    }
}
