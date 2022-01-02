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
package com.github.weisj.darklaf.components.tooltip;

import javax.swing.*;

import com.github.weisj.darklaf.ui.tooltip.DarkToolTipUI;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.LazyValue;

public class TooltipAwareToggleButton extends JToggleButton implements ToolTipAware {

    private final LazyValue<ToolTipContext> context =
            new LazyValue<>(() -> new ToolTipContext().setAlignment(Alignment.SOUTH).setCenterAlignment(Alignment.SOUTH)
                    .setAlignInside(false).setIgnoreBorder(true));

    public TooltipAwareToggleButton() {
        this(null, null);
    }

    public TooltipAwareToggleButton(final String text, final Icon icon) {
        super(text, icon);
        init();
    }

    private void init() {
        putClientProperty(DarkToolTipUI.KEY_CONTEXT, getToolTipContext());
        putClientProperty(DarkToolTipUI.KEY_STYLE, ToolTipStyle.BALLOON);
    }

    public TooltipAwareToggleButton(final Icon icon) {
        this(null, icon);
    }

    public TooltipAwareToggleButton(final String text) {
        this(text, null);
    }

    public TooltipAwareToggleButton(final Action a) {
        super(a);
        init();
    }

    @Override
    public ToolTipContext getToolTipContext() {
        return context.get();
    }
}
