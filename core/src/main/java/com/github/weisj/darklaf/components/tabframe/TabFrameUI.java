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
package com.github.weisj.darklaf.components.tabframe;

import java.awt.*;

import javax.swing.plaf.ComponentUI;

import com.github.weisj.darklaf.util.Alignment;

public abstract class TabFrameUI extends ComponentUI {

    public abstract int getTabSize(JTabFrame tabFrame);

    public abstract void clearTargetIndicator();

    public abstract void clearSourceIndicator();

    public abstract Color getDragBorderColor();

    public abstract void setSourceIndicator(Alignment a, int tabIndex);

    public abstract void setTargetIndicator(Alignment a, int tabIndex);

    public abstract JTabFrame.TabFramePosition getTabIndexAt(JTabFrame tabFrame, Point p);

    public abstract JTabFrame.TabFramePosition getNearestTabIndexAt(JTabFrame tabFrame, Point p);

    public abstract void setDropSize(final int width, final int height);

    public abstract int getTabWidth(JTabFrame tabFrame, Alignment a, int index);

    public abstract int getTabHeight(JTabFrame tabFrame, Alignment a, int index);

    public abstract Rectangle getTabContainerBounds(JTabFrame tabFrame, Alignment a);

    public abstract JTabFrame.TabFramePosition getDropPosition(JTabFrame tabFrame, Point p);
}
