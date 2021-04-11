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
package com.github.weisj.darklaf.swingdsl;

import java.awt.Color;

import javax.swing.Icon;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.UIManager;

import org.jetbrains.annotations.NotNull;

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.swingdsl.laf.ComponentFactoryDelegate;
import com.github.weisj.swingdsl.laf.DefaultComponentFactory;
import com.github.weisj.swingdsl.laf.DefaultWrappedComponent;
import com.github.weisj.swingdsl.laf.StateValue;
import com.github.weisj.swingdsl.laf.WrappedComponent;

public class DarklafComponentFactory extends ComponentFactoryDelegate {

    public DarklafComponentFactory() {
        super(DefaultComponentFactory.create());
    }

    @Override
    public @NotNull WrappedComponent<JScrollPane> createScrollPane(final @NotNull JComponent content) {
        OverlayScrollPane sp = new OverlayScrollPane(content);
        return new DefaultWrappedComponent<>(sp.getScrollPane(), sp);
    }

    @Override
    public @NotNull Color getBorderColor() {
        return UIManager.getColor("border");
    }

    @Override
    public @NotNull StateValue<Color> getDividerColor() {
        return new StateValue<>(
                UIManager.getColor("borderSecondary"),
                UIManager.getColor("widgetBorderInactive"));
    }

    @Override
    public @NotNull Color getHyperlinkColor() {
        return UIManager.getColor("hyperlink");
    }

    @Override
    public @NotNull StateValue<Icon> getExpandedIcon() {
        return new StateValue<>(
                DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowDown.svg"),
                DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowDownDisabled.svg"));
    }

    @Override
    public @NotNull StateValue<Icon> getCollapsedIcon() {
        return new StateValue<>(
                DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowRight.svg"),
                DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowRightDisabled.svg"));
    }
}
