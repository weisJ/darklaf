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

import com.github.weisj.darklaf.components.OverlayScrollPane;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.swingdsl.laf.CollapsibleComponent;
import com.github.weisj.swingdsl.laf.ComponentFactoryDelegate;
import com.github.weisj.swingdsl.laf.DefaultComponentFactory;
import com.github.weisj.swingdsl.laf.DefaultSupplier;
import com.github.weisj.swingdsl.laf.DefaultWrappedComponent;
import com.github.weisj.swingdsl.laf.SeparatorSpec;
import com.github.weisj.swingdsl.laf.TextProperty;
import com.github.weisj.swingdsl.laf.WrappedComponent;

public class DarklafComponentFactory extends ComponentFactoryDelegate {

    private final DefaultSupplier<Color> lineColorSupplier = enabled -> {
        if (enabled) {
            Color c = UIManager.getColor("borderSecondary");
            if (c != null) return c;
            c = UIManager.getColor("Label.foreground");
            if (c != null) return c;
            return Color.BLACK;
        } else {
            Color c = UIManager.getColor("widgetBorderInactive");
            if (c != null) return c;
            c = UIManager.getColor("Label.disabledForeground");
            if (c != null) return c;
            return Color.GRAY;
        }
    };
    private final DefaultSupplier<Icon> expandedIconSupplier = enabled -> {
        if (enabled) {
            return DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowDown.svg");
        } else {
            return DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowDownDisabled.svg");
        }
    };
    private final DefaultSupplier<Icon> collapsedIconSupplier = enabled -> {
        if (enabled) {
            return DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowRight.svg");
        } else {
            return DarkUIUtil.ICON_LOADER.getIcon("navigation/arrow/thick/arrowRightDisabled.svg");
        }
    };

    public DarklafComponentFactory() {
        super(DefaultComponentFactory.create());
    }

    @Override
    public WrappedComponent<JScrollPane> createScrollPane(final JComponent content) {
        OverlayScrollPane sp = new OverlayScrollPane(content);
        return new DefaultWrappedComponent<>(sp.getScrollPane(), sp);
    }

    @Override
    public SeparatorSpec<JComponent, SeparatorSpec.Default> createSeparatorComponent(final TextProperty label) {
        return new SeparatorSpec<>(null, new SeparatorSpec.Default(lineColorSupplier));
    }

    @Override
    public SeparatorSpec<CollapsibleComponent, SeparatorSpec.DefaultCollapsible> createCollapsibleSeparatorComponent(
            final TextProperty label) {
        return new SeparatorSpec<>(null,
                new SeparatorSpec.DefaultCollapsible(
                        lineColorSupplier,
                        collapsedIconSupplier,
                        expandedIconSupplier));
    }
}
