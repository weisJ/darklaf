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
package com.github.weisj.darklaf.ui.button;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.components.color.QuickColorChooser;
import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.util.AlignmentExt;

public abstract class AbstractButtonDemo<T extends AbstractButton> extends BaseComponentDemo {

    @Override
    public JComponent createComponent() {
        return createButton();
    }

    @SuppressWarnings("unchecked")
    protected T getButton() {
        return (T) getComponent();
    }

    @Override
    protected void init() {
        booleanSpec("enabled", JComponent::setEnabled, JComponent::isEnabled);
        booleanSpec("focusable", JComponent::setFocusable, JComponent::isFocusable);
        binaryOptionSpec("LeftToRight",
                JComponent::setComponentOrientation, JComponent::getComponentOrientation,
                ComponentOrientation.LEFT_TO_RIGHT, ComponentOrientation.RIGHT_TO_LEFT);
        booleanSpec("Rollover", AbstractButton::setRolloverEnabled, AbstractButton::isRolloverEnabled);
        booleanSpec("Content Area filled", AbstractButton::setContentAreaFilled, AbstractButton::isContentAreaFilled);
        booleanSpec("Border painted", AbstractButton::setBorderPainted, AbstractButton::isBorderPainted);
        booleanSpec("Focus painted", AbstractButton::setFocusPainted, AbstractButton::isFocusPainted);
        initBaseControls();
        spacer();
        booleanSpec(DarkButtonUI.KEY_SQUARE);
        booleanSpec(DarkButtonUI.KEY_ROUND);
        booleanSpec(DarkButtonUI.KEY_THIN);
        booleanSpec(DarkButtonUI.KEY_ALT_ARC);
        booleanSpec("Button.defaultButtonFollowsFocus",
                (c, b) -> UIManager.put("Button.defaultButtonFollowsFocus", b),
                (c) -> UIManager.getBoolean("Button.defaultButtonFollowsFocus"));
        spacer();
        customControls(cp -> {
            cp.add(new QuickColorChooser(DarkButtonUI.KEY_HOVER_COLOR, Color.BLACK,
                    (b, c) -> getButton().putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, b ? c : null)));
            cp.add(new QuickColorChooser(DarkButtonUI.KEY_CLICK_COLOR, Color.BLACK,
                    (b, c) -> getButton().putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, b ? c : null)));
        });
        spacer();
        binaryOptionSpec("Text enabled",
                AbstractButton::setText, AbstractButton::getText,
                "Test Button", null);
        binaryOptionSpec("Icon enabled",
                AbstractButton::setIcon, AbstractButton::getIcon,
                ((AbstractButton) getComponent()).getIcon(), null);
        spec(DarkButtonUI.KEY_VARIANT,
                DarkButtonUI.VARIANT_NONE,
                DarkButtonUI.VARIANT_BORDERLESS,
                DarkButtonUI.VARIANT_BORDERLESS_RECTANGULAR);
        enumSpecWithNone(DarkButtonUI.KEY_CORNER, AlignmentExt.class);
    }

    @Override
    protected boolean supportsSpec() {
        return true;
    }

    protected void initBaseControls() {}

    protected abstract T createButton();
}
