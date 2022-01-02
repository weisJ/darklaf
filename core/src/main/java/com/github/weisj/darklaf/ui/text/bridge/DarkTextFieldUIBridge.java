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
package com.github.weisj.darklaf.ui.text.bridge;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.basic.BasicTextFieldUI;
import javax.swing.text.*;

import com.github.weisj.darklaf.ui.text.DarkTextFieldUI;
import com.github.weisj.darklaf.ui.text.DarkTextUI;
import com.github.weisj.darklaf.ui.text.dummy.DummyTextFieldUI;
import com.github.weisj.darklaf.util.value.WeakShared;

/**
 * This class is an exact copy of the implementation of {@link BasicTextFieldUI}. In this way it is
 * possible to contain all Laf specific methods in {@link DarkTextFieldUI}, without having to
 * extends {@link BasicTextFieldUI} directly and instead extend the {@link DarkTextUI} base class.
 *
 * @author Jannis Weis
 */
public abstract class DarkTextFieldUIBridge extends DarkTextUI {

    private static final WeakShared<DummyTextFieldUI> sharedBasicTextFieldUI = new WeakShared<>(DummyTextFieldUI::new);

    private DummyTextFieldUI basicTextFieldUI;

    @Override
    public void installUI(JComponent c) {
        basicTextFieldUI = sharedBasicTextFieldUI.get();
        super.installUI(c);
    }

    @Override
    public void uninstallUI(JComponent c) {
        super.uninstallUI(c);
        basicTextFieldUI = null;
    }

    @Override
    protected String getPropertyPrefix() {
        return "TextField";
    }

    @Override
    public View create(final Element elem) {
        return basicTextFieldUI.create(elem);
    }

    @Override
    public int getBaseline(final JComponent c, final int width, final int height) {
        basicTextFieldUI.setRootView(getRootView((JTextComponent) c));
        int baseLine = basicTextFieldUI.getBaseline(c, width, height);
        basicTextFieldUI.setRootView(null);
        return baseLine;
    }

    @Override
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(final JComponent c) {
        return basicTextFieldUI.getBaselineResizeBehavior(c);
    }
}
