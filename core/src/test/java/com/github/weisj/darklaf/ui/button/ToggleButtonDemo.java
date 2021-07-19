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

import javax.swing.*;

import com.github.weisj.darklaf.ui.demo.BaseComponentDemo;
import com.github.weisj.darklaf.ui.demo.DemoExecutor;
import com.github.weisj.darklaf.ui.DemoPanel;
import com.github.weisj.darklaf.ui.DemoResources;
import com.github.weisj.darklaf.ui.togglebutton.ToggleButtonConstants;

import java.util.Arrays;

public class ToggleButtonDemo extends AbstractButtonDemo<JToggleButton> {

    public static void main(final String[] args) {
        DemoExecutor.showDemo(new ToggleButtonDemo());
    }

    @Override
    protected void init() {
        spacer();
        spec(ToggleButtonConstants.KEY_VARIANT,
                Arrays.asList("none", ToggleButtonConstants.VARIANT_SLIDER));
    }

    @Override
    protected JToggleButton createButton() {
        Icon icon = DemoResources.FOLDER_ICON;
        return new JToggleButton("Test ToggleButton", icon);
    }

    @Override
    public String getName() {
        return "ToggleButton Demo";
    }
}
