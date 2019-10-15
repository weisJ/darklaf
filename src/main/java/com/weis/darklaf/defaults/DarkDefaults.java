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
package com.weis.darklaf.defaults;

import javax.swing.*;

@SuppressWarnings("WeakerAccess")
public class DarkDefaults {

    protected static DarkDefaults instance;
    protected int ButtonArc = -1;
    protected int ButtonFocusArc = -1;
    protected int ButtonSquareFocusArc = -1;
    protected int ButtonBorderSize = -1;
    protected int ButtonShadowSize = -1;
    protected int SpinnerArc = -1;
    protected int TextFieldBorderSize = -1;
    protected int SpinnerBorderSize = -1;
    protected int TabFrameTabHeight = -1;
    protected int TaskPaneArc = -1;
    protected int ButtonSquareArc = -1;
    protected int ButtonMinimumArc = -1;

    public static DarkDefaults get() {
        if (instance == null) {
            instance = new DarkDefaults();
        }
        return instance;
    }

    public static void uninstall() {
        if (instance == null) return;
        instance.uninstallDefaults();
        instance = null;
    }

    protected void uninstallDefaults() {
        ButtonArc = -1;
        ButtonFocusArc = -1;
        ButtonSquareFocusArc = -1;
        ButtonBorderSize = -1;
        ButtonShadowSize = -1;
        SpinnerArc = -1;
        TextFieldBorderSize = -1;
        SpinnerBorderSize = -1;
        TabFrameTabHeight = -1;
        TaskPaneArc = -1;
        ButtonSquareArc = -1;
        ButtonMinimumArc = -1;
    }

    public int getButtonArc() {
        if (ButtonArc < 0) {
            ButtonArc = UIManager.getInt("Button.arc");
        }
        return ButtonArc;
    }

    public int getButtonFocusArc() {
        if (ButtonFocusArc < 0) {
            ButtonFocusArc = UIManager.getInt("Button.focusArc");
        }
        return ButtonFocusArc;
    }

    public int getButtonSquareFocusArc() {
        if (ButtonSquareFocusArc < 0) {
            ButtonSquareFocusArc = UIManager.getInt("Button.squareFocusArc");
        }
        return ButtonSquareFocusArc;
    }

    public int getButtonBorderSize() {
        if (ButtonBorderSize < 0) {
            ButtonBorderSize = UIManager.getInt("Button.borderThickness");
        }
        return ButtonBorderSize;
    }

    public int getButtonShadowSize() {
        if (ButtonShadowSize < 0) {
            ButtonShadowSize = UIManager.getInt("Button.shadowHeight");
        }
        return ButtonShadowSize;
    }

    public int getSpinnerArc() {
        if (SpinnerArc < 0) {
            SpinnerArc = UIManager.getInt("Spinner.arc");
        }
        return SpinnerArc;
    }

    public int getTextFieldBorderSize() {
        if (TextFieldBorderSize < 0) {
            TextFieldBorderSize = UIManager.getInt("TextField.borderThickness");
        }
        return TextFieldBorderSize;
    }

    public int getSpinnerBorderSize() {
        if (SpinnerBorderSize < 0) {
            SpinnerBorderSize = UIManager.getInt("Spinner.borderThickness");
        }
        return SpinnerBorderSize;
    }

    public int getTabFrameTabHeight() {
        if (TabFrameTabHeight < 0) {
            TabFrameTabHeight = UIManager.getInt("TabFrame.tabHeight");
        }
        return TabFrameTabHeight;
    }

    public int getTaskPaneArc() {
        if (TaskPaneArc < 0) {
            TaskPaneArc = UIManager.getInt("TaskPane.arc");
        }
        return TaskPaneArc;
    }

    public int getButtonSquareArc() {
        if (ButtonSquareArc < 0) {
            ButtonSquareArc = UIManager.getInt("Button.squareArc");
        }
        return ButtonSquareArc;
    }

    public int getButtonMinimumArc() {
        if (ButtonMinimumArc < 0) {
            ButtonMinimumArc = UIManager.getInt("Button.minimumArc");
        }
        return ButtonMinimumArc;
    }
}
