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
package com.github.weisj.darklaf.platform.macos.ui;

import com.github.weisj.darklaf.decorations.CustomTitlePane;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

public class MacOSTitlePane extends CustomTitlePane {

    private final JRootPane rootPane;
    private Window window;
    private WindowListener windowListener;
    private Color inactiveBackground;
    private Color activeBackground;
    private Color inactiveForeground;
    private Color activeForeground;
    private Color border;
    private int barHeight;
    private DecorationInformation decorationInformation;

    public MacOSTitlePane(final JRootPane rootPane) {
        this.rootPane = rootPane;
        determineColors();
    }

    protected void determineColors() {
        switch (getWindowDecorationStyle()) {
            case JRootPane.ERROR_DIALOG:
                activeBackground = UIManager.getColor("MacOS.OptionPane.errorDialog.titlePane.background");
                activeForeground = UIManager.getColor("MacOS.OptionPane.errorDialog.titlePane.foreground");
                break;
            case JRootPane.QUESTION_DIALOG:
            case JRootPane.COLOR_CHOOSER_DIALOG:
            case JRootPane.FILE_CHOOSER_DIALOG:
                activeBackground = UIManager.getColor("MacOS.OptionPane.questionDialog.titlePane.background");
                activeForeground = UIManager.getColor("MacOS.OptionPane.questionDialog.titlePane.foreground");
                break;
            case JRootPane.WARNING_DIALOG:
                activeBackground = UIManager.getColor("MacOS.OptionPane.warningDialog.titlePane.background");
                activeForeground = UIManager.getColor("MacOS.OptionPane.warningDialog.titlePane.foreground");
                break;
            default: //JRootPane.Frame
                activeBackground = UIManager.getColor("MacOS.TitlePane.background");
                activeForeground = UIManager.getColor("MacOS.TitlePane.foreground");
                break;
        }
        inactiveBackground = UIManager.getColor("MacOS.TitlePane.inactiveBackground");
        inactiveForeground = UIManager.getColor("MacOS.TitlePane.inactiveForeground");
        border = UIManager.getColor("MacOS.TitlePane.borderColor");
    }

    @Override
    public JRootPane getRootPane() {
        return rootPane;
    }

    @Override
    public Dimension getPreferredSize() {
        if (window == null) return super.getPreferredSize();
        return new Dimension(0, barHeight);
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    public void paintComponent(final Graphics g) {
        Window window = getWindow();
        boolean active = window == null || window.isActive();
        int width = getWidth();
        int height = getHeight();

        Color background = active ? activeBackground : inactiveBackground;

        g.setColor(background);
        g.fillRect(0, 0, width, height);

        g.setColor(border);
        g.fillRect(0, height - 1, width, 1);
    }

    public void addNotify() {
        super.addNotify();
        window = SwingUtilities.getWindowAncestor(this);
        install();
    }

    public Window getWindow() {
        return window;
    }

    private int getWindowDecorationStyle() {
        return getRootPane().getWindowDecorationStyle();
    }

    private void install() {
        determineColors();
        barHeight = window.getInsets().top;
        JRootPane rootPane = getRootPane();
        decorationInformation = MacOSDecorationsUtil.installDecorations(rootPane);
        installListeners();
        MacOSDecorationsUtil.setTitleColor(decorationInformation, activeForeground);
    }


    @Override
    public void uninstall() {
        uninstallListeners();
        MacOSDecorationsUtil.uninstallDecorations(decorationInformation);
    }

    private void installListeners() {
        if (window != null) {
            windowListener = createWindowListener();
            window.addWindowListener(windowListener);
        }
    }

    private void uninstallListeners() {
        if (window != null) {
            window.removeWindowListener(windowListener);
            windowListener = null;
        }
    }


    private WindowListener createWindowListener() {
        return new WindowHandler();
    }

    protected class WindowHandler extends WindowAdapter {

        public void windowActivated(final WindowEvent ev) {
            MacOSDecorationsUtil.setTitleColor(decorationInformation, Color.RED);
        }

        public void windowDeactivated(final WindowEvent ev) {
            MacOSDecorationsUtil.setTitleColor(decorationInformation, Color.GREEN);
        }
    }
}
