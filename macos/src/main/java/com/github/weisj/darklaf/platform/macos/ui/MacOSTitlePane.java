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
import com.github.weisj.darklaf.platform.macos.JNIDecorationsMacOS;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;

public class MacOSTitlePane extends CustomTitlePane implements LayoutManager {

    private final JRootPane rootPane;
    private Window window;
    private WindowListener windowListener;
    private Color inactiveBackground;
    private Color activeBackground;
    private Color inactiveForeground;
    private Color activeForeground;
    private Color border;
    private DecorationInformation decorationInformation;
    private JLabel titleLabel;

    public MacOSTitlePane(final JRootPane rootPane) {
        super();
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

    public void paintComponent(final Graphics g) {
        Window window = getWindow();
        boolean active = window == null || window.isActive();
        int width = getWidth();
        int height = getHeight();

        Color background = active ? activeBackground : inactiveBackground;

        g.setColor(background);
        g.fillRect(0, 0, width, height);

        boolean isFullscreen = JNIDecorationsMacOS.isFullscreen(decorationInformation.windowHandle);
        if (!isFullscreen) {
            g.setColor(border);
            g.fillRect(0, height - 1, width, 1);
        }
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
        JRootPane rootPane = getRootPane();
        decorationInformation = MacOSDecorationsUtil.installDecorations(rootPane);
        installListeners();
        if (!decorationInformation.titleVisible) {
            titleLabel = new JLabel();
            titleLabel.setFont(titleLabel.getFont().deriveFont(decorationInformation.titleFontSize));
            titleLabel.setForeground(activeForeground);
            add(titleLabel);
        }
    }


    @Override
    public void uninstall() {
        remove(titleLabel);
        titleLabel = null;
        uninstallListeners();
        MacOSDecorationsUtil.uninstallDecorations(decorationInformation);
        decorationInformation = null;
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

    @Override
    public void addLayoutComponent(final String name, final Component comp) {
    }

    @Override
    public void removeLayoutComponent(final Component comp) {
    }

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        if (window == null) return super.getPreferredSize();
        boolean isFullscreen = JNIDecorationsMacOS.isFullscreen(decorationInformation.windowHandle);
        int height = decorationInformation.titleBarHeight;
        if (isFullscreen) {
            height = 0;
        } else if (useCustomTitle()) {
            height = Math.max(height, titleLabel.getPreferredSize().height);
        }
        return new Dimension(0, height);
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        return preferredLayoutSize(parent);
    }

    private boolean useCustomTitle() {
        return titleLabel != null && decorationInformation != null && !decorationInformation.titleVisible;
    }

    @Override
    public void layoutContainer(final Container parent) {
        if (useCustomTitle()) {
            int width = parent.getWidth();
            int height = parent.getHeight();
            int labelWidth = titleLabel.getPreferredSize().width;
            int x = (width - labelWidth) / 2;
            titleLabel.setBounds(x, 0, labelWidth, height);
        }
    }

    protected class WindowHandler extends WindowAdapter {

        public void windowActivated(final WindowEvent ev) {
            if (useCustomTitle()) {
                titleLabel.setForeground(activeForeground);
            }
        }

        public void windowDeactivated(final WindowEvent ev) {
            if (useCustomTitle()) {
                titleLabel.setForeground(inactiveForeground);
            }
        }
    }
}
