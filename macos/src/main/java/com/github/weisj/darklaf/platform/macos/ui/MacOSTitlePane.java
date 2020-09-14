/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.platform.macos.ui;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;

import com.github.weisj.darklaf.platform.decorations.CustomTitlePane;
import com.github.weisj.darklaf.platform.macos.JNIDecorationsMacOS;
import com.github.weisj.darklaf.util.PropertyKey;

public class MacOSTitlePane extends CustomTitlePane {

    private final JRootPane rootPane;
    private final Window window;
    private WindowListener windowListener;
    private Color inactiveBackground;
    private Color activeBackground;
    private Color inactiveForeground;
    private Color activeForeground;
    private Color border;
    private DecorationInformation decorationInformation;
    private JLabel titleLabel;
    private PropertyChangeHandler propertyChangeListener;
    private boolean hideTitleBar = false;

    public MacOSTitlePane(final JRootPane rootPane, final Window window) {
        this.rootPane = rootPane;
        this.window = window;
        determineColors();
    }

    protected void determineColors() {
        switch (getDecorationStyle()) {
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
            default: // JRootPane.Frame
                activeBackground = UIManager.getColor("MacOS.TitlePane.background");
                activeForeground = UIManager.getColor("MacOS.TitlePane.foreground");
                break;
        }
        inactiveBackground = UIManager.getColor("MacOS.TitlePane.inactiveBackground");
        inactiveForeground = UIManager.getColor("MacOS.TitlePane.inactiveForeground");
        border = UIManager.getColor("MacOS.TitlePane.borderColor");

        // Ensure they don't get overwritten by ui updated.
        activeForeground = new Color(activeForeground.getRGB());
        inactiveForeground = new Color(inactiveForeground.getRGB());
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

        if (!hideTitleBar()) {
            g.setColor(border);
            g.fillRect(0, height - 1, width, 1);
        }
    }

    public Window getWindow() {
        return window;
    }

    public void install() {
        determineColors();
        JRootPane rootPane = getRootPane();
        decorationInformation = MacOSDecorationsUtil.installDecorations(rootPane);
        installListeners();
        if (!decorationInformation.titleVisible) {
            titleLabel = new JLabel();
            titleLabel.setFont(titleLabel.getFont().deriveFont(decorationInformation.titleFontSize));
            titleLabel.setForeground(activeForeground);
            titleLabel.setText(getTitle());
            add(titleLabel);
        }
    }

    private String getTitle() {
        if (window instanceof Frame) {
            return ((Frame) window).getTitle();
        } else if (window instanceof Dialog) {
            return ((Dialog) window).getTitle();
        }
        return "";
    }

    @Override
    public void uninstall(final boolean removeDecorations) {
        if (titleLabel != null) {
            remove(titleLabel);
            titleLabel = null;
        }
        uninstallListeners();
        MacOSDecorationsUtil.uninstallDecorations(decorationInformation);
        decorationInformation = null;
    }

    private void installListeners() {
        if (window != null && useCustomTitle()) {
            windowListener = new WindowHandler();
            window.addWindowListener(windowListener);
            propertyChangeListener = new PropertyChangeHandler();
            window.addPropertyChangeListener(propertyChangeListener);
        }
    }

    private void uninstallListeners() {
        if (window != null) {
            window.removeWindowListener(windowListener);
            windowListener = null;
            window.removePropertyChangeListener(propertyChangeListener);
            propertyChangeListener = null;
        }
    }

    @Override
    public Dimension getMinimumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getMaximumSize() {
        return getPreferredSize();
    }

    @Override
    public Dimension getPreferredSize() {
        if (decorationInformation == null) return new Dimension(0, 0);
        int height = decorationInformation.titleBarHeight;
        if (hideTitleBar()) {
            height = 0;
        } else if (useCustomTitle()) {
            height = Math.max(height, titleLabel.getPreferredSize().height);
        }
        return new Dimension(0, height);
    }

    private boolean hideTitleBar() {
        return (decorationInformation.windowHandle == 0)
                || JNIDecorationsMacOS.isFullscreen(decorationInformation.windowHandle)
                || getDecorationStyle() == JRootPane.NONE;
    }

    private boolean useCustomTitle() {
        return titleLabel != null && decorationInformation != null && !decorationInformation.titleVisible;
    }

    @Override
    public void doLayout() {
        boolean hide = hideTitleBar();
        if (useCustomTitle() && !hide) {
            int width = getWidth();
            int height = getHeight();
            int labelWidth = titleLabel.getPreferredSize().width;
            int x = (width - labelWidth) / 2;
            titleLabel.setBounds(x, 0, labelWidth, height);
        }
        if (hide != hideTitleBar) {
            hideTitleBar = hide;
            getParent().doLayout();
        }
    }

    protected class WindowHandler extends WindowAdapter {

        public void windowActivated(final WindowEvent ev) {
            titleLabel.setForeground(activeForeground);
        }

        public void windowDeactivated(final WindowEvent ev) {
            titleLabel.setForeground(inactiveForeground);
        }
    }

    protected class PropertyChangeHandler implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent pce) {
            String name = pce.getPropertyName();
            if (PropertyKey.TITLE.equals(name)) {
                titleLabel.setText(pce.getNewValue() == null ? "" : pce.getNewValue().toString());
                repaint();
            }
        }
    }
}
