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
package com.github.weisj.darklaf.ui.internalframe;

import com.github.weisj.darklaf.components.uiresource.JButtonUIResource;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicDesktopIconUI;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkDesktopIconUI extends BasicDesktopIconUI {

    protected JButton button;
    protected JLabel label;
    protected TitleListener titleListener;
    protected int width;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkDesktopIconUI();
    }

    protected void installComponents() {
        frame = desktopIcon.getInternalFrame();
        Icon icon = frame.getFrameIcon();
        String title = frame.getTitle();

        button = new JButtonUIResource(title, icon);
        button.setOpaque(false);
        button.putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_FULL_SHADOW);
        button.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, UIManager.getColor("DesktopIcon.hoverColor"));
        button.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, UIManager.getColor("DesktopIcon.hoverColor"));
        button.setRolloverEnabled(true);
        button.addActionListener(e -> deiconize());
        button.setFont(desktopIcon.getFont());
        button.setBackground(desktopIcon.getBackground());
        button.setForeground(desktopIcon.getForeground());

        Icon drag = UIManager.getIcon("DesktopIcon.drag.icon");
        label = new JLabel(drag);

        label.setBorder(new MatteBorder(0, 2, 0, 1, desktopIcon.getBackground()));
        desktopIcon.setLayout(new BorderLayout(2, 0));
        desktopIcon.add(button, BorderLayout.CENTER);
        desktopIcon.add(label, BorderLayout.WEST);
    }

    protected void uninstallComponents() {
        desktopIcon.setLayout(null);
        desktopIcon.remove(label);
        desktopIcon.remove(button);
        button = null;
        frame = null;
    }

    protected void installListeners() {
        super.installListeners();
        titleListener = new TitleListener();
        desktopIcon.getInternalFrame().addPropertyChangeListener(titleListener);
    }

    protected void uninstallListeners() {
        desktopIcon.getInternalFrame().removePropertyChangeListener(titleListener);
        titleListener = null;
        super.uninstallListeners();
    }

    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installColorsAndFont(desktopIcon, "DesktopIcon.background",
                                         "DesktopIcon.foreground", "DesktopIcon.font");
        width = UIManager.getInt("DesktopIcon.width");
    }

    public Dimension getPreferredSize(final JComponent c) {
        return getMinimumSize(c);
    }

    public Dimension getMinimumSize(final JComponent c) {
        return new Dimension(width, DarkInternalFrameTitlePane.BAR_HEIGHT);
    }

    public Dimension getMaximumSize(final JComponent c) {
        return getMinimumSize(c);
    }

    class TitleListener implements PropertyChangeListener {
        public void propertyChange(final PropertyChangeEvent e) {
            if (e.getPropertyName().equals("title")) {
                button.setText((String) e.getNewValue());
            }

            if (e.getPropertyName().equals("frameIcon")) {
                button.setIcon((Icon) e.getNewValue());
            }
        }
    }
}
