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
package com.github.weisj.darklaf.ui.internalframe;

import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicDesktopIconUI;

import com.github.weisj.darklaf.components.uiresource.JButtonUIResource;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;

/** @author Jannis Weis */
public class DarkDesktopIconUI extends BasicDesktopIconUI implements PropertyChangeListener {

    protected JButton button;
    protected JLabel gripLabel;
    protected int width;
    protected int height;
    protected int labelPad;

    public static ComponentUI createUI(final JComponent c) {
        return new DarkDesktopIconUI();
    }

    @Override
    protected void installComponents() {
        frame = desktopIcon.getInternalFrame();
        Icon icon = frame.getFrameIcon();
        String title = frame.getTitle();

        button = new JButtonUIResource(title, icon);
        button.setOpaque(false);
        button.setFocusable(false);
        button.putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS_RECTANGULAR);
        button.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, UIManager.getColor("DesktopIcon.hoverColor"));
        button.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, UIManager.getColor("DesktopIcon.hoverColor"));
        button.setRolloverEnabled(true);
        button.addActionListener(e -> deiconize());
        button.setFont(desktopIcon.getFont());
        button.setBackground(desktopIcon.getBackground());
        button.setForeground(desktopIcon.getForeground());

        labelPad = UIManager.getInt("DesktopIcon.labelPad");

        Icon drag = UIManager.getIcon("DesktopIcon.drag.icon");
        gripLabel = new JLabel(drag);

        gripLabel.setBorder(new MatteBorder(0, 2, 0, 1, desktopIcon.getBackground()));
        desktopIcon.setLayout(new BorderLayout(labelPad, 0));
        desktopIcon.add(button, BorderLayout.CENTER);
        desktopIcon.add(gripLabel, BorderLayout.EAST);
    }

    @Override
    protected void uninstallComponents() {
        desktopIcon.setLayout(null);
        desktopIcon.remove(gripLabel);
        desktopIcon.remove(button);
        button = null;
        frame = null;
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        desktopIcon.getInternalFrame().addPropertyChangeListener(this);
    }

    @Override
    protected void uninstallListeners() {
        desktopIcon.getInternalFrame().removePropertyChangeListener(this);
        super.uninstallListeners();
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installColorsAndFont(desktopIcon, "DesktopIcon.background", "DesktopIcon.foreground",
                "DesktopIcon.font");
    }

    @Override
    public Dimension getMinimumSize(final JComponent c) {
        return getPreferredSize(c);
    }

    @Override
    public Dimension getMaximumSize(final JComponent c) {
        return getPreferredSize(c);
    }

    @Override
    public void propertyChange(final PropertyChangeEvent e) {
        if (e.getPropertyName().equals("title")) {
            button.setText((String) e.getNewValue());
        }

        if (e.getPropertyName().equals("frameIcon")) {
            button.setIcon((Icon) e.getNewValue());
        }
    }
}
