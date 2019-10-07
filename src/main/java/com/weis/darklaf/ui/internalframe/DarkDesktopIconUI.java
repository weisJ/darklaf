package com.weis.darklaf.ui.internalframe;

import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.border.MatteBorder;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.basic.BasicDesktopIconUI;
import java.awt.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class DarkDesktopIconUI extends BasicDesktopIconUI {

    public static ComponentUI createUI(final JComponent c) {
        return new DarkDesktopIconUI();
    }

    protected JButton button;
    protected JLabel label;
    protected TitleListener titleListener;
    protected int width;

    protected void installDefaults() {
        super.installDefaults();
        LookAndFeel.installColorsAndFont(desktopIcon, "DesktopIcon.background",
                                         "DesktopIcon.foreground", "DesktopIcon.font");
        width = UIManager.getInt("DesktopIcon.width");
    }

    protected void installComponents() {
        frame = desktopIcon.getInternalFrame();
        Icon icon = frame.getFrameIcon();
        String title = frame.getTitle();

        button = new JButton(title, icon);
        button.setOpaque(false);
        button.putClientProperty("JButton.variant", "fullShadow");
        button.putClientProperty("JButton.shadow.hover", UIManager.getColor("DesktopIcon.hoverColor"));
        button.putClientProperty("JButton.shadow.click", UIManager.getColor("DesktopIcon.clickColor"));
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
        public void propertyChange(@NotNull final PropertyChangeEvent e) {
            if (e.getPropertyName().equals("title")) {
                button.setText((String) e.getNewValue());
            }

            if (e.getPropertyName().equals("frameIcon")) {
                button.setIcon((Icon) e.getNewValue());
            }
        }
    }
}
