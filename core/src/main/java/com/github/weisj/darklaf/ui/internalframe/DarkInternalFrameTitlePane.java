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
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.icons.ToggleIcon;
import com.github.weisj.darklaf.ui.button.DarkButtonUI;
import sun.swing.SwingUtilities2;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.plaf.basic.BasicInternalFrameTitlePane;
import java.awt.*;
import java.beans.PropertyChangeListener;

/**
 * @author Jannis Weis
 */
public class DarkInternalFrameTitlePane extends BasicInternalFrameTitlePane {
    protected static final int PAD = 5;
    protected static final int BAR_HEIGHT = 28;
    protected static final int BUTTON_WIDTH = 46;
    protected static final int IMAGE_HEIGHT = 16;
    protected static final int IMAGE_WIDTH = 16;

    private JMenuBar menu;
    private JLabel label;
    private Color border;
    private Color buttonColor;
    private Color selectedButtonColor;
    private Color buttonColorClick;
    private Color buttonColorHover;
    private Color selectedButtonColorHover;
    private Color selectedButtonColorClick;

    private ToggleIcon minimizeIcon;
    private ToggleIcon maximizeIcon;
    private ToggleIcon iconifyIcon;

    private PropertyChangeListener propertyChangeListener2;


    public DarkInternalFrameTitlePane(final JInternalFrame f) {
        super(f);
    }

    @Override
    protected void addSubComponents() {
        super.addSubComponents();
        label = new JLabel();
        label.setOpaque(false);
        if (menu != null) {
            menu = frame.getJMenuBar();
            frame.getRootPane().setJMenuBar(null);
        }
        if (menu != null) {
            add(menu);
        }
        add(label);
    }

    @Override
    protected void installListeners() {
        super.installListeners();
        propertyChangeListener2 = e -> {
            Object menuBar = e.getNewValue();
            if (menuBar instanceof JMenuBar) {
                frame.getRootPane().setJMenuBar(null);
                menu = (JMenuBar) menuBar;
                add(menu);
            }
        };
        frame.addPropertyChangeListener(JInternalFrame.MENU_BAR_PROPERTY, propertyChangeListener2);
    }

    @Override
    protected void uninstallListeners() {
        super.uninstallListeners();
        frame.removePropertyChangeListener(propertyChangeListener2);
    }

    @Override
    protected void installDefaults() {
        super.installDefaults();

        closeIcon = UIManager.getIcon("InternalFrameTitlePane.close.icon");
        minimizeIcon = new ToggleIcon(UIManager.getIcon("InternalFrameTitlePane.minimize.icon"),
                                      EmptyIcon.create(16));
        maximizeIcon = new ToggleIcon(UIManager.getIcon("InternalFrameTitlePane.maximize.icon"),
                                      EmptyIcon.create(16));
        iconifyIcon = new ToggleIcon(UIManager.getIcon("InternalFrameTitlePane.iconify.icon"),
                                     EmptyIcon.create(16));
        minIcon = minimizeIcon;
        maxIcon = maximizeIcon;
        iconIcon = iconifyIcon;

        selectedTitleColor = UIManager.getColor("InternalFrameTitlePane.selectedBackgroundColor");
        selectedTextColor = UIManager.getColor("InternalFrameTitlePane.selectedTextForeground");
        notSelectedTitleColor = UIManager.getColor("InternalFrameTitlePane.backgroundColor");
        notSelectedTextColor = UIManager.getColor("InternalFrameTitlePane.textForeground");

        selectedButtonColor = UIManager.getColor("InternalFrameTitlePane.selectedButtonColor");
        selectedButtonColorHover = UIManager.getColor("InternalFrameTitlePane.selectedButtonHoverColor");
        selectedButtonColorClick = UIManager.getColor("InternalFrameTitlePane.selectedButtonClickColor");

        buttonColor = UIManager.getColor("InternalFrameTitlePane.buttonColor");
        buttonColorHover = UIManager.getColor("InternalFrameTitlePane.buttonHoverColor");
        buttonColorClick = UIManager.getColor("InternalFrameTitlePane.buttonClickColor");

        border = UIManager.getColor("InternalFrameTitlePane.borderColor");
    }

    @Override
    protected void createButtons() {
        iconButton = createButton(UIManager.getString("InternalFrameTitlePane.iconifyButtonAccessibleName"));
        iconButton.addActionListener(iconifyAction);
        maxButton = createButton(UIManager.getString("InternalFrameTitlePane.maximizeButtonAccessibleName"));
        maxButton.addActionListener(e -> {
            if (maximizeAction.isEnabled()) {
                maximizeAction.actionPerformed(e);
            } else {
                restoreAction.actionPerformed(e);
            }
        });
        closeButton = createButton(UIManager.getString("InternalFrameTitlePane.closeButtonAccessibleName"));
        closeButton.addActionListener(closeAction);
        setButtonIcons();
    }

    protected void addSystemMenuItems(final JMenu systemMenu) {
        JMenuItem mi = systemMenu.add(restoreAction);
        mi.setMnemonic(getButtonMnemonic("restore"));
        mi.setIcon(minimizeIcon);
        mi.setDisabledIcon(minimizeIcon);

        mi = systemMenu.add(iconifyAction);
        mi.setMnemonic(getButtonMnemonic("minimize"));
        mi.setIcon(iconifyIcon);
        mi.setDisabledIcon(iconifyIcon);

        mi = systemMenu.add(maximizeAction);
        mi.setMnemonic(getButtonMnemonic("maximize"));
        mi.setIcon(maximizeIcon);
        mi.setDisabledIcon(maximizeIcon);

        systemMenu.add(new JSeparator());
        mi = systemMenu.add(closeAction);
        mi.setMnemonic(getButtonMnemonic("close"));
        mi.setIcon(closeIcon);
    }

    private static int getButtonMnemonic(final String button) {
        try {
            return Integer.parseInt(UIManager.getString("InternalFrameTitlePane." + button + "Button.mnemonic"));
        } catch (NumberFormatException e) {
            return -1;
        }
    }

    @Override
    public void paintComponent(final Graphics g) {
        g.setColor(getTitleBackground());
        g.fillRect(0, 0, getWidth(), getHeight());
        g.setColor(border);
        g.fillRect(0, getHeight() - 1, getWidth(), 1);
    }

    @Override
    protected void enableActions() {
        super.enableActions();
        iconifyIcon.setActive(iconifyAction.isEnabled());
        maximizeIcon.setActive(maximizeAction.isEnabled());
        minimizeIcon.setActive(restoreAction.isEnabled());
    }

    @Override
    protected LayoutManager createLayout() {
        return new DarkTitlePaneLayout();
    }

    protected Color getTitleBackground() {
        return frame.isSelected() ? selectedTitleColor : notSelectedTitleColor;
    }


    private static JButton createButton(final String accessibleName) {
        JButton button = new JButtonUIResource() {
            @Override
            public boolean isRolloverEnabled() {
                return true;
            }
        };
        button.setFocusable(false);
        button.setOpaque(true);
        button.putClientProperty(DarkButtonUI.KEY_VARIANT, DarkButtonUI.VARIANT_BORDERLESS_RECTANGULAR);
        button.putClientProperty("paintActive", Boolean.TRUE);
        button.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, accessibleName);
        button.setText(null);
        return button;
    }

    @Override
    protected void paintChildren(final Graphics g) {
        label.setText(frame.getTitle());
        Color colorClick = frame.isSelected() ? selectedButtonColorClick : buttonColorClick;
        Color colorHover = frame.isSelected() ? selectedButtonColorHover : buttonColorHover;
        Color bg = frame.isSelected() ? selectedButtonColor : buttonColor;
        iconButton.setBackground(bg);
        closeButton.setBackground(bg);
        maxButton.setBackground(bg);
        menu.setBackground(bg);
        menu.setOpaque(false);

        iconButton.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, colorHover);
        closeButton.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, colorHover);
        maxButton.putClientProperty(DarkButtonUI.KEY_HOVER_COLOR, colorHover);
        iconButton.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, colorClick);
        closeButton.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, colorClick);
        maxButton.putClientProperty(DarkButtonUI.KEY_CLICK_COLOR, colorClick);

        super.paintChildren(g);
    }

    @Override
    protected void paintBorder(final Graphics g) {
    }

    protected class DarkTitlePaneLayout implements LayoutManager {
        public void addLayoutComponent(final String name, final Component c) {
        }

        public void removeLayoutComponent(final Component c) {
        }

        public Dimension preferredLayoutSize(final Container c) {
            return minimumLayoutSize(c);
        }

        public Dimension minimumLayoutSize(final Container c) {
            int width = 2 * PAD;

            if (frame.isClosable()) {
                width += BUTTON_WIDTH;
            }
            if (frame.isMaximizable()) {
                width += BUTTON_WIDTH;
            }
            if (frame.isIconifiable()) {
                width += BUTTON_WIDTH;
            }

            FontMetrics fm = frame.getFontMetrics(getFont());
            String frameTitle = frame.getTitle();
            int title_w = frameTitle != null ? SwingUtilities2.stringWidth(frame, fm, frameTitle) : 0;
            int title_length = frameTitle != null ? frameTitle.length() : 0;

            // Leave room for three characters in the title.
            if (title_length > 3) {
                int subtitle_w = SwingUtilities2.stringWidth(frame, fm, frameTitle.substring(0, 3) + "...");
                width += Math.min(title_w, subtitle_w);
            } else {
                width += title_w;
            }

            if (menu != null) {
                width += menu.getPreferredSize().width + PAD;
            }

            // Calculate height.
            Icon icon = frame.getFrameIcon();
            int fontHeight = fm.getHeight();
            fontHeight += 2;
            int iconHeight = 0;
            if (icon != null) {
                // SystemMenuBar forces the icon to be 16x16 or less.
                iconHeight = Math.min(icon.getIconHeight(), IMAGE_HEIGHT);
            }
            iconHeight += 2;

            int height = Math.max(fontHeight, iconHeight);
            height = Math.max(height, BAR_HEIGHT);

            Dimension dim = new Dimension(width, height);

            // Take into account the border insets if any.
            if (getBorder() != null) {
                Insets insets = getBorder().getBorderInsets(c);
                dim.height += insets.top + insets.bottom;
                dim.width += insets.left + insets.right;
            }
            return dim;
        }

        public void layoutContainer(final Container c) {
            boolean leftToRight = frame.getComponentOrientation().isLeftToRight();

            int w = getWidth();
            int h = getHeight() - 1;
            int x1;
            int x2;

            Icon icon = frame.getFrameIcon();
            int iconHeight = 0;
            if (icon != null) {
                iconHeight = icon.getIconHeight();
            }
            x1 = (leftToRight) ? PAD : w - IMAGE_WIDTH - PAD;
            menuBar.setBounds(x1, (h - iconHeight) / 2, IMAGE_WIDTH, IMAGE_HEIGHT);
            x1 += (leftToRight) ? IMAGE_WIDTH : -IMAGE_WIDTH;
            x1 += (leftToRight) ? PAD : -PAD;
            if (menu != null) {
                int width = menu.getPreferredSize().width;
                if (leftToRight) {
                    menu.setBounds(x1, 0, width, h + 1);
                    x1 += width + PAD;
                } else {
                    x1 -= width;
                    menu.setBounds(x1, 0, width, h + 1);
                    x1 -= PAD;
                }
            }

            x2 = (leftToRight) ? w - BUTTON_WIDTH : 0;

            if (frame.isClosable()) {
                closeButton.setBounds(x2, 0, BUTTON_WIDTH, h);
                x2 += (leftToRight) ? -BUTTON_WIDTH : BUTTON_WIDTH;
            }

            if (frame.isMaximizable()) {
                maxButton.setBounds(x2, 0, BUTTON_WIDTH, h);
                x2 += (leftToRight) ? -BUTTON_WIDTH : BUTTON_WIDTH;
            }

            if (frame.isIconifiable()) {
                iconButton.setBounds(x2, 0, BUTTON_WIDTH, h);
            }
            x2 += (leftToRight) ? -PAD : PAD;

            if (leftToRight) {
                label.setBounds(x1, 0, x2 - x1, h);
            } else {
                label.setBounds(x2, 0, x1 - x2, h);
            }
        }
    }
}
