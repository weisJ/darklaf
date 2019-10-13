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
package com.weis.darklaf.ui.menu;

import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.LazyActionMap;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.MenuItemCheckIconFactory;
import sun.swing.MenuItemLayoutHelper;
import sun.swing.SwingUtilities2;
import sun.swing.UIAction;

import javax.swing.*;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentInputMapUIResource;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.plaf.basic.BasicMenuItemUI;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Objects;

/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkMenuItemUIBase extends BasicMenuItemUI {

    protected Handler handler;

    @NotNull
    @Contract("_ -> new")
    public static ComponentUI createUI(final JComponent c) {
        return new DarkMenuItemUIBase();
    }

    protected static void loadActionMap(@NotNull final LazyActionMap map) {
        map.put(new Actions(Actions.CLICK));
    }

    @Override
    public void installUI(final JComponent c) {
        super.installUI(c);
        acceleratorFont = UIManager.getFont("MenuItem.font");
        acceleratorForeground = UIManager.getColor("MenuItem.foreground");
        acceleratorSelectionForeground = UIManager.getColor("MenuItem.selectionForeground");
        arrowIcon = null;
    }

    protected void paintMenuItem(@NotNull final Graphics g, final JComponent c,
                                 final Icon checkIcon, final Icon arrowIcon,
                                 final Color background, final Color foreground,
                                 final int defaultTextIconGap) {
        // Save original graphics font and color
        Font holdf = g.getFont();
        Color holdc = g.getColor();

        JMenuItem mi = (JMenuItem) c;
        g.setFont(mi.getFont());

        Rectangle viewRect = new Rectangle(0, 0, mi.getWidth(), mi.getHeight());
        DarkUIUtil.applyInsets(viewRect, mi.getInsets());

        MenuItemLayoutHelper lh = new MenuItemLayoutHelper(mi, checkIcon,
                                                           arrowIcon, viewRect, defaultTextIconGap, acceleratorDelimiter,
                                                           mi.getComponentOrientation().isLeftToRight(), mi.getFont(),
                                                           acceleratorFont, MenuItemLayoutHelper.useCheckAndArrow(menuItem),
                                                           getPropertyPrefix());
        MenuItemLayoutHelper.LayoutResult lr = lh.layoutMenuItem();

        paintBackground(g, mi, background);
        paintCheckIcon(g, lh, lr, holdc, foreground);
        paintIcon(g, lh, lr, holdc);
        g.setColor(foreground);
        paintText(g, lh, lr);
        paintAccText(g, lh, lr);
        paintArrowIcon(g, lh, lr, foreground);

        // Restore original graphics font and color
        g.setColor(holdc);
        g.setFont(holdf);
    }

    @Override
    protected void paintBackground(@NotNull final Graphics g, @NotNull final JMenuItem menuItem, final Color bgColor) {
        ButtonModel model = menuItem.getModel();
        Color oldColor = g.getColor();
        int menuWidth = menuItem.getWidth();
        int menuHeight = menuItem.getHeight() + 1;

        boolean parentOpaque = menuItem.getParent().isOpaque();
        if (menuItem.isOpaque() && parentOpaque) {
            if (model.isArmed() || (menuItem instanceof JMenu && model.isSelected())) {
                g.setColor(bgColor);
                g.fillRect(0, 0, menuWidth, menuHeight);
            } else {
                g.setColor(menuItem.getBackground());
                g.fillRect(0, 0, menuWidth, menuHeight);
            }
            g.setColor(oldColor);
        } else if (model.isArmed() || (menuItem instanceof JMenu &&
                model.isSelected())) {
            g.setColor(bgColor);
            g.fillRect(0, 0, menuWidth, menuHeight);
            g.setColor(oldColor);
        }
    }

    protected void paintCheckIcon(final Graphics g, @NotNull final MenuItemLayoutHelper lh,
                                  final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color holdc, final Color foreground) {
        if (lh.getCheckIcon() != null) {
            ButtonModel model = lh.getMenuItem().getModel();
            if (model.isArmed() || (lh.getMenuItem() instanceof JMenu
                    && model.isSelected())) {
                g.setColor(foreground);
            } else {
                g.setColor(holdc);
            }
            if (lh.useCheckAndArrow()) {
                lh.getCheckIcon().paintIcon(lh.getMenuItem(), g,
                                            lr.getCheckRect().x, lr.getCheckRect().y);
            }
            g.setColor(holdc);
        }
    }

    protected void paintIcon(final Graphics g, @NotNull final MenuItemLayoutHelper lh,
                             final MenuItemLayoutHelper.LayoutResult lr, final Color holdc) {
        if (lh.getIcon() != null) {
            Icon icon;
            ButtonModel model = lh.getMenuItem().getModel();
            if (!model.isEnabled()) {
                icon = lh.getMenuItem().getDisabledIcon();
            } else if (model.isPressed() && model.isArmed()) {
                icon = lh.getMenuItem().getPressedIcon();
                if (icon == null) {
                    // Use default icon
                    icon = lh.getMenuItem().getIcon();
                }
            } else {
                icon = lh.getMenuItem().getIcon();
            }

            if (icon != null) {
                icon.paintIcon(lh.getMenuItem(), g, lr.getIconRect().x, lr.getIconRect().y);
                g.setColor(holdc);
            }
        }
    }

    protected void paintText(final Graphics g, @NotNull final MenuItemLayoutHelper lh,
                             final MenuItemLayoutHelper.LayoutResult lr) {
        if (!lh.getText().isBlank()) {
            if (lh.getHtmlView() != null) {
                // Text is HTML
                lh.getHtmlView().paint(g, lr.getTextRect());
            } else {
                // Text isn't HTML
                paintText(g, lh.getMenuItem(), lr.getTextRect(), lh.getText());
            }
        }
    }

    protected void paintAccText(final Graphics g, final MenuItemLayoutHelper lh,
                                final MenuItemLayoutHelper.LayoutResult lr) {
        rightAlignAccText(lh, lr);
        if (!lh.getAccText().isBlank()) {
            ButtonModel model = lh.getMenuItem().getModel();
            g.setFont(lh.getAccFontMetrics().getFont());
            if (!model.isEnabled()) {
                // *** paint the accText disabled
                if (disabledForeground != null) {
                    g.setColor(disabledForeground);
                    SwingUtilities2.drawString(lh.getMenuItem(), g,
                                               lh.getAccText(), lr.getAccRect().x,
                                               lr.getAccRect().y + lh.getAccFontMetrics().getAscent());
                } else {
                    g.setColor(lh.getMenuItem().getBackground().brighter());
                    SwingUtilities2.drawString(lh.getMenuItem(), g,
                                               lh.getAccText(), lr.getAccRect().x,
                                               lr.getAccRect().y + lh.getAccFontMetrics().getAscent());
                    g.setColor(lh.getMenuItem().getBackground().darker());
                    SwingUtilities2.drawString(lh.getMenuItem(), g,
                                               lh.getAccText(), lr.getAccRect().x - 1,
                                               lr.getAccRect().y + lh.getFontMetrics().getAscent() - 1);
                }
            } else {
                // *** paint the accText normally
                if (model.isArmed()
                        || (lh.getMenuItem() instanceof JMenu
                        && model.isSelected())) {
                    g.setColor(acceleratorSelectionForeground);
                } else {
                    g.setColor(acceleratorForeground);
                }
                SwingUtilities2.drawString(lh.getMenuItem(), g, lh.getAccText(),
                                           lr.getAccRect().x, lr.getAccRect().y +
                                                   lh.getAccFontMetrics().getAscent());
            }
        }
    }

    protected void paintArrowIcon(final Graphics g, @NotNull final MenuItemLayoutHelper lh,
                                  final MenuItemLayoutHelper.LayoutResult lr,
                                  final Color foreground) {
        if (lh.getArrowIcon() != null) {
            ButtonModel model = lh.getMenuItem().getModel();
            if (model.isArmed() || (lh.getMenuItem() instanceof JMenu
                    && model.isSelected())) {
                g.setColor(foreground);
            }
            if (lh.useCheckAndArrow()) {
                lh.getArrowIcon().paintIcon(lh.getMenuItem(), g,
                                            lr.getArrowRect().x, lr.getArrowRect().y);
            }
        }
    }

    private static void rightAlignAccText(@NotNull final MenuItemLayoutHelper lh,
                                          @NotNull final MenuItemLayoutHelper.LayoutResult lr) {
        var accRect = lr.getAccRect();
        ButtonModel model = lh.getMenuItem().getModel();
        if (model.isEnabled()) {
            accRect.x = lh.getViewRect().x + lh.getViewRect().width
                    - lh.getMenuItem().getIconTextGap() - lr.getAccRect().width;
        }
    }

    /*
     * Code from BasicMenuItemUI.
     */

    protected DarkMenuItemUIBase.Handler getHandler() {
        if (handler == null) {
            handler = new DarkMenuItemUIBase.Handler();
        }
        return handler;
    }

    protected void updateAcceleratorBinding() {
        KeyStroke accelerator = menuItem.getAccelerator();
        InputMap windowInputMap = SwingUtilities.getUIInputMap(menuItem, JComponent.WHEN_IN_FOCUSED_WINDOW);

        if (windowInputMap != null) {
            windowInputMap.clear();
        }
        if (accelerator != null) {
            if (windowInputMap == null) {
                windowInputMap = createInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
                SwingUtilities.replaceUIInputMap(menuItem, JComponent.WHEN_IN_FOCUSED_WINDOW, windowInputMap);
            }
            windowInputMap.put(accelerator, "doClick");

            int modifiers = accelerator.getModifiers();
            if (((modifiers & InputEvent.ALT_DOWN_MASK) != 0) &&
                    ((modifiers & InputEvent.ALT_GRAPH_DOWN_MASK) != 0)) {
                //When both ALT and ALT_GRAPH are set, add the ALT only
                // modifier keystroke which is used for left ALT key.
                // Unsetting the ALT_GRAPH will do that as ALT is already set
                modifiers &= ~InputEvent.ALT_GRAPH_DOWN_MASK;
                modifiers &= ~InputEvent.ALT_GRAPH_MASK;
                KeyStroke keyStroke = KeyStroke.getKeyStroke(accelerator.getKeyCode(),
                                                             modifiers, accelerator.isOnKeyRelease());
                windowInputMap.put(keyStroke, "doClick");
            } else if (((modifiers & InputEvent.ALT_DOWN_MASK) != 0) && (
                    (modifiers & InputEvent.ALT_GRAPH_DOWN_MASK) == 0)) {
                //When only ALT modifier is set, add the ALT + ALT_GRAPH
                // modifier keystroke which is used for right ALT key
                modifiers |= InputEvent.ALT_GRAPH_DOWN_MASK;
                KeyStroke keyStroke = KeyStroke.getKeyStroke(accelerator.getKeyCode(),
                                                             modifiers, accelerator.isOnKeyRelease());
                windowInputMap.put(keyStroke, "doClick");
            } else if ((modifiers & InputEvent.ALT_GRAPH_DOWN_MASK) != 0) {
                //When only ALT_GRAPH is set, remove the ALT_GRAPH only
                // modifier and add the ALT and ALT+ALT_GRAPH modifiers
                // keystroke which are used for left ALT key and right ALT
                // respectively
                modifiers &= ~InputEvent.ALT_GRAPH_DOWN_MASK;
                modifiers &= ~InputEvent.ALT_GRAPH_MASK;

                modifiers |= InputEvent.ALT_DOWN_MASK;
                KeyStroke keyStroke = KeyStroke.getKeyStroke(accelerator.getKeyCode(),
                                                             modifiers, accelerator.isOnKeyRelease());
                windowInputMap.put(keyStroke, "doClick");

                //Add ALT+ALT_GRAPH modifier which is used for right ALT key
                modifiers |= InputEvent.ALT_GRAPH_DOWN_MASK;
                keyStroke = KeyStroke.getKeyStroke(accelerator.getKeyCode(),
                                                   modifiers, accelerator.isOnKeyRelease());
                windowInputMap.put(keyStroke, "doClick");
            }
        }
    }

    protected InputMap createInputMap(final int condition) {
        if (condition == JComponent.WHEN_IN_FOCUSED_WINDOW) {
            return new ComponentInputMapUIResource(menuItem);
        }
        return null;
    }

    protected void updateCheckIcon() {
        String prefix = getPropertyPrefix();

        if (checkIcon == null || checkIcon instanceof UIResource) {
            checkIcon = UIManager.getIcon(prefix + ".checkIcon");
            //In case of column layout, .checkIconFactory is defined for this UI,
            //the icon is compatible with it and useCheckAndArrow() is true,
            //then the icon is handled by the checkIcon.
            boolean isColumnLayout = MenuItemLayoutHelper.isColumnLayout(
                    menuItem.getComponentOrientation().isLeftToRight(), menuItem);
            if (isColumnLayout) {
                MenuItemCheckIconFactory iconFactory =
                        (MenuItemCheckIconFactory) UIManager.get(prefix + ".checkIconFactory");
                if (iconFactory != null
                        && MenuItemLayoutHelper.useCheckAndArrow(menuItem)
                        && iconFactory.isCompatible(checkIcon, prefix)) {
                    checkIcon = iconFactory.getIcon(menuItem);
                }
            }
        }
    }

    private static class Actions extends UIAction {
        private static final String CLICK = "doClick";

        Actions(final String key) {
            super(key);
        }

        public void actionPerformed(final ActionEvent e) {
            JMenuItem mi = (JMenuItem) e.getSource();
            MenuSelectionManager.defaultManager().clearSelectedPath();
            mi.doClick();
        }
    }

    protected class Handler implements MenuDragMouseListener, MouseInputListener, PropertyChangeListener {
        //
        // MouseInputListener
        //
        public void mouseClicked(final MouseEvent e) {
        }

        public void mousePressed(final MouseEvent e) {
        }

        public void mouseReleased(final MouseEvent e) {
            if (!menuItem.isEnabled()) {
                return;
            }
            MenuSelectionManager manager =
                    MenuSelectionManager.defaultManager();
            Point p = e.getPoint();
            if (p.x >= 0 && p.x < menuItem.getWidth() &&
                    p.y >= 0 && p.y < menuItem.getHeight()) {
                doClick(manager);
            } else {
                manager.processMouseEvent(e);
            }
        }

        @SuppressWarnings("deprecation")
        public void mouseEntered(@NotNull final MouseEvent e) {
            MenuSelectionManager manager = MenuSelectionManager.defaultManager();
            int modifiers = e.getModifiers();
            // 4188027: drag enter/exit added in JDK 1.1.7A, JDK1.2
            if ((modifiers & (InputEvent.BUTTON1_MASK |
                    InputEvent.BUTTON2_MASK | InputEvent.BUTTON3_MASK)) != 0) {
                MenuSelectionManager.defaultManager().processMouseEvent(e);
            } else {
                manager.setSelectedPath(getPath());
            }
        }

        @SuppressWarnings("deprecation")
        public void mouseExited(@NotNull final MouseEvent e) {
            MenuSelectionManager manager = MenuSelectionManager.defaultManager();

            int modifiers = e.getModifiers();
            // 4188027: drag enter/exit added in JDK 1.1.7A, JDK1.2
            if ((modifiers & (InputEvent.BUTTON1_MASK |
                    InputEvent.BUTTON2_MASK | InputEvent.BUTTON3_MASK)) != 0) {
                MenuSelectionManager.defaultManager().processMouseEvent(e);
            } else {

                MenuElement[] path = manager.getSelectedPath();
                if (path.length > 1 && path[path.length - 1] == menuItem) {
                    MenuElement[] newPath = new MenuElement[path.length - 1];
                    int i, c;
                    for (i = 0, c = path.length - 1; i < c; i++) { newPath[i] = path[i]; }
                    manager.setSelectedPath(newPath);
                }
            }
        }

        public void mouseDragged(final MouseEvent e) {
            MenuSelectionManager.defaultManager().processMouseEvent(e);
        }

        public void mouseMoved(final MouseEvent e) {
        }

        //
        // MenuDragListener
        //
        public void menuDragMouseEntered(@NotNull final MenuDragMouseEvent e) {
            MenuSelectionManager manager = e.getMenuSelectionManager();
            MenuElement[] path = e.getPath();
            manager.setSelectedPath(path);
        }

        public void menuDragMouseExited(final MenuDragMouseEvent e) {
        }

        public void menuDragMouseDragged(@NotNull final MenuDragMouseEvent e) {
            MenuSelectionManager manager = e.getMenuSelectionManager();
            MenuElement[] path = e.getPath();
            manager.setSelectedPath(path);
        }

        public void menuDragMouseReleased(final MenuDragMouseEvent e) {
            if (!menuItem.isEnabled()) {
                return;
            }
            MenuSelectionManager manager = e.getMenuSelectionManager();
            e.getPath();
            Point p = e.getPoint();
            if (p.x >= 0 && p.x < menuItem.getWidth() &&
                    p.y >= 0 && p.y < menuItem.getHeight()) {
                doClick(manager);
            } else {
                manager.clearSelectedPath();
            }
        }


        //
        // PropertyChangeListener
        //
        public void propertyChange(final PropertyChangeEvent e) {
            String name = e.getPropertyName();

            if (Objects.equals(name, "labelFor") || Objects.equals(name, "displayedMnemonic") ||
                    Objects.equals(name, "accelerator")) {
                updateAcceleratorBinding();
            } else if (Objects.equals(name, "text") || "font".equals(name) || "foreground".equals(name)
                    || SwingUtilities2.isScaleChanged(e)) {
                // remove the old html view client property if one
                // existed, and install a new one if the text installed
                // into the JLabel is html source.
                JMenuItem lbl = ((JMenuItem) e.getSource());
                String text = lbl.getText();
                BasicHTML.updateRenderer(lbl, text);
            } else if (Objects.equals(name, "iconTextGap")) {
                defaultTextIconGap = ((Number) e.getNewValue()).intValue();
            } else if (Objects.equals(name, "horizontalTextPosition")) {
                updateCheckIcon();
            }
        }
    }
}
