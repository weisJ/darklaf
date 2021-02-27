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
package com.github.weisj.darklaf.platform.windows.ui;

import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.plaf.UIResource;

import sun.awt.SunToolkit;

import com.github.weisj.darklaf.icons.ScaledIcon;
import com.github.weisj.darklaf.icons.ToggleIcon;
import com.github.weisj.darklaf.platform.decorations.CustomTitlePane;
import com.github.weisj.darklaf.platform.windows.JNIDecorationsWindows;
import com.github.weisj.darklaf.platform.windows.PointerUtil;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.PropertyUtil;
import com.github.weisj.darklaf.util.Scale;

/** @author Jannis Weis */
public class WindowsTitlePane extends CustomTitlePane {
    public static final String KEY_RESIZABLE = "resizable";
    public static final String KEY_STATE = "state";
    public static final String KEY_ICON_IMAGE = "iconImage";
    private static final int PAD = 5;
    private static final int BAR_HEIGHT = 28;
    private static final int BUTTON_WIDTH = 46;
    private static final int ICON_WIDTH = 32;
    private static final int ICON_SIZE = ICON_WIDTH - 3 * PAD;
    private final JRootPane rootPane;

    private boolean unifiedMenuBar;
    private boolean titleBarHidden;
    private ContainerListener rootPaneContainerListener;
    private ContainerListener layeredPaneContainerListener;

    private boolean oldResizable;
    private PropertyChangeListener windowPropertyChangeListener;
    private PropertyChangeListener rootPanePropertyChangeListener;
    private WindowListener windowListener;
    private ToggleIcon closeIcon;
    private ToggleIcon maximizeIcon;
    private ToggleIcon restoreIcon;
    private ToggleIcon minimizeIcon;
    private JButton windowIconButton;
    private JButton closeButton;
    private JButton maximizeToggleButton;
    private JButton minimizeButton;
    private Action closeAction;
    private Action restoreAction;
    private Action maximizeAction;
    private Action minimizeAction;
    private JLabel titleLabel;
    private final Window window;
    private long windowHandle;
    private JMenuBar menuBar;
    private int state;

    private Color inactiveBackground;
    private Color inactiveForeground;
    private Color inactiveHover;
    private Color inactiveClick;
    private Color activeBackground;
    private Color activeForeground;
    private Color border;
    private Border oldBorder;

    private int left;
    private int right;
    private int height;

    private GraphicsConfiguration gc;

    public WindowsTitlePane(final JRootPane root, final int decorationStyle, final Window window) {
        this.rootPane = root;
        this.window = window;
        this.decorationStyle = decorationStyle;

        state = -1;
        oldResizable = true;
        installSubcomponents();
        updateMenuBar(true);
        updateTitleBarVisibility();
        installDefaults();
        setLayout(createLayout());
    }

    private void updateMenuBar(final boolean install) {
        unifiedMenuBar = PropertyUtil.getBooleanProperty(rootPane, "JRootPane.unifiedMenuBar");
        if (unifiedMenuBar && install) {
            if (rootPaneContainerListener == null) {
                rootPaneContainerListener = createRootPaneContainerListener();
            }
            if (layeredPaneContainerListener == null) {
                layeredPaneContainerListener = createLayeredPaneContainerListener();
            }
            rootPane.addContainerListener(rootPaneContainerListener);
            rootPane.getLayeredPane().addContainerListener(layeredPaneContainerListener);
            addMenuBar(getRootPane().getJMenuBar());
        } else {
            rootPane.removeContainerListener(rootPaneContainerListener);
            rootPane.getLayeredPane().removeContainerListener(layeredPaneContainerListener);
            if (menuBar != null) {
                getRootPane().setJMenuBar(menuBar);
                menuBar.setPreferredSize(null);
                menuBar = null;
            }
        }
        rootPane.revalidate();
    }

    private void updateTitleBarVisibility() {
        titleBarHidden = PropertyUtil.getBooleanProperty(rootPane, "JRootPane.hideTitleBar");
        rootPane.doLayout();
        rootPane.repaint();
    }

    private ContainerListener createRootPaneContainerListener() {
        return new ContainerListener() {
            @Override
            public void componentAdded(final ContainerEvent e) {
                if (e.getChild() instanceof JLayeredPane) {
                    ((JLayeredPane) e.getChild()).addContainerListener(layeredPaneContainerListener);
                }
            }

            @Override
            public void componentRemoved(final ContainerEvent e) {
                if (e.getChild() instanceof JLayeredPane) {
                    ((JLayeredPane) e.getChild()).removeContainerListener(layeredPaneContainerListener);
                }
            }
        };
    }

    private ContainerListener createLayeredPaneContainerListener() {
        return new ContainerListener() {
            @Override
            public void componentAdded(final ContainerEvent e) {
                if (e.getChild() instanceof JMenuBar) {
                    addMenuBar(getRootPane().getJMenuBar());
                }
                if (getRootPane().getJMenuBar() == null && menuBar != null) {
                    removeMenuBar();
                }
            }

            @Override
            public void componentRemoved(final ContainerEvent e) {}
        };
    }

    private static JButton createButton(final Icon icon, final Action action) {
        return createButton(icon, action, false);
    }

    private static JButton createButton(final Icon icon, final Action action, final boolean close) {
        JButton button = new JButton(action) {
            @Override
            public boolean isRolloverEnabled() {
                return true;
            }
        };
        if (close) {
            button.putClientProperty("JButton.borderless.hover",
                    UIManager.getColor("Windows.TitlePane.close.rollOverColor"));
            button.putClientProperty("JButton.borderless.click",
                    UIManager.getColor("Windows.TitlePane.close.clickColor"));
        }
        button.putClientProperty("JButton.noBorderlessOverwrite", true);
        button.setFocusable(false);
        button.setOpaque(false);
        button.setRolloverEnabled(true);
        button.putClientProperty("JButton.variant", "borderlessRectangular");
        button.putClientProperty("paintActive", Boolean.TRUE);
        button.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, button.getText());
        button.putClientProperty("JToolTip.style", "plain");
        button.setToolTipText(button.getText());
        button.setIcon(icon);
        button.setText(null);
        return button;
    }

    @Override
    public void uninstall(final boolean removeDecorations) {
        uninstallListeners();
        uninstallDecorations(removeDecorations);
        removeAll();
        updateMenuBar(false);
    }

    private void uninstallListeners() {
        if (window != null) {
            window.removeWindowListener(windowListener);
            window.removePropertyChangeListener(windowPropertyChangeListener);
        }
        if (rootPane != null) {
            rootPane.removePropertyChangeListener(rootPanePropertyChangeListener);
        }
    }

    protected void uninstallDecorations(final boolean removeDecorations) {
        if (windowHandle != 0) {
            if (removeDecorations) {
                JNIDecorationsWindows.uninstallDecorations(windowHandle, decorationStyle != JRootPane.NONE);
            }
            windowHandle = 0;
        }
        rootPane.removeContainerListener(rootPaneContainerListener);
        rootPane.getLayeredPane().removeContainerListener(layeredPaneContainerListener);
        if (menuBar != null) {
            menuBar.setPreferredSize(null);
            rootPane.setJMenuBar(menuBar);
        }
    }

    public void install() {
        if (window != null) {
            if (!installDecorations()) return;
            if (window instanceof Frame) {
                titleLabel.setText(((Frame) window).getTitle());
                setState(((Frame) window).getExtendedState());
            } else {
                setState(0);
            }
            if (window instanceof Dialog) {
                titleLabel.setText(((Dialog) window).getTitle());
            }
            determineColors();
            setActive(window.isActive());
            installListeners();
            if (getComponentCount() == 0) {
                addComponents();
            }
            updateSystemIcon(getGraphicsConfiguration());
        }
    }

    @Override
    public void setDecorationsStyle(final int style) {
        super.setDecorationsStyle(style);
        if (style == JRootPane.NONE && windowHandle != 0) {
            uninstall(true);
        } else if (windowHandle == 0) {
            install();
        }
    }

    private boolean installDecorations() {
        if (getDecorationStyle() == JRootPane.NONE) return false;
        if (!window.isDisplayable()) return false;
        if (window instanceof Dialog || window instanceof Frame) {
            windowHandle = PointerUtil.getHWND(window);
            if (windowHandle != 0) {
                JNIDecorationsWindows.installDecorations(windowHandle);
                updateResizeBehaviour();
                Color color = rootPane.getBackground();
                JNIDecorationsWindows.setBackground(windowHandle, color.getRed(), color.getGreen(), color.getBlue());
            } else {
                uninstall(decorationStyle != JRootPane.NONE);
                return false;
            }
        }
        return true;
    }

    private void installListeners() {
        if (window != null) {
            windowListener = createWindowListener();
            window.addWindowListener(windowListener);
            windowPropertyChangeListener = createWindowPropertyChangeListener();
            rootPanePropertyChangeListener = createRootPanePropertyChangeListener();
            rootPane.addPropertyChangeListener(rootPanePropertyChangeListener);
            window.addPropertyChangeListener(windowPropertyChangeListener);
        }
    }

    private WindowListener createWindowListener() {
        return new WindowsTitlePane.WindowHandler();
    }

    private PropertyChangeListener createWindowPropertyChangeListener() {
        return new WindowPropertyChangeListener();
    }

    private PropertyChangeListener createRootPanePropertyChangeListener() {
        return new RootPanePropertyChangeListener();
    }

    private void installSubcomponents() {
        titleLabel = new JLabel();
        titleLabel.setHorizontalAlignment(JLabel.LEFT);

        createIcons();
        createActions();
        createButtons();

        addComponents();
    }

    private void addComponents() {
        add(windowIconButton);
        add(minimizeButton);
        add(maximizeToggleButton);
        add(closeButton);
        add(titleLabel);
        setComponentZOrder(closeButton, 0);
        setComponentZOrder(maximizeToggleButton, 1);
        setComponentZOrder(minimizeButton, 2);
        setComponentZOrder(windowIconButton, 3);
        setComponentZOrder(titleLabel, 4);
    }

    protected void addMenuBar(final JMenuBar menuBar) {
        if (menuBar != null && unifiedMenuBar) {
            this.menuBar = menuBar;
            // Otherwise, a white bar will appear where the menuBar used to be.
            menuBar.setPreferredSize(new Dimension(0, 0));
            menuBar.setOpaque(false);
            add(menuBar);
            setComponentZOrder(menuBar, 5);
        }
    }

    protected void removeMenuBar() {
        remove(menuBar);
        menuBar.setOpaque(true);
        menuBar = null;
    }

    private void determineColors() {
        switch (getDecorationStyle()) {
            case JRootPane.ERROR_DIALOG:
                activeBackground = UIManager.getColor("Windows.OptionPane.errorDialog.titlePane.background");
                activeForeground = UIManager.getColor("Windows.OptionPane.errorDialog.titlePane.foreground");
                break;
            case JRootPane.QUESTION_DIALOG:
            case JRootPane.COLOR_CHOOSER_DIALOG:
            case JRootPane.FILE_CHOOSER_DIALOG:
                activeBackground = UIManager.getColor("Windows.OptionPane.questionDialog.titlePane.background");
                activeForeground = UIManager.getColor("Windows.OptionPane.questionDialog.titlePane.foreground");
                break;
            case JRootPane.WARNING_DIALOG:
                activeBackground = UIManager.getColor("Windows.OptionPane.warningDialog.titlePane.background");
                activeForeground = UIManager.getColor("Windows.OptionPane.warningDialog.titlePane.foreground");
                break;
            default: // JRootPane.Frame
                activeBackground = UIManager.getColor("Windows.TitlePane.background");
                activeForeground = UIManager.getColor("Windows.TitlePane.foreground");
                break;
        }
        inactiveBackground = UIManager.getColor("Windows.TitlePane.inactiveBackground");
        inactiveForeground = UIManager.getColor("Windows.TitlePane.inactiveForeground");
        inactiveHover = UIManager.getColor("Windows.TitlePane.inactiveBackgroundHover");
        inactiveClick = UIManager.getColor("Windows.TitlePane.inactiveBackgroundClick");
        border = UIManager.getColor("Windows.TitlePane.borderColor");

        // Ensure they don't get overwritten by ui updated.
        activeForeground = new Color(activeForeground.getRGB());
        inactiveForeground = new Color(inactiveForeground.getRGB());
    }

    private void installDefaults() {
        setFont(UIManager.getFont("InternalFrame.titleFont", getLocale()));
    }

    protected JButton createWindowIcon() {
        windowIconButton = new JButton();
        windowIconButton.putClientProperty("JButton.noShadowOverwrite", true);
        windowIconButton.setComponentPopupMenu(createMenu());
        windowIconButton.addActionListener(e -> windowIconButton.getComponentPopupMenu().show(windowIconButton,
                windowIconButton.getWidth() / 2, windowIconButton.getHeight() / 2));
        windowIconButton.setFocusable(false);
        windowIconButton.setContentAreaFilled(false);
        windowIconButton.setBorderPainted(false);
        return windowIconButton;
    }

    private JPopupMenu createMenu() {
        JPopupMenu menu = new JPopupMenu();
        if (getDecorationStyle() == JRootPane.FRAME) {
            addMenuItems(menu);
        }
        return menu;
    }

    private void addMenuItems(final JPopupMenu menu) {
        menu.add(new JMenuItem(restoreAction) {
            {
                setDisabledIcon(restoreIcon);
            }
        });
        menu.add(new JMenuItem(minimizeAction) {
            {
                setDisabledIcon(minimizeIcon);
            }
        });
        if (Toolkit.getDefaultToolkit().isFrameStateSupported(Frame.MAXIMIZED_BOTH)) {
            menu.add(new JMenuItem(maximizeAction) {
                {
                    setDisabledIcon(maximizeIcon);
                }
            });
        }
        menu.addSeparator();
        menu.add(new JMenuItem(closeAction) {
            {
                setDisabledIcon(closeIcon);
            }
        });
    }

    private void close() {
        Window window = getWindow();
        if (window != null) {
            window.dispatchEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSING));
        }
    }

    private Window getWindow() {
        return window;
    }

    private void minimize() {
        JNIDecorationsWindows.minimize(windowHandle);
    }

    private void maximize() {
        JNIDecorationsWindows.maximize(windowHandle);
    }

    private void restore() {
        JNIDecorationsWindows.restore(windowHandle);
    }

    private void createActions() {
        closeAction = new CloseAction();
        minimizeAction = new MinimizeAction();
        restoreAction = new RestoreAction();
        maximizeAction = new MaximizeAction();
    }

    private void createIcons() {
        minimizeIcon = new ToggleIcon(UIManager.getIcon("Windows.TitlePane.minimize.icon"),
                UIManager.getIcon("Windows.TitlePane.minimizeInactive.icon"));
        maximizeIcon = new ToggleIcon(UIManager.getIcon("Windows.TitlePane.maximize.icon"),
                UIManager.getIcon("Windows.TitlePane.maximizeInactive.icon"));
        restoreIcon = new ToggleIcon(UIManager.getIcon("Windows.TitlePane.restore.icon"),
                UIManager.getIcon("Windows.TitlePane.restoreInactive.icon"));
        closeIcon = new ToggleIcon(UIManager.getIcon("Windows.TitlePane.close.icon"),
                UIManager.getIcon("Windows.TitlePane.closeInactive.icon"));
    }

    private void createButtons() {
        closeButton = createButton(closeIcon, closeAction, true);
        Icon closePressed = UIManager.getIcon("Windows.TitlePane.closeHover.icon");
        closeButton.setRolloverIcon(closePressed);
        closeButton.setPressedIcon(closePressed);

        minimizeButton = createButton(minimizeIcon, minimizeAction);
        maximizeToggleButton = createButton(maximizeIcon, restoreAction);
        windowIconButton = createWindowIcon();
    }

    private LayoutManager createLayout() {
        return new TitlePaneLayout();
    }

    private void setActive(final boolean active) {
        closeIcon.setActive(active);
        titleLabel.setForeground(active ? activeForeground : inactiveForeground);
        minimizeIcon.setActive(active);
        maximizeIcon.setActive(isResizable() && active);
        closeIcon.setActive(active);
        restoreIcon.setActive(isResizable());
        setButtonActive(minimizeButton, active);
        setButtonActive(maximizeToggleButton, isResizable());
        getRootPane().repaint();
    }

    protected void setButtonActive(final JButton button, final boolean active) {
        if (active) {
            button.putClientProperty("JButton.borderless.hover", null);
            button.putClientProperty("JButton.borderless.click", null);
        } else {
            button.putClientProperty("JButton.borderless.hover", inactiveHover);
            button.putClientProperty("JButton.borderless.click", inactiveClick);
        }
    }

    public void paintComponent(final Graphics g) {
        if (getFrame() != null) {
            setState(getFrame().getExtendedState());
        }
        updateResizeBehaviour();
        Window window = getWindow();
        boolean active = window == null || window.isActive();
        int width = getWidth();
        int height = getHeight();

        Color background = active ? activeBackground : inactiveBackground;

        g.setColor(background);
        g.fillRect(0, 0, width, height);

        if (isDrawBorder()) {
            g.setColor(border);
            g.fillRect(0, height - 1, width, 1);
        }

        GraphicsConfiguration currentGC = getGraphicsConfiguration();
        if (currentGC != null && currentGC != gc) {
            gc = currentGC;
            updateDragArea(gc);
            updateSystemIcon(gc);
        }
    }

    protected boolean isDrawBorder() {
        return getDecorationStyle() != JRootPane.NONE && menuBar != null;
    }

    public JRootPane getRootPane() {
        return rootPane;
    }

    private Frame getFrame() {
        if (window instanceof Frame) {
            return (Frame) window;
        }
        return null;
    }

    private void setState(final int state) {
        setState(state, false);
    }

    protected void updateResizeBehaviour() {
        boolean res = isResizable();
        if (oldResizable != res) {
            oldResizable = res;
            JNIDecorationsWindows.setResizable(windowHandle, res);
        }
    }

    private void setState(final int state, final boolean updateRegardless) {
        Window wnd = getWindow();
        if (wnd != null && getDecorationStyle() == JRootPane.FRAME) {
            if (this.state == state && !updateRegardless) {
                return;
            }
            Frame frame = getFrame();

            if (frame != null) {
                JRootPane rootPane = getRootPane();

                if (((state & Frame.MAXIMIZED_BOTH) != 0)) {
                    oldBorder = rootPane.getBorder();
                    LookAndFeel.uninstallBorder(rootPane);
                } else {
                    Border border = rootPane.getBorder();
                    if (oldBorder != null && border == null || border instanceof UIResource) {
                        rootPane.setBorder(oldBorder);
                    }
                }

                if (frame.isResizable()) {
                    maximizeIcon.setActive(true);
                    restoreIcon.setActive(true);
                    if ((state & Frame.MAXIMIZED_BOTH) != 0) {
                        updateToggleButton(restoreAction, restoreIcon);
                        maximizeAction.setEnabled(false);
                        restoreAction.setEnabled(true);
                    } else {
                        updateToggleButton(maximizeAction, maximizeIcon);
                        maximizeAction.setEnabled(true);
                        restoreAction.setEnabled(false);
                    }
                    maximizeToggleButton.setText(null);
                } else {
                    maximizeIcon.setActive(false);
                    restoreIcon.setActive(false);
                    maximizeAction.setEnabled(false);
                    restoreAction.setEnabled(false);
                }
            } else {
                // Not contained in a Frame
                maximizeAction.setEnabled(false);
                restoreAction.setEnabled(false);
                minimizeAction.setEnabled(false);
            }
            closeAction.setEnabled(true);
            this.state = state;
            repaint();
        }
    }

    protected boolean isResizable() {
        if (JRootPane.NONE == getDecorationStyle()) {
            return false;
        } else {
            if (window instanceof Dialog) {
                return ((Dialog) window).isResizable();
            } else if (window instanceof Frame) {
                return ((Frame) window).isResizable();
            }
        }
        return false;
    }

    private void updateToggleButton(final Action action, final Icon icon) {
        maximizeToggleButton.setAction(action);
        maximizeToggleButton.setIcon(icon);
        maximizeToggleButton.setToolTipText(maximizeToggleButton.getText());
        maximizeToggleButton.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY,
                maximizeToggleButton.getText());
        maximizeToggleButton.setText(null);
    }

    protected boolean isLeftToRight(final Window window) {
        return (window == null) ? getRootPane().getComponentOrientation().isLeftToRight()
                : window.getComponentOrientation().isLeftToRight();
    }

    private void updateSystemIcon(final GraphicsConfiguration gc) {
        boolean frame = getDecorationStyle() == JRootPane.FRAME;
        Window window = getWindow();
        if (window == null) {
            windowIconButton.setIcon(null);
            return;
        }

        List<Image> icons = window.getIconImages();
        Icon systemIcon = null;
        if (icons == null || icons.size() == 0) {
            if (frame) {
                systemIcon = UIManager.getIcon("Windows.TitlePane.icon");
            }
        } else if (icons.size() == 1) {
            systemIcon = new ScaledIcon(icons.get(0).getScaledInstance((int) Scale.scaleWidth(ICON_SIZE, gc),
                    (int) Scale.scaleHeight(ICON_SIZE, gc), Image.SCALE_AREA_AVERAGING), this);
        } else {
            systemIcon = new ScaledIcon(SunToolkit.getScaledIconImage(icons, (int) Scale.scaleWidth(ICON_SIZE, gc),
                    (int) Scale.scaleHeight(ICON_SIZE, gc)), this);
        }
        if (windowIconButton != null) {
            windowIconButton.setIcon(systemIcon);
            SwingUtilities.invokeLater(this::repaint);
        }
    }

    private class CloseAction extends AbstractAction {
        public CloseAction() {
            super(UIManager.getString("Actions.close", getLocale()), closeIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            close();
        }
    }

    private class MinimizeAction extends AbstractAction {
        public MinimizeAction() {
            super(UIManager.getString("Actions.minimize", getLocale()), minimizeIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            minimize();
        }
    }

    private class MaximizeAction extends AbstractAction {
        public MaximizeAction() {
            super(UIManager.getString("Actions.maximize", getLocale()), maximizeIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            maximize();
        }
    }

    private class RestoreAction extends AbstractAction {
        public RestoreAction() {
            super(UIManager.getString("Actions.restore", getLocale()), restoreIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            restore();
        }
    }

    private boolean hideTitleBar() {
        if (titleBarHidden) return true;
        String title = titleLabel.getText();
        if (title == null) title = "";
        // e.g. VCLJ achieves fullscreen by hiding the titlebar through jni and setting visibility
        // of the menubar.
        boolean emptyMenuBar = !unifiedMenuBar || menuBar == null || !menuBar.isVisible();
        boolean emptyContent = getDecorationStyle() == JRootPane.NONE && emptyMenuBar && title.length() == 0;
        return windowHandle == 0 || emptyContent || (menuBar != null && !menuBar.isVisible());
    }

    @Override
    public Dimension getPreferredSize() {
        if (hideTitleBar()) return new Dimension(0, 0);
        int height = computeHeight();
        int width = computeWidth();
        if (isDrawBorder()) height++;
        return new Dimension(width, height);
    }

    private int computeHeight() {
        if (hideTitleBar()) return 0;
        FontMetrics fm = rootPane.getFontMetrics(getFont());
        int height = fm.getHeight() + 7;
        if (menuBar != null) {
            height = Math.max(height, menuBar.getMinimumSize().height);
        }
        return Math.max(BAR_HEIGHT, height);
    }

    private int computeWidth() {
        int width = 0;
        Icon windowIcon = windowIconButton.getIcon();
        if (windowIcon != null) {
            width += Math.min(ICON_WIDTH, Math.max(windowIcon.getIconHeight(), windowIcon.getIconWidth()));
            width += 2 * PAD;
        }
        if (menuBar != null) {
            width += getPreferredMenuSize().width;
            width += PAD;
        }
        boolean frame = getDecorationStyle() == JRootPane.FRAME;
        boolean undecorated = getDecorationStyle() == JRootPane.NONE;
        if (!undecorated) {
            width += BUTTON_WIDTH;
        }
        if (frame) {
            width += BUTTON_WIDTH;
            if (Toolkit.getDefaultToolkit().isFrameStateSupported(Frame.MAXIMIZED_BOTH)) {
                width += BUTTON_WIDTH;
            }
        }
        if (titleLabel != null && !titleLabel.getText().isEmpty()) {
            width += titleLabel.getPreferredSize().width;
            width += PAD;
        }
        return width;
    }

    protected void updateDragArea(final GraphicsConfiguration gc) {
        JNIDecorationsWindows.updateValues(windowHandle, (int) Scale.scaleWidth(left, gc),
                (int) Scale.scaleWidth(right, gc), (int) Scale.scaleHeight(height, gc));
    }

    private Dimension getPreferredMenuSize() {
        LayoutManager menuBarLayout = menuBar.getLayout();
        Dimension size = null;
        if (menuBarLayout != null) {
            size = menuBarLayout.preferredLayoutSize(menuBar);
        }
        return (size != null) ? size : menuBar.getPreferredSize();
    }

    private class TitlePaneLayout implements LayoutManager {
        public void addLayoutComponent(final String name, final Component c) {}

        public void removeLayoutComponent(final Component c) {}

        @Override
        public Dimension preferredLayoutSize(final Container parent) {
            return getPreferredSize();
        }

        public Dimension minimumLayoutSize(final Container c) {
            return preferredLayoutSize(c);
        }

        public void layoutContainer(final Container c) {
            if (hideTitleBar()) return;
            boolean leftToRight = isLeftToRight(window);
            boolean frame = getDecorationStyle() == JRootPane.FRAME;
            boolean undecorated = getDecorationStyle() == JRootPane.NONE;

            windowIconButton.setBounds(Integer.MIN_VALUE, Integer.MIN_VALUE, 0, 0);
            closeButton.setBounds(Integer.MIN_VALUE, Integer.MIN_VALUE, 0, 0);
            minimizeButton.setBounds(Integer.MIN_VALUE, Integer.MIN_VALUE, 0, 0);
            maximizeToggleButton.setBounds(Integer.MIN_VALUE, Integer.MIN_VALUE, 0, 0);

            int w = getWidth();
            int x;
            int start = 0;
            int y = 0;
            height = computeHeight();
            left = 0;
            right = 0;

            Icon windowIcon = windowIconButton.getIcon();
            if (windowIcon != null) {
                int windowButtonWidth = Math.max(windowIcon.getIconHeight(), windowIcon.getIconWidth());
                windowButtonWidth = Math.min(ICON_WIDTH, windowButtonWidth);
                windowIconButton.setBounds(start + PAD, y, windowButtonWidth, height);
                start += windowButtonWidth + 2 * PAD;
                left = start;
            }

            if (menuBar != null) {
                int menuWidth = getPreferredMenuSize().width;
                Insets menuInsets = menuBar.getInsets();
                menuBar.setBounds(start, y, menuWidth, height + menuInsets.bottom);
                start += menuWidth + PAD;
                left += menuWidth;
            }
            x = w;
            if (!undecorated) {
                x -= BUTTON_WIDTH;
                right += BUTTON_WIDTH;
                closeButton.setBounds(x, y, BUTTON_WIDTH, height);
            }
            if (frame) {
                if (Toolkit.getDefaultToolkit().isFrameStateSupported(Frame.MAXIMIZED_BOTH)) {
                    x -= BUTTON_WIDTH;
                    right += BUTTON_WIDTH;
                    maximizeToggleButton.setBounds(x, y, BUTTON_WIDTH, height);
                }
                x -= BUTTON_WIDTH;
                right += BUTTON_WIDTH;
                minimizeButton.setBounds(x, y, BUTTON_WIDTH, height);
                x -= PAD;
            }
            start = Math.max(start, PAD);
            x = Math.min(getWidth() - PAD, x);
            int labelWidth = x - start;
            int prefLabelWidth = titleLabel.getPreferredSize().width;
            if (prefLabelWidth > labelWidth) {
                int extra = Math.min(prefLabelWidth - labelWidth, 2 * PAD);
                start -= extra / 2;
                labelWidth += extra;
            }
            titleLabel.setBounds(start, 0, labelWidth, height);

            if (!leftToRight) {
                mirror(windowIconButton, w);
                mirror(menuBar, w);
                mirror(closeButton, w);
                mirror(minimizeButton, w);
                mirror(maximizeToggleButton, w);
                mirror(titleLabel, w);
                int tmp = left;
                left = right;
                right = tmp;
            }
            updateDragArea(getGraphicsConfiguration());
        }

        private void mirror(final JComponent component, final int w) {
            if (component != null) {
                component.setLocation(w - component.getX() - component.getWidth(), component.getY());
            }
        }
    }

    protected class WindowPropertyChangeListener implements PropertyChangeListener {

        @Override
        public void propertyChange(final PropertyChangeEvent pce) {
            String name = pce.getPropertyName();
            if (KEY_RESIZABLE.equals(name) || KEY_STATE.equals(name)) {
                Frame frame = getFrame();
                if (frame != null) {
                    setState(frame.getExtendedState(), true);
                }
                if (KEY_RESIZABLE.equals(name)) {
                    JNIDecorationsWindows.setResizable(windowHandle, Boolean.TRUE.equals(pce.getNewValue()));
                    getRootPane().repaint();
                }
            } else if (PropertyKey.TITLE.equals(name)) {
                titleLabel.setText(pce.getNewValue() == null ? "" : pce.getNewValue().toString());
                doLayout();
                repaint();
            } else if (PropertyKey.COMPONENT_ORIENTATION.equals(name)) {
                revalidate();
                repaint();
            } else if (KEY_ICON_IMAGE.equals(name)) {
                updateSystemIcon(getGraphicsConfiguration());
                revalidate();
                repaint();
            } else if (PropertyKey.BACKGROUND.equals(name) && pce.getNewValue() instanceof Color) {
                Color color = (Color) pce.getNewValue();
                if (color == null) return;
                JNIDecorationsWindows.setBackground(windowHandle, color.getRed(), color.getGreen(), color.getBlue());
            }
        }
    }

    protected class RootPanePropertyChangeListener implements PropertyChangeListener {

        @Override
        public void propertyChange(final PropertyChangeEvent evt) {
            if ("JRootPane.unifiedMenuBar".equals(evt.getPropertyName())) {
                updateMenuBar(true);
            } else if ("JRootPane.hideTitleBar".equals(evt.getPropertyName())) {
                updateTitleBarVisibility();
            }
        }
    }

    protected class WindowHandler extends WindowAdapter {

        public void windowActivated(final WindowEvent ev) {
            setActive(true);
        }

        public void windowDeactivated(final WindowEvent ev) {
            setActive(false);
        }
    }
}
