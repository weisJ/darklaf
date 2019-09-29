package com.weis.darklaf.ui.rootpane;

/*
 * Copyright 2000-2014 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import com.weis.darklaf.ui.button.DarkButtonUI;
import com.weis.darklaf.util.GraphicsUtil;
import com.weis.darklaf.util.WindowsFrameUtil;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import sun.awt.SunToolkit;

import javax.accessibility.AccessibleContext;
import javax.swing.*;
import javax.swing.plaf.UIResource;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ContainerEvent;
import java.awt.event.ContainerListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;


/**
 * @author Konstantin Bulenkov
 * @author Jannis Weis
 */
public class DarkTitlePane extends JComponent {
    private static final int PAD = 5;
    private static final int TITLE_PAD = 15;
    private static final int BAR_HEIGHT = (int) (56 / GraphicsUtil.SCALE_Y);
    private static final int BUTTON_WIDTH = (int) (92.5 / GraphicsUtil.SCALE_X);
    private static final int ICON_WIDTH = (int) (65 / GraphicsUtil.SCALE_X);
    private static final int IMAGE_HEIGHT = 16;
    private static final int IMAGE_WIDTH = 16;

    private final ContainerListener layeredPaneContainerListener = new ContainerListener() {
        @Override
        public void componentAdded(@NotNull final ContainerEvent e) {
            if (e.getChild() instanceof JMenuBar) {
                menuBar = getRootPane().getJMenuBar();
                menuBar.setPreferredSize(new Dimension(0, 0));
                add(menuBar);
            }
            if (getRootPane().getJMenuBar() == null && menuBar != null) {
                remove(menuBar);
                menuBar = null;
            }
        }

        @Override
        public void componentRemoved(final ContainerEvent e) {
        }
    };
    private final ContainerListener rootPaneContainerListener = new ContainerListener() {
        @Override
        public void componentAdded(@NotNull final ContainerEvent e) {
            if (e.getChild() instanceof JLayeredPane) {
                ((JLayeredPane) e.getChild()).addContainerListener(layeredPaneContainerListener);
            }
        }

        @Override
        public void componentRemoved(@NotNull final ContainerEvent e) {
            if (e.getChild() instanceof JLayeredPane) {
                ((JLayeredPane) e.getChild()).removeContainerListener(layeredPaneContainerListener);
            }
        }
    };
    private PropertyChangeListener propertyChangeListener;
    private WindowListener windowListener;

    private TitlePaneIcon closeIcon;
    private TitlePaneIcon maximizeIcon;
    private TitlePaneIcon restoreIcon;
    private TitlePaneIcon minimizeIcon;

    private JButton windowIconButton;
    private JButton closeButton;
    private JButton maximizeToggleButton;
    private JButton minimizeButton;

    private Action closeAction;
    private Action restoreAction;
    private Action maximizeAction;
    private Action minimizeAction;

    private JLabel titleLabel;

    private Window window;
    private final JRootPane rootPane;
    private JMenuBar menuBar;
    private int state;

    private Color inactiveBackground;
    private Color inactiveForeground;
    private Color activeBackground;
    private Color activeForeground;
    private Color border;

    public DarkTitlePane(final JRootPane root, final DarkRootPaneUI ui) {
        this.rootPane = root;
        rootPane.addContainerListener(rootPaneContainerListener);
        rootPane.getLayeredPane().addContainerListener(layeredPaneContainerListener);
        state = -1;
        installSubcomponents();
        determineColors();
        installDefaults();
        setLayout(createLayout());
    }

    public void uninstall() {
        uninstallListeners();
        window = null;
        rootPane.removeContainerListener(rootPaneContainerListener);
        removeAll();
    }

    private void installListeners() {
        if (window != null) {
            windowListener = createWindowListener();
            window.addWindowListener(windowListener);
            propertyChangeListener = createWindowPropertyChangeListener();
            window.addPropertyChangeListener(propertyChangeListener);
        }
    }

    private void uninstallListeners() {
        if (window != null) {
            window.removeWindowListener(windowListener);
            window.removePropertyChangeListener(propertyChangeListener);
        }
    }

    @NotNull
    @Contract(" -> new")
    private WindowListener createWindowListener() {
        return new DarkTitlePane.WindowHandler();
    }

    @NotNull
    @Contract(value = " -> new", pure = true)
    private PropertyChangeListener createWindowPropertyChangeListener() {
        return new PropertyChangeHandler();
    }

    public JRootPane getRootPane() {
        return rootPane;
    }

    private int getWindowDecorationStyle() {
        return getRootPane().getWindowDecorationStyle();
    }

    public void addNotify() {
        super.addNotify();

        uninstallListeners();

        window = SwingUtilities.getWindowAncestor(this);
        if (window != null) {
            WindowsFrameUtil.enableTitleBar(window, false);

            if (window instanceof Frame) {
                titleLabel.setText(((Frame) window).getTitle());
                ((Frame) window).setMaximizedBounds(getMaximizedBounds());
                setState(((Frame) window).getExtendedState());
            } else {
                setState(0);
            }
            if (window instanceof Dialog) {
                titleLabel.setText(((Dialog) window).getTitle());
            }
            setActive(window.isActive());
            installListeners();
            updateSystemIcon();
        }
    }

    @NotNull
    private Rectangle getMaximizedBounds() {
        Insets screenInsets = getToolkit().getScreenInsets(getGraphicsConfiguration());
        Rectangle screenSize = getGraphicsConfiguration().getBounds();
        return new Rectangle(screenInsets.left + screenSize.x,
                             screenInsets.top + screenSize.y,
                             screenSize.x + screenSize.width - screenInsets.right - screenInsets.left,
                             screenSize.y + screenSize.height - screenInsets.bottom - screenInsets.top);
    }

    public void removeNotify() {
        super.removeNotify();

        uninstallListeners();
        window = null;
    }

    private void installSubcomponents() {
        int decorationStyle = getWindowDecorationStyle();
        titleLabel = new JLabel();
        add(titleLabel);

        createIcons();
        createActions();
        createButtons();
        if (decorationStyle == JRootPane.FRAME) {
            windowIconButton = createWindowIcon();
            add(windowIconButton);
            add(minimizeButton);
            add(maximizeToggleButton);
            add(closeButton);
        } else if (decorationStyle == JRootPane.PLAIN_DIALOG ||
                   decorationStyle == JRootPane.INFORMATION_DIALOG ||
                   decorationStyle == JRootPane.ERROR_DIALOG ||
                   decorationStyle == JRootPane.COLOR_CHOOSER_DIALOG ||
                   decorationStyle == JRootPane.FILE_CHOOSER_DIALOG ||
                   decorationStyle == JRootPane.QUESTION_DIALOG ||
                   decorationStyle == JRootPane.WARNING_DIALOG) {
            add(closeButton);
        }
    }

    private void determineColors() {
        switch (getWindowDecorationStyle()) {
            case JRootPane.ERROR_DIALOG:
                activeBackground = UIManager.getColor("OptionPane.errorDialog.titlePane.background");
                activeForeground = UIManager.getColor("OptionPane.errorDialog.titlePane.foreground");
                break;
            case JRootPane.QUESTION_DIALOG:
            case JRootPane.COLOR_CHOOSER_DIALOG:
            case JRootPane.FILE_CHOOSER_DIALOG:
                activeBackground = UIManager.getColor("OptionPane.questionDialog.titlePane.background");
                activeForeground = UIManager.getColor("OptionPane.questionDialog.titlePane.foreground");
                break;
            case JRootPane.WARNING_DIALOG:
                activeBackground = UIManager.getColor("OptionPane.warningDialog.titlePane.background");
                activeForeground = UIManager.getColor("OptionPane.warningDialog.titlePane.foreground");
                break;
            default: //JRootPane.Frame
                activeBackground = UIManager.getColor("TitlePane.background");
                activeForeground = UIManager.getColor("TitlePane.foreground");
                break;
        }
        inactiveBackground = UIManager.getColor("TitlePane.inactiveBackground");
        inactiveForeground = UIManager.getColor("TitlePane.inactiveForeground");
        border = UIManager.getColor("TitlePane.borderColor");
    }

    private void installDefaults() {
        setFont(UIManager.getFont("InternalFrame.titleFont", getLocale()));
    }


    protected JButton createWindowIcon() {
        windowIconButton = new JButton();
        windowIconButton.setComponentPopupMenu(createMenu());
        windowIconButton.putClientProperty("JButton.variant", "onlyLabel");
        windowIconButton.addActionListener(
                e -> windowIconButton.getComponentPopupMenu().show(windowIconButton,
                                                                   windowIconButton.getWidth() / 2,
                                                                   windowIconButton.getHeight() / 2));
        windowIconButton.setFocusable(false);
        windowIconButton.setBorderPainted(false);
        return windowIconButton;
    }


    @NotNull
    private JPopupMenu createMenu() {
        JPopupMenu menu = new JPopupMenu();
        if (getWindowDecorationStyle() == JRootPane.FRAME) {
            addMenuItems(menu);
        }
        return menu;
    }

    private void addMenuItems(@NotNull final JPopupMenu menu) {
        menu.add(new JMenuItem(restoreAction) {{
            setDisabledIcon(restoreIcon);
        }});
        menu.add(new JMenuItem(minimizeAction) {{
            setDisabledIcon(minimizeIcon);
        }});
        if (Toolkit.getDefaultToolkit().isFrameStateSupported(Frame.MAXIMIZED_BOTH)) {
            menu.add(new JMenuItem(maximizeAction) {{
                setDisabledIcon(maximizeIcon);
            }});
        }
        menu.add(new JSeparator());
        menu.add(new JMenuItem(closeAction) {{
            setDisabledIcon(closeIcon);
        }});
    }

    private void close() {
        Window window = getWindow();

        if (window != null) {
            window.dispatchEvent(new WindowEvent(window, WindowEvent.WINDOW_CLOSING));
        }
    }

    private void minimize() {
        Frame frame = getFrame();
        if (frame != null) {
            frame.setExtendedState(state | Frame.ICONIFIED);
        }
    }

    private void maximize() {
        Frame frame = getFrame();
        if (frame != null) {
            frame.setExtendedState(state | Frame.MAXIMIZED_BOTH);
        }
    }

    private void restore() {
        Frame frame = getFrame();

        if (frame == null) {
            return;
        }

        if ((state & Frame.ICONIFIED) != 0) {
            frame.setExtendedState(state & ~Frame.ICONIFIED);
        } else {
            frame.setExtendedState(state & ~Frame.MAXIMIZED_BOTH);
        }
    }

    private void createActions() {
        closeAction = new CloseAction();
        if (getWindowDecorationStyle() == JRootPane.FRAME) {
            minimizeAction = new MinimizeAction();
            restoreAction = new RestoreAction();
            maximizeAction = new MaximizeAction();
        }
    }

    private void createIcons() {
        minimizeIcon = new TitlePaneIcon(UIManager.getIcon("TitlePane.minimize.icon"),
                                         UIManager.getIcon("TitlePane.minimizeInactive.icon"));
        maximizeIcon = new TitlePaneIcon(UIManager.getIcon("TitlePane.maximize.icon"),
                                         UIManager.getIcon("TitlePane.maximizeInactive.icon"));
        restoreIcon = new TitlePaneIcon(UIManager.getIcon("TitlePane.restore.icon"),
                                        UIManager.getIcon("TitlePane.restoreInactive.icon"));
        closeIcon = new TitlePaneIcon(UIManager.getIcon("TitlePane.close.icon"),
                                      UIManager.getIcon("TitlePane.close.icon"));
    }

    private void createButtons() {
        closeButton = createButton("Close", closeIcon, closeAction);
        closeButton.setRolloverIcon(UIManager.getIcon("TitlePane.closeHover.icon"));
        closeButton.setUI(new CloseButtonUI());

        if (getWindowDecorationStyle() == JRootPane.FRAME) {
            minimizeButton = createButton("Iconify", minimizeIcon, minimizeAction);
            maximizeToggleButton = createButton("Maximize", maximizeIcon, restoreAction);
        }
    }


    @NotNull
    private static JButton createButton(final String accessibleName, final Icon icon, final Action action) {
        JButton button = new JButton();
        button.setFocusable(false);
        button.setOpaque(true);
        button.setRolloverEnabled(true);
        button.putClientProperty("JButton.variant", "fullShadow");
        button.putClientProperty("paintActive", Boolean.TRUE);
        button.putClientProperty(AccessibleContext.ACCESSIBLE_NAME_PROPERTY, accessibleName);
        button.setAction(action);
        button.setIcon(icon);
        button.setText(null);
        return button;
    }


    @NotNull
    @Contract(value = " -> new", pure = true)
    private LayoutManager createLayout() {
        return new TitlePaneLayout();
    }

    private void setActive(final boolean active) {
        closeIcon.setActive(active);
        titleLabel.setForeground(active ? activeForeground : inactiveForeground);
        if (getWindowDecorationStyle() == JRootPane.FRAME) {
            minimizeIcon.setActive(active);
            maximizeIcon.setActive(active);
            restoreIcon.setActive(active);
        }
        getRootPane().repaint();
    }

    private void setState(final int state) {
        setState(state, false);
    }

    private void setState(final int state, final boolean updateRegardless) {
        Window wnd = getWindow();

        if (wnd != null && getWindowDecorationStyle() == JRootPane.FRAME) {
            if (this.state == state && !updateRegardless) {
                return;
            }
            Frame frame = getFrame();

            if (frame != null) {
                JRootPane rootPane = getRootPane();

                if (((state & Frame.MAXIMIZED_BOTH) != 0) &&
                    (rootPane.getBorder() == null ||
                     (rootPane.getBorder() instanceof UIResource)) &&
                    frame.isShowing()) {
                    rootPane.setBorder(null);
                } else if ((state & Frame.MAXIMIZED_BOTH) == 0) {
                    // This is a croak, if state becomes bound, this can
                    // be nuked.
                    //Todo
//                    rootPaneUI.installBorder(rootPane);
                }
                if (frame.isResizable()) {
                    if ((state & Frame.MAXIMIZED_BOTH) != 0) {
                        updateToggleButton(restoreAction, restoreIcon);
                        maximizeAction.setEnabled(false);
                        restoreAction.setEnabled(true);
                    } else {
                        updateToggleButton(maximizeAction, maximizeIcon);
                        maximizeAction.setEnabled(true);
                        restoreAction.setEnabled(false);
                    }
                    if (maximizeToggleButton.getParent() == null ||
                        minimizeButton.getParent() == null) {
                        add(maximizeToggleButton);
                        add(minimizeButton);
                        revalidate();
                        repaint();
                    }
                    maximizeToggleButton.setText(null);
                } else {
                    maximizeAction.setEnabled(false);
                    restoreAction.setEnabled(false);
                    if (maximizeToggleButton.getParent() != null) {
                        remove(maximizeToggleButton);
                        revalidate();
                        repaint();
                    }
                }
            } else {
                // Not contained in a Frame
                maximizeAction.setEnabled(false);
                restoreAction.setEnabled(false);
                minimizeAction.setEnabled(false);
                remove(maximizeToggleButton);
                remove(minimizeButton);
                revalidate();
                repaint();
            }
            closeAction.setEnabled(true);
            this.state = state;
        }
    }

    private void updateToggleButton(final Action action, final Icon icon) {
        maximizeToggleButton.setAction(action);
        maximizeToggleButton.setIcon(icon);
        maximizeToggleButton.setText(null);
    }

    @Nullable
    @Contract(pure = true)
    private Frame getFrame() {
        Window window = getWindow();

        if (window instanceof Frame) {
            return (Frame) window;
        }
        return null;
    }

    @Contract(pure = true)
    private Window getWindow() {
        return window;
    }

    public void paintComponent(final Graphics g) {
        if (getFrame() != null) {
            setState(getFrame().getExtendedState());
        }
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

    protected boolean isLeftToRight(final Window window) {
        return (window == null) ?
               getRootPane().getComponentOrientation().isLeftToRight() :
               window.getComponentOrientation().isLeftToRight();
    }

    private class CloseAction extends AbstractAction {
        public CloseAction() {
            super("Close", closeIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            close();
        }
    }


    private class MinimizeAction extends AbstractAction {
        public MinimizeAction() {
            //UIManager.getString("Minimize", getLocale())
            super("Minimize", minimizeIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            minimize();
        }
    }

    private class MaximizeAction extends AbstractAction {
        public MaximizeAction() {
            super("Maximize", maximizeIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            maximize();
        }
    }

    private class RestoreAction extends AbstractAction {
        public RestoreAction() {
            super("Restore", restoreIcon);
        }

        public void actionPerformed(final ActionEvent e) {
            restore();
        }
    }

    private class TitlePaneLayout implements LayoutManager {
        public void addLayoutComponent(final String name, final Component c) {
        }

        public void removeLayoutComponent(final Component c) {
        }

        public Dimension preferredLayoutSize(final Container c) {
            int size = computeHeight();
            return new Dimension(size + 1, size + 1);
        }

        public Dimension minimumLayoutSize(final Container c) {
            return preferredLayoutSize(c);
        }

        private int computeHeight() {
            FontMetrics fm = rootPane.getFontMetrics(getFont());
            int height = fm.getHeight() + 7;
            if (menuBar != null) {
                height = Math.max(height, menuBar.getMinimumSize().height);
            }
            return Math.max(BAR_HEIGHT, height);
        }

        private Dimension getPreferredMenuSize() {
            var menuBarLayout = menuBar.getLayout();
            Dimension size = null;
            if (menuBarLayout != null) {
                size = menuBarLayout.preferredLayoutSize(menuBar);
            }
            return (size != null) ? size : menuBar.getPreferredSize();
        }

        public void layoutContainer(final Container c) {
            boolean leftToRight = isLeftToRight(window);

            int w = getWidth();
            int x;
            int start = 0;
            int y = 0;
            int height = computeHeight();

            if (leftToRight) {
                if (windowIconButton != null) {
                    windowIconButton.setBounds(start + PAD, y, ICON_WIDTH, height);
                    start += ICON_WIDTH + PAD;
                }
                if (menuBar != null) {
                    int menuWidth = getPreferredMenuSize().width;
                    Insets menuInsets = menuBar.getInsets();
                    menuBar.setBounds(start + PAD, y, menuWidth, height + menuInsets.bottom);
                    start += menuWidth + PAD;
                }
                x = w;
                if (closeButton != null) {
                    x -= BUTTON_WIDTH;
                    closeButton.setBounds(x, y, BUTTON_WIDTH, height);
                }
                if (getWindowDecorationStyle() == JRootPane.FRAME) {
                    if (Toolkit.getDefaultToolkit().isFrameStateSupported(Frame.MAXIMIZED_BOTH)) {
                        if (minimizeButton != null && maximizeToggleButton.getParent() != null) {
                            x -= BUTTON_WIDTH;
                            maximizeToggleButton.setBounds(x, y, BUTTON_WIDTH, height);
                        }
                        if (minimizeButton != null && minimizeButton.getParent() != null) {
                            x -= BUTTON_WIDTH;
                            minimizeButton.setBounds(x, y, BUTTON_WIDTH, height);
                        }
                    }
                }
                titleLabel.setBounds(start + TITLE_PAD, 0, x - start - 2 * TITLE_PAD, height);
            } else {
                //Todo.
            }
        }
    }

    protected class PropertyChangeHandler implements PropertyChangeListener {
        public void propertyChange(@NotNull final PropertyChangeEvent pce) {
            String name = pce.getPropertyName();
            if ("resizable".equals(name) || "state".equals(name)) {
                Frame frame = getFrame();
                if (frame != null) {
                    setState(frame.getExtendedState(), true);
                }
                if ("resizable".equals(name)) {
                    getRootPane().repaint();
                }
            } else if ("title".equals(name)) {
                titleLabel.setText(pce.getNewValue().toString());
                repaint();
            } else if ("componentOrientation".equals(name)) {
                revalidate();
                repaint();
            } else if ("iconImage".equals(name)) {
                updateSystemIcon();
                revalidate();
                repaint();
            }
        }
    }

    private void updateSystemIcon() {
        Window window = getWindow();
        if (window == null) {
            windowIconButton.setIcon(null);
            return;
        }

        List<Image> icons = window.getIconImages();
        assert icons != null;

        Icon systemIcon;
        if (icons.size() == 0) {
            systemIcon = UIManager.getIcon("TitlePane.icon");
        } else if (icons.size() == 1) {
            systemIcon = new ImageIcon(icons.get(0));
        } else {
            systemIcon = new ImageIcon(SunToolkit.getScaledIconImage(icons, IMAGE_WIDTH, IMAGE_HEIGHT));
        }
        if (windowIconButton != null) {
            windowIconButton.setIcon(systemIcon);
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

    protected static class TitlePaneIcon implements Icon {

        private final Icon activeIcon;
        private final Icon inactiveIcon;
        private boolean active = true;

        @Contract(pure = true)
        protected TitlePaneIcon(final Icon active, final Icon inactive) {
            this.activeIcon = active;
            this.inactiveIcon = inactive;
        }

        public void setActive(final boolean active) {
            this.active = active;
        }

        @Override
        public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
            currentIcon().paintIcon(c, g, x, y);
        }

        @Contract(pure = true)
        private Icon currentIcon() {
            return active ? activeIcon : inactiveIcon;
        }

        @Override
        public int getIconWidth() {
            return currentIcon().getIconWidth();
        }

        @Override
        public int getIconHeight() {
            return currentIcon().getIconHeight();
        }
    }

    protected static class CloseButtonUI extends DarkButtonUI {

        @Override
        protected Color getShadowColor(@NotNull final AbstractButton c) {
            return c.getModel().isArmed() ? UIManager.getColor("TitlePane.close.clickColor")
                                          : UIManager.getColor("TitlePane.close.rollOverColor");
        }
    }
}
