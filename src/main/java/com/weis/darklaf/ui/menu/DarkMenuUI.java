package com.weis.darklaf.ui.menu;

import com.weis.darklaf.util.DarkUIUtil;
import com.weis.darklaf.util.LazyActionMap;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import sun.swing.DefaultLookup;
import sun.swing.UIAction;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.MenuDragMouseEvent;
import javax.swing.event.MenuDragMouseListener;
import javax.swing.event.MenuKeyEvent;
import javax.swing.event.MenuKeyListener;
import javax.swing.event.MenuListener;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicMenuUI;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Objects;

/**
 * Code taken from {@link BasicMenuUI}
 */
public class DarkMenuUI extends DarkMenuItemUIBase {

    /**
     * The instance of {@code ChangeListener}.
     */
    protected ChangeListener changeListener;

    /**
     * The instance of {@code MenuListener}.
     */
    protected MenuListener menuListener;

    private int lastMnemonic = 0;

    /**
     * Uses as the parent of the windowInputMap when selected.
     */
    private InputMap selectedWindowInputMap;

    /* diagnostic aids -- should be false for production builds. */
    private static final boolean TRACE = false; // trace creates and disposes
    private static final boolean VERBOSE = false; // show reuse hits/misses
    private static final boolean DEBUG = false;  // show bad params, misc.

    private static boolean crossMenuMnemonic = true;

    /**
     * Constructs a new instance of {@code BasicMenuUI}.
     *
     * @param x a component
     * @return a new instance of {@code BasicMenuUI}
     */
    @NotNull
    @Contract(value = "_ -> new", pure = true)
    public static ComponentUI createUI(final JComponent x) {
        return new DarkMenuUI();
    }

    protected static void loadActionMap(@NotNull final LazyActionMap map) {
        loadActionMap(map);
        map.put(new Actions(Actions.SELECT, null, true));
    }


    protected void installDefaults() {
        super.installDefaults();
        updateDefaultBackgroundColor();
        ((JMenu) menuItem).setDelay(200);
        crossMenuMnemonic = UIManager.getBoolean("Menu.crossMenuMnemonic");
    }

    protected String getPropertyPrefix() {
        return "Menu";
    }

    protected void installListeners() {
        super.installListeners();

        if (changeListener == null) { changeListener = createChangeListener(menuItem); }

        if (changeListener != null) { menuItem.addChangeListener(changeListener); }

        if (menuListener == null) { menuListener = createMenuListener(menuItem); }

        if (menuListener != null) { ((JMenu) menuItem).addMenuListener(menuListener); }
    }

    protected void installKeyboardActions() {
        super.installKeyboardActions();
        updateMnemonicBinding();
    }

    void installLazyActionMap() {
        LazyActionMap.installLazyActionMap(menuItem, BasicMenuUI.class,
                                           getPropertyPrefix() + ".actionMap");
    }

    @SuppressWarnings("deprecation")
    void updateMnemonicBinding() {
        int mnemonic = menuItem.getModel().getMnemonic();
        int[] shortcutKeys = (int[]) DefaultLookup.get(menuItem, this,
                                                       "Menu.shortcutKeys");
        if (shortcutKeys == null) {
            shortcutKeys = new int[]{KeyEvent.ALT_MASK,
                    KeyEvent.ALT_MASK | KeyEvent.ALT_GRAPH_MASK};
        }
        if (mnemonic == lastMnemonic) {
            return;
        }
        InputMap windowInputMap = SwingUtilities.getUIInputMap(
                menuItem, JComponent.WHEN_IN_FOCUSED_WINDOW);
        if (lastMnemonic != 0 && windowInputMap != null) {
            for (int shortcutKey : shortcutKeys) {
                windowInputMap.remove(KeyStroke.getKeyStroke
                        (lastMnemonic, shortcutKey, false));
            }
        }
        if (mnemonic != 0) {
            if (windowInputMap == null) {
                windowInputMap = createInputMap(JComponent.
                                                        WHEN_IN_FOCUSED_WINDOW);
                SwingUtilities.replaceUIInputMap(menuItem, JComponent.
                        WHEN_IN_FOCUSED_WINDOW, windowInputMap);
            }
            for (int shortcutKey : shortcutKeys) {
                windowInputMap.put(KeyStroke.getKeyStroke(mnemonic,
                                                          shortcutKey, false), "selectMenu");
            }
        }
        lastMnemonic = mnemonic;
    }

    protected void uninstallKeyboardActions() {
        super.uninstallKeyboardActions();
        lastMnemonic = 0;
    }

    protected MouseInputListener createMouseInputListener(final JComponent c) {
        return getHandler();
    }

    /**
     * Returns an instance of {@code MenuListener}.
     *
     * @param c a component
     * @return an instance of {@code MenuListener}
     */
    protected MenuListener createMenuListener(final JComponent c) {
        return null;
    }

    /**
     * Returns an instance of {@code ChangeListener}.
     *
     * @param c a component
     * @return an instance of {@code ChangeListener}
     */
    protected ChangeListener createChangeListener(final JComponent c) {
        return null;
    }

    protected PropertyChangeListener createPropertyChangeListener(final JComponent c) {
        return getHandler();
    }

    protected DarkMenuItemUIBase.Handler getHandler() {
        if (handler == null) {
            handler = new DarkMenuUI.Handler();
        }
        return handler;
    }

    protected void uninstallDefaults() {
        menuItem.setArmed(false);
        menuItem.setSelected(false);
        menuItem.resetKeyboardActions();
        super.uninstallDefaults();
    }

    protected void uninstallListeners() {
        super.uninstallListeners();

        if (changeListener != null) { menuItem.removeChangeListener(changeListener); }

        if (menuListener != null) { ((JMenu) menuItem).removeMenuListener(menuListener); }

        changeListener = null;
        menuListener = null;
        handler = null;
    }

    protected MenuDragMouseListener createMenuDragMouseListener(final JComponent c) {
        return getHandler();
    }

    protected MenuKeyListener createMenuKeyListener(final JComponent c) {
        return (MenuKeyListener) getHandler();
    }

    public Dimension getMinimumSize(final JComponent c) {
        return (((JMenu) menuItem).isTopLevelMenu()) ?
               c.getPreferredSize() : null;
    }

    public Dimension getMaximumSize(final JComponent c) {
        if (((JMenu) menuItem).isTopLevelMenu()) {
            Dimension d = c.getPreferredSize();
            return new Dimension(d.width, Short.MAX_VALUE);
        }
        return null;
    }

    /**
     * Sets timer to the {@code menu}.
     *
     * @param menu an instance of {@code JMenu}.
     */
    protected void setupPostTimer(@NotNull final JMenu menu) {
        Timer timer = new Timer(menu.getDelay(), new Actions(Actions.SELECT, menu, false));
        timer.setRepeats(false);
        timer.start();
    }

    private static void appendPath(@NotNull final MenuElement[] path, final MenuElement elem) {
        MenuElement[] newPath = new MenuElement[path.length + 1];
        System.arraycopy(path, 0, newPath, 0, path.length);
        newPath[path.length] = elem;
        MenuSelectionManager.defaultManager().setSelectedPath(newPath);
    }

    private static class Actions extends UIAction {
        private static final String SELECT = "selectMenu";

        // NOTE: This will be null if the action is registered in the
        // ActionMap. For the timer use it will be non-null.
        private JMenu menu;
        private boolean force = false;

        Actions(final String key, final JMenu menu, final boolean shouldForce) {
            super(key);
            this.menu = menu;
            this.force = shouldForce;
        }

        private JMenu getMenu(final ActionEvent e) {
            if (e.getSource() instanceof JMenu) {
                return (JMenu) e.getSource();
            }
            return menu;
        }

        public void actionPerformed(final ActionEvent e) {
            JMenu menu = getMenu(e);
            if (!crossMenuMnemonic) {
                JPopupMenu pm = getLastPopup();
                if (pm != null && pm != menu.getParent()) {
                    return;
                }
            }

            final MenuSelectionManager defaultManager = MenuSelectionManager.defaultManager();
            if (force) {
                Container cnt = menu.getParent();
                if (cnt instanceof JMenuBar) {
                    MenuElement[] me;
                    MenuElement[] subElements;

                    subElements = menu.getPopupMenu().getSubElements();
                    if (subElements.length > 0) {
                        me = new MenuElement[4];
                        me[0] = (MenuElement) cnt;
                        me[1] = menu;
                        me[2] = menu.getPopupMenu();
                        me[3] = subElements[0];
                    } else {
                        me = new MenuElement[3];
                        me[0] = (MenuElement) cnt;
                        me[1] = menu;
                        me[2] = menu.getPopupMenu();
                    }
                    defaultManager.setSelectedPath(me);
                }
            } else {
                MenuElement[] path = defaultManager.getSelectedPath();
                if (path.length > 0 && path[path.length - 1] == menu) {
                    appendPath(path, menu.getPopupMenu());
                }
            }
        }

        @Override
        public boolean accept(final Object c) {
            if (c instanceof JMenu) {
                return ((JMenu) c).isEnabled();
            }
            return true;
        }
    }

    /*
     * Set the background color depending on whether this is a toplevel menu
     * in a menubar or a submenu of another menu.
     */
    private void updateDefaultBackgroundColor() {
        if (!UIManager.getBoolean("Menu.useMenuBarBackgroundForTopLevel")) {
            return;
        }
        JMenu menu = (JMenu) menuItem;
        if (menu.getBackground() instanceof UIResource) {
            if (menu.isTopLevelMenu()) {
                menu.setBackground(UIManager.getColor("MenuBar.background"));
            } else {
                menu.setBackground(UIManager.getColor(getPropertyPrefix() + ".background"));
            }
        }
    }

    /**
     * Instantiated and used by a menu item to handle the current menu selection
     * from mouse events. A MouseInputHandler processes and forwards all mouse events
     * to a shared instance of the MenuSelectionManager.
     * <p>
     * This class is protected so that it can be subclassed by other look and
     * feels to implement their own mouse handling behavior. All overridden
     * methods should call the parent methods so that the menu selection
     * is correct.
     *
     * @see javax.swing.MenuSelectionManager
     * @since 1.4
     */
    protected class MouseInputHandler implements MouseInputListener {
        // NOTE: This class exists only for backward compatibility. All
        // its functionality has been moved into Handler. If you need to add
        // new functionality add it to the Handler, but make sure this
        // class calls into the Handler.

        public void mouseClicked(final MouseEvent e) {
            getHandler().mouseClicked(e);
        }

        /**
         * Invoked when the mouse has been clicked on the menu. This
         * method clears or sets the selection path of the
         * MenuSelectionManager.
         *
         * @param e the mouse event
         */
        public void mousePressed(final MouseEvent e) {
            getHandler().mousePressed(e);
        }

        /**
         * Invoked when the mouse has been released on the menu. Delegates the
         * mouse event to the MenuSelectionManager.
         *
         * @param e the mouse event
         */
        public void mouseReleased(final MouseEvent e) {
            getHandler().mouseReleased(e);
        }

        /**
         * Invoked when the cursor enters the menu. This method sets the selected
         * path for the MenuSelectionManager and handles the case
         * in which a menu item is used to pop up an additional menu, as in a
         * hierarchical menu system.
         *
         * @param e the mouse event; not used
         */
        public void mouseEntered(final MouseEvent e) {
            getHandler().mouseEntered(e);
        }

        public void mouseExited(final MouseEvent e) {
            getHandler().mouseExited(e);
        }

        /**
         * Invoked when a mouse button is pressed on the menu and then dragged.
         * Delegates the mouse event to the MenuSelectionManager.
         *
         * @param e the mouse event
         * @see java.awt.event.MouseMotionListener#mouseDragged
         */
        public void mouseDragged(final MouseEvent e) {
            getHandler().mouseDragged(e);
        }

        public void mouseMoved(final MouseEvent e) {
            getHandler().mouseMoved(e);
        }
    }

    /**
     * As of Java 2 platform 1.4, this previously undocumented class
     * is now obsolete. KeyBindings are now managed by the popup menu.
     */
    public class ChangeHandler implements ChangeListener {
        /**
         * The instance of {@code JMenu}.
         */
        public JMenu menu;

        /**
         * The instance of {@code BasicMenuUI}.
         */
        public BasicMenuUI ui;

        /**
         * {@code true} if an item of popup menu is selected.
         */
        public boolean isSelected = false;

        /**
         * The component that was focused.
         */
        public Component wasFocused;

        /**
         * Constructs a new instance of {@code ChangeHandler}.
         *
         * @param m  an instance of {@code JMenu}
         * @param ui an instance of {@code BasicMenuUI}
         */
        public ChangeHandler(final JMenu m, final BasicMenuUI ui) {
            menu = m;
            this.ui = ui;
        }

        public void stateChanged(final ChangeEvent e) {
        }
    }

    private class Handler extends DarkMenuItemUIBase.Handler implements MenuKeyListener {
        //
        // PropertyChangeListener
        //
        public void propertyChange(final PropertyChangeEvent e) {
            if (Objects.equals(e.getPropertyName(), AbstractButton.
                    MNEMONIC_CHANGED_PROPERTY)) {
                updateMnemonicBinding();
            } else {
                if (e.getPropertyName().equals("ancestor")) {
                    updateDefaultBackgroundColor();
                }
                super.propertyChange(e);
            }
        }

        //
        // MouseInputListener
        //
        public void mouseClicked(final MouseEvent e) {
        }

        /**
         * Invoked when the mouse has been clicked on the menu. This
         * method clears or sets the selection path of the
         * MenuSelectionManager.
         *
         * @param e the mouse event
         */
        public void mousePressed(final MouseEvent e) {
            JMenu menu = (JMenu) menuItem;
            if (!menu.isEnabled()) { return; }

            MenuSelectionManager manager = MenuSelectionManager.defaultManager();
            if (menu.isTopLevelMenu()) {
                if (menu.isSelected() && menu.getPopupMenu().isShowing()) {
                    manager.clearSelectedPath();
                } else {
                    Container cnt = menu.getParent();
                    if (cnt instanceof JMenuBar) {
                        MenuElement[] me = new MenuElement[2];
                        me[0] = (MenuElement) cnt;
                        me[1] = menu;
                        manager.setSelectedPath(me);
                    }
                }
            }

            MenuElement[] selectedPath = manager.getSelectedPath();
            if (selectedPath.length > 0 && selectedPath[selectedPath.length - 1] != menu.getPopupMenu()) {
                if (menu.isTopLevelMenu() || menu.getDelay() == 0) {
                    appendPath(selectedPath, menu.getPopupMenu());
                } else {
                    setupPostTimer(menu);
                }
            }
        }

        /**
         * Invoked when the mouse has been released on the menu. Delegates the
         * mouse event to the MenuSelectionManager.
         *
         * @param e the mouse event
         */
        public void mouseReleased(final MouseEvent e) {
            JMenu menu = (JMenu) menuItem;
            if (!menu.isEnabled()) { return; }
            MenuSelectionManager manager = MenuSelectionManager.defaultManager();
            manager.processMouseEvent(e);
            if (!e.isConsumed()) { manager.clearSelectedPath(); }
        }

        /**
         * Invoked when the cursor enters the menu. This method sets the selected
         * path for the MenuSelectionManager and handles the case
         * in which a menu item is used to pop up an additional menu, as in a
         * hierarchical menu system.
         *
         * @param e the mouse event; not used
         */
        public void mouseEntered(final MouseEvent e) {
            JMenu menu = (JMenu) menuItem;
            // only disable the menu highlighting if it's disabled and the property isn't
            // true. This allows disabled rollovers to work in WinL&F
            if (!menu.isEnabled() && !UIManager.getBoolean("MenuItem.disabledAreNavigable")) {
                return;
            }

            MenuSelectionManager manager = MenuSelectionManager.defaultManager();
            MenuElement[] selectedPath = manager.getSelectedPath();
            if (!menu.isTopLevelMenu()) {
                if (!(selectedPath.length > 0 && selectedPath[selectedPath.length - 1] == menu.getPopupMenu())) {
                    if (menu.getDelay() == 0) {
                        appendPath(getPath(), menu.getPopupMenu());
                    } else {
                        manager.setSelectedPath(getPath());
                        setupPostTimer(menu);
                    }
                }
            } else {
                if (selectedPath.length > 0 && selectedPath[0] == menu.getParent()) {
                    MenuElement[] newPath = new MenuElement[3];
                    // A top level menu's parent is by definition
                    // a JMenuBar
                    newPath[0] = (MenuElement) menu.getParent();
                    newPath[1] = menu;
                    if (getLastPopup() != null) {
                        newPath[2] = menu.getPopupMenu();
                    }
                    manager.setSelectedPath(newPath);
                }
            }
        }

        public void mouseExited(final MouseEvent e) {
        }

        /**
         * Invoked when a mouse button is pressed on the menu and then dragged.
         * Delegates the mouse event to the MenuSelectionManager.
         *
         * @param e the mouse event
         * @see java.awt.event.MouseMotionListener#mouseDragged
         */
        public void mouseDragged(final MouseEvent e) {
            JMenu menu = (JMenu) menuItem;
            if (!menu.isEnabled()) { return; }
            MenuSelectionManager.defaultManager().processMouseEvent(e);
        }

        public void mouseMoved(final MouseEvent e) {
        }


        //
        // MenuDragHandler
        //
        public void menuDragMouseEntered(final MenuDragMouseEvent e) {
        }

        public void menuDragMouseDragged(final MenuDragMouseEvent e) {
            if (!menuItem.isEnabled()) return;

            MenuSelectionManager manager = e.getMenuSelectionManager();
            MenuElement[] path = e.getPath();

            Point p = e.getPoint();
            if (p.x >= 0 && p.x < menuItem.getWidth() &&
                    p.y >= 0 && p.y < menuItem.getHeight()) {
                JMenu menu = (JMenu) menuItem;
                MenuElement[] selectedPath = manager.getSelectedPath();
                if (!(selectedPath.length > 0 &&
                        selectedPath[selectedPath.length - 1] ==
                                menu.getPopupMenu())) {
                    if (menu.isTopLevelMenu() ||
                            menu.getDelay() == 0 ||
                            e.getID() == MouseEvent.MOUSE_DRAGGED) {
                        appendPath(path, menu.getPopupMenu());
                    } else {
                        manager.setSelectedPath(path);
                        setupPostTimer(menu);
                    }
                }
            } else if (e.getID() == MouseEvent.MOUSE_RELEASED) {
                Component comp = manager.componentForPoint(e.getComponent(), e.getPoint());
                if (comp == null) { manager.clearSelectedPath(); }
            }

        }

        public void menuDragMouseExited(final MenuDragMouseEvent e) {
        }

        public void menuDragMouseReleased(final MenuDragMouseEvent e) {
        }

        //
        // MenuKeyListener
        //

        /**
         * Open the Menu
         */
        public void menuKeyTyped(final MenuKeyEvent e) {
            if (!crossMenuMnemonic && getLastPopup() != null) {
                // when crossMenuMnemonic is not set, we don't open a toplevel
                // menu if another toplevel menu is already open
                return;
            }

            if (getPopups().size() != 0) {
                //Fix 6939261: to return in case not on the main menu
                //and has a pop-up.
                //after return code will be handled in BasicPopupMenuUI.java
                return;
            }

            char key = Character.toLowerCase((char) menuItem.getMnemonic());
            MenuElement[] path = e.getPath();
            if (key == Character.toLowerCase(e.getKeyChar())) {
                JPopupMenu popupMenu = ((JMenu) menuItem).getPopupMenu();
                ArrayList<MenuElement> newList = new ArrayList<>(Arrays.asList(path));
                newList.add(popupMenu);
                MenuElement[] subs = popupMenu.getSubElements();
                MenuElement sub = DarkUIUtil.findEnabledChild(subs, -1, true);
                if (sub != null) {
                    newList.add(sub);
                }
                MenuSelectionManager manager = e.getMenuSelectionManager();
                MenuElement[] newPath = new MenuElement[0];
                newPath = newList.toArray(newPath);
                manager.setSelectedPath(newPath);
                e.consume();
            }
        }

        public void menuKeyPressed(final MenuKeyEvent e) {
        }

        public void menuKeyReleased(final MenuKeyEvent e) {
        }
    }

    @NotNull
    protected static java.util.List<JPopupMenu> getPopups() {
        MenuSelectionManager msm = MenuSelectionManager.defaultManager();
        MenuElement[] p = msm.getSelectedPath();

        java.util.List<JPopupMenu> list = new ArrayList<JPopupMenu>(p.length);
        for (MenuElement element : p) {
            if (element instanceof JPopupMenu) {
                list.add((JPopupMenu) element);
            }
        }
        return list;
    }

    protected static JPopupMenu getLastPopup() {
        MenuSelectionManager msm = MenuSelectionManager.defaultManager();
        MenuElement[] p = msm.getSelectedPath();
        JPopupMenu popup = null;

        for (int i = p.length - 1; popup == null && i >= 0; i--) {
            if (p[i] instanceof JPopupMenu) { popup = (JPopupMenu) p[i]; }
        }
        return popup;
    }
}
