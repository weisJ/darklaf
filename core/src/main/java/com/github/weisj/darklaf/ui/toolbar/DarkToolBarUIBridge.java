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
package com.github.weisj.darklaf.ui.toolbar;

import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LazyActionMap;
import com.github.weisj.darklaf.util.PropertyKey;
import sun.swing.DefaultLookup;
import sun.swing.UIAction;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;
import javax.swing.event.MouseInputListener;
import javax.swing.plaf.ToolBarUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicBorders;
import javax.swing.plaf.basic.BasicToolBarUI;
import java.awt.*;
import java.awt.event.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.Objects;

/**
 * A Basic L&amp;F implementation of ToolBarUI.  This implementation is a "combined" view/controller.
 *
 * @author Georges Saab
 * @author Jeff Shapiro
 */
public abstract class DarkToolBarUIBridge extends ToolBarUI implements SwingConstants {
    // Rollover button implementation.
    protected static final String IS_ROLLOVER = "JToolBar.isRollover";
    protected static Border rolloverBorder;
    protected static Border nonRolloverBorder;
    protected static Border nonRolloverToggleBorder;
    protected static final String FOCUSED_COMP_INDEX = "JToolBar.focusedCompIndex";
    /**
     * The instance of {@code JToolBar}.
     */
    protected JToolBar toolBar;
    protected boolean floating;
    protected int floatingX;
    protected int floatingY;
    protected RootPaneContainer floatingToolBar;
    /**
     * The instance of {@code DragWindow}.
     */
    protected DragWindow dragWindow;
    protected Container dockingSource;
    protected int dockingSensitivity = 0;
    /**
     * The index of the focused component.
     */
    protected int focusedCompIndex = -1;
    /**
     * The background color of the docking border.
     */
    protected Color dockingColor = null;
    /**
     * The background color of the not docking border.
     */
    protected Color floatingColor = null;
    /**
     * The color of the docking border.
     */
    protected Color dockingBorderColor = null;
    /**
     * The color of the not docking border.
     */
    protected Color floatingBorderColor = null;
    /**
     * The instance of a {@code MouseInputListener}.
     */
    protected MouseInputListener dockingListener;
    /**
     * The instance of a {@code PropertyChangeListener}.
     */
    protected PropertyChangeListener propertyListener;
    /**
     * The instance of a {@code ContainerListener}.
     */
    protected ContainerListener toolBarContListener;
    /**
     * The instance of a {@code FocusListener}.
     */
    protected FocusListener toolBarFocusListener;
    protected Handler handler;
    /**
     * The layout before floating.
     */
    protected String constraintBeforeFloating = BorderLayout.NORTH;
    protected boolean rolloverBorders = false;
    protected final HashMap<AbstractButton, Border> borderTable = new HashMap<>();
    protected final Hashtable<AbstractButton, Boolean> rolloverTable = new Hashtable<>();
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are now defined by
     * the LookAndFeel, please refer to the key bindings specification for further details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke upKey;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are now defined by
     * the LookAndFeel, please refer to the key bindings specification for further details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke downKey;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are now defined by
     * the LookAndFeel, please refer to the key bindings specification for further details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke leftKey;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are now defined by
     * the LookAndFeel, please refer to the key bindings specification for further details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke rightKey;

    public static void loadActionMap(final LazyActionMap map) {
        map.put(new Actions(Actions.NAVIGATE_RIGHT));
        map.put(new Actions(Actions.NAVIGATE_LEFT));
        map.put(new Actions(Actions.NAVIGATE_UP));
        map.put(new Actions(Actions.NAVIGATE_DOWN));
    }

    public void installUI(final JComponent c) {
        toolBar = (JToolBar) c;

        // Set defaults
        installDefaults();
        installComponents();
        installListeners();
        installKeyboardActions();

        // Initialize instance vars
        dockingSensitivity = 0;
        floating = false;
        floatingX = floatingY = 0;
        floatingToolBar = null;

        setOrientation(toolBar.getOrientation());
        LookAndFeel.installProperty(c, PropertyKey.OPAQUE, Boolean.TRUE);

        if (c.getClientProperty(FOCUSED_COMP_INDEX) != null) {
            focusedCompIndex = (Integer) (c.getClientProperty(FOCUSED_COMP_INDEX));
        }
    }

    public void uninstallUI(final JComponent c) {

        // Clear defaults
        uninstallDefaults();
        uninstallComponents();
        uninstallListeners();
        uninstallKeyboardActions();

        // Clear instance vars
        if (isFloating()) {
            setFloating(false, null);
        }

        floatingToolBar = null;
        dragWindow = null;
        dockingSource = null;

        c.putClientProperty(FOCUSED_COMP_INDEX, focusedCompIndex);
    }

    /**
     * Uninstalls default properties.
     */
    protected void uninstallDefaults() {
        LookAndFeel.uninstallBorder(toolBar);
        dockingColor = null;
        floatingColor = null;
        dockingBorderColor = null;
        floatingBorderColor = null;

        installNormalBorders(toolBar);

        rolloverBorder = null;
        nonRolloverBorder = null;
        nonRolloverToggleBorder = null;
    }

    /**
     * Unregisters components.
     */
    protected void uninstallComponents() {
    }

    /**
     * Unregisters listeners.
     */
    protected void uninstallListeners() {
        if (dockingListener != null) {
            toolBar.removeMouseMotionListener(dockingListener);
            toolBar.removeMouseListener(dockingListener);

            dockingListener = null;
        }

        if (propertyListener != null) {
            toolBar.removePropertyChangeListener(propertyListener);
            propertyListener = null;  // removed in setFloating
        }

        if (toolBarContListener != null) {
            toolBar.removeContainerListener(toolBarContListener);
            toolBarContListener = null;
        }

        if (toolBarFocusListener != null) {
            // Remove focus listener from all components in toolbar
            Component[] components = toolBar.getComponents();

            for (Component component : components) {
                component.removeFocusListener(toolBarFocusListener);
            }

            toolBarFocusListener = null;
        }
        handler = null;
    }

    /**
     * Unregisters keyboard actions.
     */
    protected void uninstallKeyboardActions() {
        SwingUtilities.replaceUIActionMap(toolBar, null);
        SwingUtilities.replaceUIInputMap(toolBar, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
    }

    /**
     * Returns {@code true} if the {@code JToolBar} is floating
     *
     * @return {@code true} if the {@code JToolBar} is floating
     */
    public boolean isFloating() {
        return floating;
    }

    public abstract void setFloating(final boolean b, final Point p);

    /**
     * Installs normal borders on all the child components of the JComponent. A normal border is the original border
     * that was installed on the child component before it was added to the toolbar.
     * <p>
     * This is a convenience method to call <code>setBorderNormal</code> for each child component.
     *
     * @param c container which holds the child components (usually a JToolBar)
     * @see #setBorderToNonRollover
     * @since 1.4
     */
    protected void installNormalBorders(final JComponent c) {
        // Put back the normal borders on buttons
        Component[] components = c.getComponents();

        for (Component component : components) {
            setBorderToNormal(component);
        }
    }

    /**
     * Sets the border of the component to have a normal border. A normal border is the original border that was
     * installed on the child component before it was added to the toolbar.
     *
     * @param c component which will have a normal border re-installed
     * @see #createNonRolloverBorder
     * @since 1.4
     */
    protected void setBorderToNormal(final Component c) {
        if (c instanceof AbstractButton) {
            AbstractButton b = (AbstractButton) c;

            Border border = borderTable.remove(b);
            b.setBorder(border);

            Boolean value = rolloverTable.remove(b);
            if (value != null) {
                b.setRolloverEnabled(value);
            }
        }
    }

    /**
     * Installs default properties.
     */
    protected void installDefaults() {
        LookAndFeel.installBorder(toolBar, "ToolBar.border");
        LookAndFeel.installColorsAndFont(toolBar,
                                         "ToolBar.background",
                                         "ToolBar.foreground",
                                         "ToolBar.font");
        // Toolbar specific defaults
        if (dockingColor == null || dockingColor instanceof UIResource) {
            dockingColor = UIManager.getColor("ToolBar.dockingBackground");
        }
        if (floatingColor == null || floatingColor instanceof UIResource) {
            floatingColor = UIManager.getColor("ToolBar.floatingBackground");
        }
        if (dockingBorderColor == null ||
            dockingBorderColor instanceof UIResource) {
            dockingBorderColor = UIManager.getColor("ToolBar.dockingForeground");
        }
        if (floatingBorderColor == null ||
            floatingBorderColor instanceof UIResource) {
            floatingBorderColor = UIManager.getColor("ToolBar.floatingForeground");
        }

        // ToolBar rollover button borders
        Object rolloverProp = toolBar.getClientProperty(IS_ROLLOVER);
        if (rolloverProp == null) {
            rolloverProp = UIManager.get("ToolBar.isRollover");
        }
        if (rolloverProp != null) {
            rolloverBorders = (Boolean) rolloverProp;
        }

        if (rolloverBorder == null) {
            rolloverBorder = createRolloverBorder();
        }
        if (nonRolloverBorder == null) {
            nonRolloverBorder = createNonRolloverBorder();
        }
        if (nonRolloverToggleBorder == null) {
            nonRolloverToggleBorder = createNonRolloverToggleBorder();
        }


        setRolloverBorders(isRolloverBorders());
    }

    /**
     * Registers components.
     */
    protected void installComponents() {
    }

    /**
     * Registers listeners.
     */
    protected void installListeners() {
        dockingListener = createDockingListener();

        if (dockingListener != null) {
            toolBar.addMouseMotionListener(dockingListener);
            toolBar.addMouseListener(dockingListener);
        }

        propertyListener = createPropertyListener();  // added in setFloating
        if (propertyListener != null) {
            toolBar.addPropertyChangeListener(propertyListener);
        }

        toolBarContListener = createToolBarContListener();
        if (toolBarContListener != null) {
            toolBar.addContainerListener(toolBarContListener);
        }

        toolBarFocusListener = createToolBarFocusListener();

        if (toolBarFocusListener != null) {
            // Put focus listener on all components in toolbar
            Component[] components = toolBar.getComponents();

            for (Component component : components) {
                component.addFocusListener(toolBarFocusListener);
            }
        }
    }

    /**
     * Registers keyboard actions.
     */
    protected void installKeyboardActions() {
        InputMap km = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        SwingUtilities.replaceUIInputMap(toolBar, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, km);

        LazyActionMap.installLazyActionMap(toolBar, BasicToolBarUI.class, "ToolBar.actionMap");
    }

    /**
     * Sets the tool bar's orientation.
     *
     * @param orientation the new orientation
     */
    public void setOrientation(final int orientation) {
        toolBar.setOrientation(orientation);

        if (dragWindow != null) {
            dragWindow.setOrientation(orientation);
        }
    }

    /**
     * Creates a rollover border for toolbar components. The rollover border will be installed if rollover borders are
     * enabled.
     * <p>
     * Override this method to provide an alternate rollover border.
     *
     * @return a rollover border for toolbar components
     * @since 1.4
     */
    protected Border createRolloverBorder() {
        Object border = UIManager.get("ToolBar.rolloverBorder");
        if (border != null) {
            return (Border) border;
        }
        UIDefaults table = UIManager.getLookAndFeelDefaults();
        return new CompoundBorder(new BasicBorders.RolloverButtonBorder(
            table.getColor("controlShadow"),
            table.getColor("controlDkShadow"),
            table.getColor("controlHighlight"),
            table.getColor("controlLtHighlight")),
                                  new EmptyBorder(0, 0, 0, 0));
    }

    /**
     * Creates the non rollover border for toolbar components. This border will be installed as the border for
     * components added to the toolbar if rollover borders are not enabled.
     * <p>
     * Override this method to provide an alternate rollover border.
     *
     * @return the non rollover border for toolbar components
     * @since 1.4
     */
    protected Border createNonRolloverBorder() {
        Object border = UIManager.get("ToolBar.nonrolloverBorder");
        if (border != null) {
            return (Border) border;
        }
        UIDefaults table = UIManager.getLookAndFeelDefaults();
        return new CompoundBorder(new BasicBorders.ButtonBorder(
            table.getColor("Button.shadow"),
            table.getColor("Button.darkShadow"),
            table.getColor("Button.light"),
            table.getColor("Button.highlight")),
                                  new EmptyBorder(0, 0, 0, 0));
    }

    /**
     * Creates a non rollover border for Toggle buttons in the toolbar.
     *
     * @return the border
     */
    protected Border createNonRolloverToggleBorder() {
        UIDefaults table = UIManager.getLookAndFeelDefaults();
        return new CompoundBorder(new BasicBorders.RadioButtonBorder(
            table.getColor("ToggleButton.shadow"),
            table.getColor("ToggleButton.darkShadow"),
            table.getColor("ToggleButton.light"),
            table.getColor("ToggleButton.highlight")),
                                  new EmptyBorder(0, 0, 0, 0));
    }

    /**
     * Returns a flag to determine whether rollover button borders are enabled.
     *
     * @return true if rollover borders are enabled; false otherwise
     * @see #setRolloverBorders
     * @since 1.4
     */
    public boolean isRolloverBorders() {
        return rolloverBorders;
    }

    /**
     * Sets the flag for enabling rollover borders on the toolbar and it will also install the appropriate border
     * depending on the state of the flag.
     *
     * @param rollover if true, rollover borders are installed. Otherwise non-rollover borders are installed
     * @see #isRolloverBorders
     * @since 1.4
     */
    public void setRolloverBorders(final boolean rollover) {
        rolloverBorders = rollover;

        if (rolloverBorders) {
            installRolloverBorders(toolBar);
        } else {
            installNonRolloverBorders(toolBar);
        }
    }

    /**
     * Returns an instance of {@code MouseInputListener}.
     *
     * @return an instance of {@code MouseInputListener}
     */
    protected MouseInputListener createDockingListener() {
        getHandler().tb = toolBar;
        return getHandler();
    }

    /**
     * Returns an instance of {@code PropertyChangeListener}.
     *
     * @return an instance of {@code PropertyChangeListener}
     */
    protected PropertyChangeListener createPropertyListener() {
        return getHandler();
    }

    /**
     * Returns an instance of {@code ContainerListener}.
     *
     * @return an instance of {@code ContainerListener}
     */
    protected ContainerListener createToolBarContListener() {
        return getHandler();
    }

    /**
     * Returns an instance of {@code FocusListener}.
     *
     * @return an instance of {@code FocusListener}
     */
    protected FocusListener createToolBarFocusListener() {
        return getHandler();
    }

    InputMap getInputMap(final int condition) {
        if (condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) {
            return (InputMap) DefaultLookup.get(toolBar, this, "ToolBar.ancestorInputMap");
        }
        return null;
    }

    /**
     * Installs rollover borders on all the child components of the JComponent.
     * <p>
     * This is a convenience method to call <code>setBorderToRollover</code> for each child component.
     *
     * @param c container which holds the child components (usually a JToolBar)
     * @see #setBorderToRollover
     * @since 1.4
     */
    protected void installRolloverBorders(final JComponent c) {
        // Put rollover borders on buttons
        Component[] components = c.getComponents();

        for (Component component : components) {
            if (component instanceof JComponent) {
                ((JComponent) component).updateUI();
                setBorderToRollover(component);
            }
        }
    }

    /**
     * Installs non-rollover borders on all the child components of the JComponent. A non-rollover border is the border
     * that is installed on the child component while it is in the toolbar.
     * <p>
     * This is a convenience method to call <code>setBorderToNonRollover</code> for each child component.
     *
     * @param c container which holds the child components (usually a JToolBar)
     * @see #setBorderToNonRollover
     * @since 1.4
     */
    protected void installNonRolloverBorders(final JComponent c) {
        // Put non-rollover borders on buttons. These borders reduce the margin.
        Component[] components = c.getComponents();

        for (Component component : components) {
            if (component instanceof JComponent) {
                ((JComponent) component).updateUI();
                setBorderToNonRollover(component);
            }
        }
    }

    protected Handler getHandler() {
        if (handler == null) {
            handler = new Handler();
        }
        return handler;
    }

    /**
     * Sets the border of the component to have a rollover border which was created by the {@link #createRolloverBorder}
     * method.
     *
     * @param c component which will have a rollover border installed
     * @see #createRolloverBorder
     * @since 1.4
     */
    protected void setBorderToRollover(final Component c) {
        if (c instanceof AbstractButton) {
            AbstractButton b = (AbstractButton) c;

            Border border = borderTable.get(b);
            if (border == null || border instanceof UIResource) {
                borderTable.put(b, b.getBorder());
            }

            // Only set the border if its the default border
            if (b.getBorder() instanceof UIResource) {
                b.setBorder(getRolloverBorder(b));
            }

            rolloverTable.put(b, b.isRolloverEnabled() ?
                                 Boolean.TRUE : Boolean.FALSE);
            b.setRolloverEnabled(true);
        }
    }

    /**
     * Sets the border of the component to have a non-rollover border which was created by the {@link
     * #createNonRolloverBorder} method.
     *
     * @param c component which will have a non-rollover border installed
     * @see #createNonRolloverBorder
     * @since 1.4
     */
    protected void setBorderToNonRollover(final Component c) {
        if (c instanceof AbstractButton) {
            AbstractButton b = (AbstractButton) c;

            Border border = borderTable.get(b);
            if (border == null || border instanceof UIResource) {
                borderTable.put(b, b.getBorder());
            }

            // Only set the border if its the default border
            if (b.getBorder() instanceof UIResource) {
                b.setBorder(getNonRolloverBorder(b));
            }
            rolloverTable.put(b, b.isRolloverEnabled() ?
                                 Boolean.TRUE : Boolean.FALSE);
            b.setRolloverEnabled(false);
        }
    }

    /**
     * Returns a rollover border for the button.
     *
     * @param b the button to calculate the rollover border for
     * @return the rollover border
     * @see #setBorderToRollover
     * @since 1.6
     */
    protected Border getRolloverBorder(final AbstractButton b) {
        return rolloverBorder;
    }

    /**
     * Returns a non-rollover border for the button.
     *
     * @param b the button to calculate the non-rollover border for
     * @return the non-rollover border
     * @see #setBorderToNonRollover
     * @since 1.6
     */
    protected Border getNonRolloverBorder(final AbstractButton b) {
        if (b instanceof JToggleButton) {
            return nonRolloverToggleBorder;
        } else {
            return nonRolloverBorder;
        }
    }

    /**
     * Navigates the focused component.
     *
     * @param direction a direction
     */
    @SuppressWarnings("deprecation")
    protected void navigateFocusedComp(final int direction) {
        int nComp = toolBar.getComponentCount();
        int j;
        switch (direction) {
            case EAST:
            case SOUTH:
                if (focusedCompIndex < 0 || focusedCompIndex >= nComp) {
                    break;
                }
                j = focusedCompIndex + 1;
                while (j != focusedCompIndex) {
                    if (j >= nComp) {
                        j = 0;
                    }
                    Component comp = toolBar.getComponentAtIndex(j++);
                    if (comp != null && comp.isFocusTraversable() && comp.isEnabled()) {
                        comp.requestFocus();
                        break;
                    }
                }
                break;
            case WEST:
            case NORTH:
                if (focusedCompIndex < 0 || focusedCompIndex >= nComp) {
                    break;
                }
                j = focusedCompIndex - 1;
                while (j != focusedCompIndex) {
                    if (j < 0) {
                        j = nComp - 1;
                    }
                    Component comp = toolBar.getComponentAtIndex(j--);

                    if (comp != null && comp.isFocusTraversable() && comp.isEnabled()) {
                        comp.requestFocus();
                        break;
                    }
                }
                break;
            default:
                break;
        }
    }

    /**
     * Creates a window which contains the toolbar after it has been dragged out from its container
     *
     * @param toolbar an instance of {@code JToolBar}
     * @return a {@code RootPaneContainer} object, containing the toolbar
     * @since 1.4
     */
    protected RootPaneContainer createFloatingWindow(final JToolBar toolbar) {
        @SuppressWarnings("serial")
            // Superclass is not serializable across versions
        class ToolBarDialog extends JDialog {
            public ToolBarDialog(final Frame owner, final String title, final boolean modal) {
                super(owner, title, modal);
            }

            public ToolBarDialog(final Dialog owner, final String title, final boolean modal) {
                super(owner, title, modal);
            }

            // Override createRootPane() to automatically resize
            // the frame when contents change
            protected JRootPane createRootPane() {
                @SuppressWarnings("serial") // anonymous class
                    JRootPane rootPane = new JRootPane() {
                    protected boolean packing = false;

                    public void validate() {
                        super.validate();
                        if (!packing) {
                            packing = true;
                            pack();
                            packing = false;
                        }
                    }
                };
                rootPane.setOpaque(true);
                return rootPane;
            }
        }

        JDialog dialog;
        Window window = SwingUtilities.getWindowAncestor(toolbar);
        if (window instanceof Frame) {
            dialog = new ToolBarDialog((Frame) window, toolbar.getName(), false);
        } else if (window instanceof Dialog) {
            dialog = new ToolBarDialog((Dialog) window, toolbar.getName(), false);
        } else {
            dialog = new ToolBarDialog((Frame) null, toolbar.getName(), false);
        }

        dialog.getRootPane().setName("ToolBar.FloatingWindow");
        dialog.setTitle(toolbar.getName());
        dialog.setResizable(false);
        WindowListener wl = createFrameListener();
        dialog.addWindowListener(wl);
        return dialog;
    }

    /**
     * Constructs a new instance of {@code WindowListener}.
     *
     * @return a new instance of {@code WindowListener}
     */
    protected WindowListener createFrameListener() {
        return new FrameListener();
    }

    protected abstract DragWindow createDragWindow(final JToolBar toolbar);

    /**
     * Sets the floating location.
     *
     * @param x an X coordinate
     * @param y an Y coordinate
     */
    public void setFloatingLocation(final int x, final int y) {
        floatingX = x;
        floatingY = y;
    }

    protected int mapConstraintToOrientation(final String constraint) {
        int orientation = toolBar.getOrientation();

        if (constraint != null) {
            if (constraint.equals(BorderLayout.EAST) || constraint.equals(BorderLayout.WEST)) {
                orientation = JToolBar.VERTICAL;
            } else if (constraint.equals(BorderLayout.NORTH) || constraint.equals(BorderLayout.SOUTH)) {
                orientation = JToolBar.HORIZONTAL;
            }
        }

        return orientation;
    }

    /**
     * Gets the color displayed when over a docking area
     *
     * @return the color displayed when over a docking area
     */
    public Color getDockingColor() {
        return dockingColor;
    }

    /**
     * Sets the color displayed when over a docking area
     *
     * @param c the new color
     */
    public void setDockingColor(final Color c) {
        this.dockingColor = c;
    }

    /**
     * Gets the color displayed when over a floating area
     *
     * @return the color displayed when over a floating area
     */
    public Color getFloatingColor() {
        return floatingColor;
    }

    /**
     * Sets the color displayed when over a floating area
     *
     * @param c the new color
     */
    public void setFloatingColor(final Color c) {
        this.floatingColor = c;
    }

    protected abstract boolean isBlocked(final Component comp, final Object constraint);

    /**
     * Returns {@code true} if the {@code JToolBar} can dock at the given position.
     *
     * @param c a component
     * @param p a position
     * @return {@code true} if the {@code JToolBar} can dock at the given position
     */
    public boolean canDock(final Component c, final Point p) {
        return (p != null && getDockingConstraint(c, p) != null);
    }

    protected abstract String getDockingConstraint(final Component c, final Point p);

    protected String calculateConstraint() {
        String constraint = null;
        LayoutManager lm = dockingSource.getLayout();
        if (lm instanceof BorderLayout) {
            constraint = (String) ((BorderLayout) lm).getConstraints(toolBar);
        }
        return (constraint != null) ? constraint : constraintBeforeFloating;
    }

    /**
     * The method is used to drag {@code DragWindow} during the {@code JToolBar} is being dragged.
     */
    protected abstract void dragTo();

    /**
     * The method is called at end of dragging to place the frame in either its original place or in its floating
     * frame.
     */
    protected abstract void floatAt();

    /**
     * Paints the contents of the window used for dragging.
     *
     * @param g Graphics to paint to.
     * @throws NullPointerException is <code>g</code> is null
     * @since 1.5
     */
    protected void paintDragWindow(final Graphics g) {
        g.setColor(dragWindow.getBackground());
        int w = dragWindow.getWidth();
        int h = dragWindow.getHeight();
        g.fillRect(0, 0, w, h);
        g.setColor(dragWindow.getBorderColor());
        g.drawRect(0, 0, w - 1, h - 1);
    }


    protected static class Actions extends UIAction {
        protected static final String NAVIGATE_RIGHT = "navigateRight";
        protected static final String NAVIGATE_LEFT = "navigateLeft";
        protected static final String NAVIGATE_UP = "navigateUp";
        protected static final String NAVIGATE_DOWN = "navigateDown";

        public Actions(final String name) {
            super(name);
        }

        public void actionPerformed(final ActionEvent evt) {
            String key = getName();
            JToolBar toolBar = (JToolBar) evt.getSource();
            DarkToolBarUIBridge ui = (DarkToolBarUIBridge) DarkUIUtil.getUIOfType(toolBar.getUI(),
                                                                                  DarkToolBarUIBridge.class);

            if (ui == null) return;
            if (Objects.equals(NAVIGATE_RIGHT, key)) {
                ui.navigateFocusedComp(EAST);
            } else if (Objects.equals(NAVIGATE_LEFT, key)) {
                ui.navigateFocusedComp(WEST);
            } else if (Objects.equals(NAVIGATE_UP, key)) {
                ui.navigateFocusedComp(NORTH);
            } else if (Objects.equals(NAVIGATE_DOWN, key)) {
                ui.navigateFocusedComp(SOUTH);
            }
        }
    }


    protected class Handler implements ContainerListener,
                                       FocusListener, MouseInputListener, PropertyChangeListener {

        //
        // MouseInputListener (DockingListener)
        //
        JToolBar tb;
        boolean isDragging = false;

        //
        // ContainerListener
        //
        public void componentAdded(final ContainerEvent evt) {
            Component c = evt.getChild();

            if (toolBarFocusListener != null) {
                c.addFocusListener(toolBarFocusListener);
            }

            if (isRolloverBorders()) {
                setBorderToRollover(c);
            } else {
                setBorderToNonRollover(c);
            }
        }

        public void componentRemoved(final ContainerEvent evt) {
            Component c = evt.getChild();

            if (toolBarFocusListener != null) {
                c.removeFocusListener(toolBarFocusListener);
            }

            // Revert the button border
            setBorderToNormal(c);
        }

        //
        // FocusListener
        //
        public void focusGained(final FocusEvent evt) {
            Component c = evt.getComponent();
            focusedCompIndex = toolBar.getComponentIndex(c);
        }

        public void focusLost(final FocusEvent evt) {
        }

        public void mouseDragged(final MouseEvent evt) {
            if (!tb.isEnabled()) {
                return;
            }
            isDragging = true;
            dragTo();
        }

        public void mouseMoved(final MouseEvent evt) {
        }

        public void mouseClicked(final MouseEvent evt) {
        }

        public void mousePressed(final MouseEvent evt) {
            if (!tb.isEnabled()) {
                return;
            }
            isDragging = false;
        }

        public void mouseReleased(final MouseEvent evt) {
            if (!tb.isEnabled()) {
                return;
            }
            if (isDragging) {
                floatAt();
            }
            isDragging = false;
        }

        public void mouseEntered(final MouseEvent evt) {
        }

        public void mouseExited(final MouseEvent evt) {
        }

        //
        // PropertyChangeListener
        //
        public void propertyChange(final PropertyChangeEvent evt) {
            String propertyName = evt.getPropertyName();
            if (Objects.equals(propertyName, "lookAndFeel")) {
                toolBar.updateUI();
            } else if (Objects.equals(propertyName, PropertyKey.ORIENTATION)) {
                // Search for JSeparator components and change it's orientation
                // to match the toolbar and flip it's orientation.
                Component[] components = toolBar.getComponents();
                int orientation = (Integer) evt.getNewValue();
                JToolBar.Separator separator;

                for (Component component : components) {
                    if (component instanceof JToolBar.Separator) {
                        separator = (JToolBar.Separator) component;
                        if ((orientation == JToolBar.HORIZONTAL)) {
                            separator.setOrientation(JSeparator.VERTICAL);
                        } else {
                            separator.setOrientation(JSeparator.HORIZONTAL);
                        }
                        Dimension size = separator.getSeparatorSize();
                        if (size != null && size.width != size.height) {
                            // Flip the orientation.
                            Dimension newSize = new Dimension(size.height, size.width);
                            separator.setSeparatorSize(newSize);
                        }
                    }
                }
            } else if (Objects.equals(propertyName, IS_ROLLOVER)) {
                installNormalBorders(toolBar);
                setRolloverBorders((Boolean) evt.getNewValue());
            }
        }
    }

    /**
     * The class listens for window events.
     */
    protected class FrameListener extends WindowAdapter {
        public void windowClosing(final WindowEvent w) {
            if (toolBar.isFloatable()) {
                if (dragWindow != null) {
                    dragWindow.setVisible(false);
                }
                floating = false;
                if (floatingToolBar == null) {
                    floatingToolBar = createFloatingWindow(toolBar);
                }
                if (floatingToolBar instanceof Window) {
                    ((Window) floatingToolBar).setVisible(false);
                }
                floatingToolBar.getContentPane().remove(toolBar);
                String constraint = constraintBeforeFloating;
                if (toolBar.getOrientation() == JToolBar.HORIZONTAL) {
                    if (Objects.equals(constraint, "West") || Objects.equals(constraint, "East")) {
                        constraint = "North";
                    }
                } else {
                    if (Objects.equals(constraint, "North") || Objects.equals(constraint, "South")) {
                        constraint = "West";
                    }
                }
                if (dockingSource == null) {
                    dockingSource = toolBar.getParent();
                }
                if (propertyListener != null) {
                    UIManager.removePropertyChangeListener(propertyListener);
                }
                dockingSource.add(toolBar, constraint);
                dockingSource.invalidate();
                Container dockingSourceParent = dockingSource.getParent();
                if (dockingSourceParent != null) {
                    dockingSourceParent.validate();
                }
                dockingSource.repaint();
            }
        }

    }

    /**
     * The window which appears during dragging the {@code JToolBar}.
     */
    @SuppressWarnings("serial") // Same-version serialization only
    protected class DragWindow extends JWindow {
        Color borderColor = Color.gray;
        int orientation = toolBar.getOrientation();
        Point offset; // offset of the mouse cursor inside the DragWindow

        DragWindow(final Window w) {
            super(w);
        }

        /**
         * Sets the orientation.
         *
         * @param o the new orientation
         */
        public void setOrientation(final int o) {
            if (isShowing()) {
                if (o == this.orientation) {
                    return;
                }
                this.orientation = o;
                Dimension size = getSize();
                setSize(new Dimension(size.height, size.width));
                if (offset != null) {
                    if (toolBar.getComponentOrientation().isLeftToRight()) {
                        setOffset(new Point(offset.y, offset.x));
                    } else if (o == JToolBar.HORIZONTAL) {
                        setOffset(new Point(size.height - offset.y, offset.x));
                    } else {
                        setOffset(new Point(offset.y, size.width - offset.x));
                    }
                }
                repaint();
            }
        }

        /**
         * Returns the offset.
         *
         * @return the offset
         */
        public Point getOffset() {
            return offset;
        }

        /**
         * Sets the offset.
         *
         * @param p the new offset
         */
        public void setOffset(final Point p) {
            this.offset = p;
        }

        /**
         * Returns the border color.
         *
         * @return the border color
         */
        public Color getBorderColor() {
            return this.borderColor;
        }

        /**
         * Sets the border color.
         *
         * @param c the new border color
         */
        public void setBorderColor(final Color c) {
            if (this.borderColor == c) {
                return;
            }
            this.borderColor = c;
            repaint();
        }

        public void paint(final Graphics g) {
            paintDragWindow(g);
            // Paint the children
            super.paint(g);
        }

        public Insets getInsets() {
            return new Insets(1, 1, 1, 1);
        }
    }
}
