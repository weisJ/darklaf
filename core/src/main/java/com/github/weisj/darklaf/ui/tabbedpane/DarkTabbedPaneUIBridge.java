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
package com.github.weisj.darklaf.ui.tabbedpane;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.FocusListener;
import java.awt.event.InputEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyChangeListener;
import java.util.Hashtable;
import java.util.Objects;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.ChangeListener;
import javax.swing.plaf.ComponentInputMapUIResource;
import javax.swing.plaf.TabbedPaneUI;
import javax.swing.plaf.UIResource;
import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.View;

import com.github.weisj.darklaf.ui.UIAction;
import com.github.weisj.darklaf.util.DarkUIUtil;
import com.github.weisj.darklaf.util.LazyActionMap;
import com.github.weisj.darklaf.util.PropertyKey;
import com.github.weisj.darklaf.util.SwingUtil;

/**
 * A Basic L&amp;F implementation of TabbedPaneUI.
 *
 * @author Amy Fowler
 * @author Philip Milne
 * @author Steve Wilson
 * @author Tom Santos
 * @author Dave Moore
 *         <p>
 *         This class stands in to allow to access the hell of ui code produced in
 *         {@link javax.swing.plaf.basic.BasicTabbedPaneUI}. This code is almost identical to the
 *         original implementation besides the fact that all fields and methods are now protected
 *         instead of private.
 * @author Jannis Weis
 */
public abstract class DarkTabbedPaneUIBridge extends TabbedPaneUI implements SwingConstants {

    // Instance variables initialized at installation

    /** The Current pad insets. */
    protected final Insets currentPadInsets = new Insets(0, 0, 0, 0);
    /** The Current tab area insets. */
    protected final Insets currentTabAreaInsets = new Insets(0, 0, 0, 0);
    /** The tab pane */
    protected JTabbedPane tabPane;
    /** Text icon gap */
    protected int textIconGap;
    /** Tab run overlay */
    protected int tabRunOverlay;
    /** Tab insets */
    protected Insets tabInsets;
    /** Selected tab insets */
    protected Insets selectedTabPadInsets;
    /** Tab area insets */
    protected Insets tabAreaInsets;
    /** Content border insets */
    protected Insets contentBorderInsets;
    /** The Tabs overlap border. */
    protected boolean tabsOverlapBorder;

    /** The Tabs opaque. */
    // Transient variables (recalculated each time TabbedPane is layed out)
    protected boolean tabsOpaque = true;
    /** The Content opaque. */
    protected boolean contentOpaque = true;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are
     * now defined by the LookAndFeel, please refer to the key bindings specification for further
     * details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke upKey;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are
     * now defined by the LookAndFeel, please refer to the key bindings specification for further
     * details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke downKey;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are
     * now defined by the LookAndFeel, please refer to the key bindings specification for further
     * details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke leftKey;
    /**
     * As of Java 2 platform v1.3 this previously undocumented field is no longer used. Key bindings are
     * now defined by the LookAndFeel, please refer to the key bindings specification for further
     * details.
     *
     * @deprecated As of Java 2 platform v1.3.
     */
    @Deprecated
    protected KeyStroke rightKey;

    // Listeners
    /** Tab runs */
    protected int[] tabRuns = new int[10];
    /** Run count */
    protected int runCount = 0;
    /** Selected run */
    protected int selectedRun = -1;
    /** Tab rects */
    protected Rectangle[] rects = new Rectangle[0];

    // Private instance data
    /** Maximum tab height */
    protected int maxTabHeight;
    /** Maximum tab width */
    protected int maxTabWidth;
    /** Tab change listener */
    protected ChangeListener tabChangeListener;
    /** Property change listener */
    protected PropertyChangeListener propertyChangeListener;
    /** Mouse change listener */
    protected MouseListener mouseListener;
    /** Focus change listener */
    protected FocusListener focusListener;
    /** The Visible component. */
    protected Component visibleComponent;
    /** The Html views. */
    // PENDING(api): See comment for ContainerHandler
    protected Vector<View> htmlViews;
    /** The Mnemonic to index map. */
    protected Hashtable<Integer, Integer> mnemonicToIndexMap;
    /**
     * InputMap used for mnemonics. Only non-null if the JTabbedPane has mnemonics associated with it.
     * Lazily created in initMnemonics.
     */
    protected InputMap mnemonicInputMap;
    /** The Tab scroller. */
    // For use when tabLayoutPolicy = SCROLL_TAB_LAYOUT
    protected ScrollableTabSupport tabScroller;
    /** The Tab container. */
    protected TabContainer tabContainer;
    /** Tab that has focus. */
    protected int focusIndex;
    /** Combined listeners. */
    protected TabbedPaneHandler handler;
    /** Index of the tab the mouse is over. */
    protected int rolloverTabIndex;

    // UI creation
    /**
     * This is set to true when a component is added/removed from the tab pane and set to false when
     * layout happens. If true it indicates that tabRuns is not valid and shouldn't be used.
     */
    protected boolean isRunsDirty;
    /** The Calculated baseline. */
    protected boolean calculatedBaseline;

    /** The Selected foreground. */
    protected Color selectedForeground;

    /** The Baseline. */
    // UI Installation/De-installation
    protected int baseline;

    /**
     * Load action map.
     *
     * @param map the map
     */
    public static void loadActionMap(final LazyActionMap map) {
        map.put(new Actions(Actions.NEXT));
        map.put(new Actions(Actions.PREVIOUS));
        map.put(new Actions(Actions.RIGHT));
        map.put(new Actions(Actions.LEFT));
        map.put(new Actions(Actions.UP));
        map.put(new Actions(Actions.DOWN));
        map.put(new Actions(Actions.PAGE_UP));
        map.put(new Actions(Actions.PAGE_DOWN));
        map.put(new Actions(Actions.REQUEST_FOCUS));
        map.put(new Actions(Actions.REQUEST_FOCUS_FOR_VISIBLE));
        map.put(new Actions(Actions.SET_SELECTED));
        map.put(new Actions(Actions.SELECT_FOCUSED));
        map.put(new Actions(Actions.SCROLL_FORWARD));
        map.put(new Actions(Actions.SCROLL_BACKWARD));
    }

    public void installUI(final JComponent c) {
        this.tabPane = (JTabbedPane) c;

        calculatedBaseline = false;
        rolloverTabIndex = -1;
        focusIndex = -1;

        c.setLayout(createLayoutManager());
        installComponents();
        installDefaults();
        installListeners();
        installKeyboardActions();
    }

    public void uninstallUI(final JComponent c) {
        uninstallKeyboardActions();
        uninstallListeners();
        uninstallDefaults();
        uninstallComponents();
        c.setLayout(null);

        this.tabPane = null;
    }

    /** Uninstalls the keyboard actions. */
    protected void uninstallKeyboardActions() {
        SwingUtilities.replaceUIActionMap(tabPane, null);
        SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, null);
        SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_FOCUSED, null);
        SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_IN_FOCUSED_WINDOW, null);
        mnemonicToIndexMap = null;
        mnemonicInputMap = null;
    }

    /** Uninstall the listeners. */
    protected void uninstallListeners() {
        if (mouseListener != null) {
            tabPane.removeMouseListener(mouseListener);
            mouseListener = null;
        }
        tabPane.removeMouseMotionListener(getHandler());
        if (focusListener != null) {
            tabPane.removeFocusListener(focusListener);
            focusListener = null;
        }

        tabPane.removeContainerListener(getHandler());
        if (htmlViews != null) {
            htmlViews.removeAllElements();
            htmlViews = null;
        }
        if (tabChangeListener != null) {
            tabPane.removeChangeListener(tabChangeListener);
            tabChangeListener = null;
        }
        if (propertyChangeListener != null) {
            tabPane.removePropertyChangeListener(propertyChangeListener);
            propertyChangeListener = null;
        }
        handler = null;
    }

    /** Uninstall the defaults. */
    protected void uninstallDefaults() {
        tabInsets = null;
        selectedTabPadInsets = null;
        tabAreaInsets = null;
        contentBorderInsets = null;
    }

    /**
     * Removes any installed subcomponents from the JTabbedPane. Invoked by uninstallUI.
     *
     * @since 1.4
     */
    protected void uninstallComponents() {
        uninstallTabContainer();
        if (scrollableTabLayoutEnabled()) {
            tabPane.remove(tabScroller.viewport);
            tabScroller = null;
        }
    }

    /** Uninstall tab container. */
    protected void uninstallTabContainer() {
        if (tabContainer == null) {
            return;
        }
        // Remove all the tabComponents, making sure not to notify
        // the tabbedpane.
        tabContainer.notifyTabbedPane = false;
        tabContainer.removeAll();
        if (scrollableTabLayoutEnabled()) {
            tabScroller.tabPanel.remove(tabContainer);
        } else {
            tabPane.remove(tabContainer);
        }
        tabContainer = null;
    }

    public abstract void paint(final Graphics g, final JComponent c);

    public Dimension getMinimumSize(final JComponent c) {
        // Default to LayoutManager's minimumLayoutSize
        return null;
    }

    public Dimension getMaximumSize(final JComponent c) {
        // Default to LayoutManager's maximumLayoutSize
        return null;
    }

    /**
     * Returns the baseline.
     *
     * @throws NullPointerException {@inheritDoc}
     * @throws IllegalArgumentException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public int getBaseline(final JComponent c, final int width, final int height) {
        super.getBaseline(c, width, height);
        int baseline = calculateBaselineIfNecessary();
        if (baseline != -1) {
            int placement = tabPane.getTabPlacement();
            Insets insets = tabPane.getInsets();
            Insets tabAreaInsets = getTabAreaInsets(placement);
            switch (placement) {
                case JTabbedPane.TOP:
                case JTabbedPane.LEFT:
                case JTabbedPane.RIGHT:
                    baseline += insets.top + tabAreaInsets.top;
                    return baseline;
                case JTabbedPane.BOTTOM:
                    baseline = height - insets.bottom - tabAreaInsets.bottom - maxTabHeight + baseline;
                    return baseline;
            }
        }
        return -1;
    }

    /**
     * Returns an enum indicating how the baseline of the component changes as the size changes.
     *
     * @throws NullPointerException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(final JComponent c) {
        super.getBaselineResizeBehavior(c);
        switch (tabPane.getTabPlacement()) {
            case JTabbedPane.LEFT:
            case JTabbedPane.RIGHT:
            case JTabbedPane.TOP:
                return Component.BaselineResizeBehavior.CONSTANT_ASCENT;
            case JTabbedPane.BOTTOM:
                return Component.BaselineResizeBehavior.CONSTANT_DESCENT;
        }
        return Component.BaselineResizeBehavior.OTHER;
    }

    /**
     * Scrollable tab layout enabled boolean.
     *
     * @return the boolean
     */
    /*
     * In an attempt to preserve backward compatibility for programs which have extended
     * BasicTabbedPaneUI to do their own layout, the UI uses the installed layoutManager (and not
     * tabLayoutPolicy) to determine if scrollTabLayout is enabled.
     */
    protected boolean scrollableTabLayoutEnabled() {
        return (tabPane.getLayout() instanceof TabbedPaneScrollLayout);
    }

    /**
     * Paints the content border.
     *
     * @param g the graphics context in which to paint
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param selectedIndex the tab index of the selected component
     */
    protected abstract void paintContentBorder(final Graphics g, final int tabPlacement, final int selectedIndex);

    /**
     * Paints the tabs in the tab area. Invoked by paint(). The graphics parameter must be a valid
     * <code>Graphics</code> object. Tab placement may be either: <code>JTabbedPane.TOP</code>, <code>
     * JTabbedPane.BOTTOM</code>, <code>JTabbedPane.LEFT</code>, or <code>JTabbedPane.RIGHT</code>. The
     * selected index must be a valid tabbed pane tab index (0 to tab count - 1, inclusive) or -1 if no
     * tab is currently selected. The handling of invalid parameters is unspecified.
     *
     * @param g the graphics object to use for rendering
     * @param tabPlacement the placement for the tabs within the JTabbedPane
     * @param selectedIndex the tab index of the selected component
     * @since 1.4
     */
    protected void paintTabArea(final Graphics g, final int tabPlacement, final int selectedIndex) {
        int tabCount = tabPane.getTabCount();

        Rectangle iconRect = new Rectangle(), textRect = new Rectangle();
        Rectangle clipRect = g.getClipBounds();

        // Paint tabRuns of tabs from back to front
        for (int i = runCount - 1; i >= 0; i--) {
            int start = tabRuns[i];
            int next = tabRuns[(i == runCount - 1) ? 0 : i + 1];
            int end = (next != 0 ? next - 1 : tabCount - 1);
            for (int j = start; j <= end; j++) {
                if (j != selectedIndex && rects[j].intersects(clipRect)) {
                    paintTab(g, tabPlacement, rects, j, iconRect, textRect);
                }
            }
        }

        // Paint selected tab if its in the front run
        // since it may overlap other tabs
        if (selectedIndex >= 0 && rects[selectedIndex].intersects(clipRect)) {
            paintTab(g, tabPlacement, rects, selectedIndex, iconRect, textRect);
        }
    }

    /**
     * Paint tab.
     *
     * @param g the g
     * @param tabPlacement the tab placement
     * @param rects the rects
     * @param tabIndex the tab index
     * @param iconRect the icon rect
     * @param textRect the text rect
     */
    protected void paintTab(final Graphics g, final int tabPlacement, final Rectangle[] rects, final int tabIndex,
            final Rectangle iconRect, final Rectangle textRect) {
        paintTab(g, tabPlacement, rects[tabIndex], tabIndex, iconRect, textRect);
    }

    /**
     * Paints a tab.
     *
     * @param g the graphics
     * @param tabPlacement the tab placement
     * @param tabRect the tab rectangle
     * @param tabIndex the tab index
     * @param iconRect the icon rectangle
     * @param textRect the text rectangle
     */
    protected void paintTab(final Graphics g, final int tabPlacement, final Rectangle tabRect, final int tabIndex,
            final Rectangle iconRect, final Rectangle textRect) {
        int selectedIndex = tabPane.getSelectedIndex();
        boolean isSelected = selectedIndex == tabIndex;

        if (tabsOpaque || tabPane.isOpaque()) {
            paintTabBackground(g, tabPlacement, tabIndex, tabRect.x, tabRect.y, tabRect.width, tabRect.height,
                    isSelected);
        }

        paintTabBorder(g, tabPlacement, tabIndex, tabRect.x, tabRect.y, tabRect.width, tabRect.height, isSelected);

        String title = tabPane.getTitleAt(tabIndex);
        Font font = tabPane.getFont();
        FontMetrics metrics = SwingUtil.getFontMetrics(tabPane, font);
        Icon icon = getIconForTab(tabIndex);

        layoutLabel(tabPlacement, metrics, tabIndex, title, icon, tabRect, iconRect, textRect, isSelected);

        if (tabPane.getTabComponentAt(tabIndex) == null) {
            String clippedTitle = title;

            if (!scrollableTabLayoutEnabled() && isHorizontalTabPlacement()) {
                clippedTitle = SwingUtil.clipStringIfNecessary(null, metrics, title, textRect.width);
            }

            paintText(g, tabPlacement, font, metrics, tabIndex, clippedTitle, textRect, isSelected);

            paintIcon(g, tabPlacement, tabIndex, icon, iconRect, isSelected);
        }
        paintFocusIndicator(g, tabPlacement, tabRect, tabIndex, iconRect, textRect, isSelected);
    }

    /**
     * Paints the tab background.
     *
     * @param g the graphics context in which to paint
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabIndex the index of the tab with respect to other tabs
     * @param x the x coordinate of tab
     * @param y the y coordinate of tab
     * @param w the width of the tab
     * @param h the height of the tab
     * @param isSelected a {@code boolean} which determines whether or not the tab is selected
     */
    protected abstract void paintTabBackground(final Graphics g, final int tabPlacement, final int tabIndex,
            final int x, final int y, final int w, final int h, final boolean isSelected);

    /**
     * this function draws the border around each tab note that this function does now draw the
     * background of the tab. that is done elsewhere
     *
     * @param g the graphics context in which to paint
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabIndex the index of the tab with respect to other tabs
     * @param x the x coordinate of tab
     * @param y the y coordinate of tab
     * @param w the width of the tab
     * @param h the height of the tab
     * @param isSelected a {@code boolean} which determines whether or not the tab is selected
     */
    protected abstract void paintTabBorder(final Graphics g, final int tabPlacement, final int tabIndex, final int x,
            final int y, final int w, final int h, final boolean isSelected);

    /**
     * Returns the icon for a tab.
     *
     * @param tabIndex the index of the tab
     * @return the icon for a tab
     */
    protected Icon getIconForTab(final int tabIndex) {
        return (!tabPane.isEnabled() || !tabPane.isEnabledAt(tabIndex)) ? tabPane.getDisabledIconAt(tabIndex)
                : tabPane.getIconAt(tabIndex);
    }

    /**
     * Laysout a label.properties.
     *
     * @param tabPlacement the tab placement
     * @param metrics the font metric
     * @param tabIndex the tab index
     * @param title the title
     * @param icon the icon
     * @param tabRect the tab rectangle
     * @param iconRect the icon rectangle
     * @param textRect the text rectangle
     * @param isSelected selection status
     */
    protected void layoutLabel(final int tabPlacement, final FontMetrics metrics, final int tabIndex,
            final String title, final Icon icon, final Rectangle tabRect, final Rectangle iconRect,
            final Rectangle textRect, final boolean isSelected) {
        textRect.x = textRect.y = iconRect.x = iconRect.y = 0;

        View v = getTextViewForTab(tabIndex);
        if (v != null) {
            tabPane.putClientProperty(PropertyKey.HTML, v);
        }

        SwingUtilities.layoutCompoundLabel(tabPane, metrics, title, icon, SwingUtilities.CENTER, SwingUtilities.CENTER,
                SwingUtilities.CENTER, SwingUtilities.TRAILING, tabRect, iconRect, textRect, textIconGap);

        tabPane.putClientProperty(PropertyKey.HTML, null);

        int xNudge = getTabLabelShiftX(tabPlacement, tabIndex, isSelected);
        int yNudge = getTabLabelShiftY(tabPlacement, tabIndex, isSelected);
        iconRect.x += xNudge;
        iconRect.y += yNudge;
        textRect.x += xNudge;
        textRect.y += yNudge;
    }

    /**
     * Invoked by <code>installUI</code> to create a layout manager object to manage the <code>
     * JTabbedPane</code>.
     *
     * @return a layout manager object
     * @see TabbedPaneLayout
     * @see javax.swing.JTabbedPane#getTabLayoutPolicy javax.swing.JTabbedPane#getTabLayoutPolicy
     */
    protected abstract LayoutManager createLayoutManager();

    /**
     * Paints text.
     *
     * @param g the graphics
     * @param tabPlacement the tab placement
     * @param font the font
     * @param metrics the font metrics
     * @param tabIndex the tab index
     * @param title the title
     * @param textRect the text rectangle
     * @param isSelected selection status
     */
    protected abstract void paintText(final Graphics g, final int tabPlacement, final Font font,
            final FontMetrics metrics, final int tabIndex, final String title, final Rectangle textRect,
            final boolean isSelected);

    /**
     * Paints an icon.
     *
     * @param g the graphics
     * @param tabPlacement the tab placement
     * @param tabIndex the tab index
     * @param icon the icon
     * @param iconRect the icon rectangle
     * @param isSelected selection status
     */
    protected void paintIcon(final Graphics g, final int tabPlacement, final int tabIndex, final Icon icon,
            final Rectangle iconRect, final boolean isSelected) {
        if (icon != null) {
            // Clip the icon within iconRect bounds
            Shape oldClip = g.getClip();
            ((Graphics2D) g).clip(iconRect);
            icon.paintIcon(tabPane, g, iconRect.x, iconRect.y);
            g.setClip(oldClip);
        }
    }

    /**
     * Paints the focus indicator.
     *
     * @param g the graphics
     * @param tabPlacement the tab placement
     * @param tabRect the tabRect
     * @param tabIndex the tab index
     * @param iconRect the icon rectangle
     * @param textRect the text rectangle
     * @param isSelected selection status
     */
    protected abstract void paintFocusIndicator(final Graphics g, final int tabPlacement, final Rectangle tabRect,
            final int tabIndex, final Rectangle iconRect, final Rectangle textRect, final boolean isSelected);

    /**
     * Returns the text View object required to render stylized text (HTML) for the specified tab or
     * null if no specialized text rendering is needed for this tab. This is provided to support html
     * rendering inside tabs.
     *
     * @param tabIndex the index of the tab
     * @return the text view to render the tab's text or null if no specialized rendering is required
     * @since 1.4
     */
    protected View getTextViewForTab(final int tabIndex) {
        if (htmlViews != null) {
            return htmlViews.elementAt(tabIndex);
        }
        return null;
    }

    /**
     * Returns the tab label.properties shift x.
     *
     * @param tabPlacement the tab placement
     * @param tabIndex the tab index
     * @param isSelected selection status
     * @return the tab label.properties shift x
     */
    protected int getTabLabelShiftX(final int tabPlacement, final int tabIndex, final boolean isSelected) {
        Rectangle tabRect = rects[tabIndex];
        String propKey = (isSelected ? "selectedLabelShift" : "labelShift");
        int nudge = UIManager.getInt("TabbedPane." + propKey);

        switch (tabPlacement) {
            case LEFT:
                return nudge;
            case RIGHT:
                return -nudge;
            case BOTTOM:
            case TOP:
            default:
                return tabRect.width % 2;
        }
    }

    /**
     * Returns the tab label.properties shift y.
     *
     * @param tabPlacement the tab placement
     * @param tabIndex the tab index
     * @param isSelected selection status
     * @return the tab label.properties shift y
     */
    protected int getTabLabelShiftY(final int tabPlacement, final int tabIndex, final boolean isSelected) {
        Rectangle tabRect = rects[tabIndex];
        int nudge = (isSelected ? UIManager.getInt("TabbedPane.selectedLabelShift")
                : UIManager.getInt("TabbedPane.labelShift"));

        switch (tabPlacement) {
            case BOTTOM:
                return -nudge;
            case LEFT:
            case RIGHT:
                return tabRect.height % 2;
            case TOP:
            default:
                return nudge;
        }
    }

    /** Install tab container. */
    protected void installTabContainer() {
        for (int i = 0; i < tabPane.getTabCount(); i++) {
            Component tabComponent = tabPane.getTabComponentAt(i);
            if (tabComponent != null) {
                if (tabContainer == null) {
                    tabContainer = new TabContainer();
                }
                tabContainer.add(tabComponent);
            }
        }
        if (tabContainer == null) {
            return;
        }
        if (scrollableTabLayoutEnabled()) {
            tabScroller.tabPanel.add(tabContainer);
        } else {
            tabPane.add(tabContainer);
        }
    }

    /**
     * Creates and installs any required subcomponents for the JTabbedPane. Invoked by installUI.
     *
     * @since 1.4
     */
    protected abstract void installComponents();

    /** Install the defaults. */
    protected void installDefaults() {
        LookAndFeel.installColorsAndFont(tabPane, "TabbedPane.background", "TabbedPane.foreground", "TabbedPane.font");
        textIconGap = UIManager.getInt("TabbedPane.textIconGap");
        tabInsets = UIManager.getInsets("TabbedPane.tabInsets");
        selectedTabPadInsets = UIManager.getInsets("TabbedPane.selectedTabPadInsets");
        tabAreaInsets = UIManager.getInsets("TabbedPane.tabAreaInsets");
        tabsOverlapBorder = UIManager.getBoolean("TabbedPane.tabsOverlapBorder");
        contentBorderInsets = UIManager.getInsets("TabbedPane.contentBorderInsets");
        tabRunOverlay = UIManager.getInt("TabbedPane.tabRunOverlay");
        tabsOpaque = UIManager.getBoolean("TabbedPane.tabsOpaque");
        contentOpaque = UIManager.getBoolean("TabbedPane.contentOpaque");
        selectedForeground = UIManager.getColor("TabbedPane.selectedForeground");
        Object opaque = UIManager.get("TabbedPane.opaque");
        if (opaque == null) {
            opaque = Boolean.FALSE;
        }
        LookAndFeel.installProperty(tabPane, PropertyKey.OPAQUE, opaque);

        // Fix for 6711145 BasicTabbedPanuUI should not throw a NPE if these
        // keys are missing. So we are setting them to there default values here
        // if the keys are missing.
        if (tabInsets == null) tabInsets = new Insets(0, 4, 1, 4);
        if (selectedTabPadInsets == null) selectedTabPadInsets = new Insets(2, 2, 2, 1);
        if (tabAreaInsets == null) tabAreaInsets = new Insets(3, 2, 0, 2);
        if (contentBorderInsets == null) contentBorderInsets = new Insets(2, 2, 3, 3);
    }

    // UI Rendering

    /** Install the listeners. */
    protected void installListeners() {
        if ((propertyChangeListener = createPropertyChangeListener()) != null) {
            tabPane.addPropertyChangeListener(propertyChangeListener);
        }
        if ((tabChangeListener = createChangeListener()) != null) {
            tabPane.addChangeListener(tabChangeListener);
        }
        if ((mouseListener = createMouseListener()) != null) {
            tabPane.addMouseListener(mouseListener);
        }
        tabPane.addMouseMotionListener(getHandler());
        if ((focusListener = createFocusListener()) != null) {
            tabPane.addFocusListener(focusListener);
        }
        tabPane.addContainerListener(getHandler());
        if (tabPane.getTabCount() > 0) {
            htmlViews = createHTMLVector();
        }
    }

    /** Installs the keyboard actions. */
    protected void installKeyboardActions() {
        InputMap km = getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT);

        SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT, km);
        km = getInputMap(JComponent.WHEN_FOCUSED);
        SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_FOCUSED, km);

        LazyActionMap.installLazyActionMap(tabPane, DarkTabbedPaneUIBridge.class, "TabbedPane.actionMap");
        updateMnemonics();
    }

    /**
     * Gets handler.
     *
     * @return the handler
     */
    protected TabbedPaneHandler getHandler() {
        if (handler == null) {
            handler = new TabbedPaneHandler(this);
        }
        return handler;
    }

    /**
     * Create html vector vector.
     *
     * @return the vector
     */
    protected Vector<View> createHTMLVector() {
        Vector<View> htmlViews = new Vector<>();
        int count = tabPane.getTabCount();
        if (count > 0) {
            for (int i = 0; i < count; i++) {
                String title = tabPane.getTitleAt(i);
                if (BasicHTML.isHTMLString(title)) {
                    htmlViews.addElement(BasicHTML.createHTMLView(tabPane, title));
                } else {
                    htmlViews.addElement(null);
                }
            }
        }
        return htmlViews;
    }

    /**
     * Creates a property change listener.
     *
     * @return a property change listener
     */
    protected PropertyChangeListener createPropertyChangeListener() {
        return getHandler();
    }

    /**
     * Creates a change listener.
     *
     * @return a change listener
     */
    protected ChangeListener createChangeListener() {
        return getHandler();
    }

    /**
     * Creates a mouse listener.
     *
     * @return a mouse listener
     */
    protected MouseListener createMouseListener() {
        return getHandler();
    }

    /**
     * Gets input map.
     *
     * @param condition the condition
     * @return the input map
     */
    InputMap getInputMap(final int condition) {
        if (condition == JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT) {
            return (InputMap) UIManager.get("TabbedPane.ancestorInputMap", tabPane.getLocale());
        } else if (condition == JComponent.WHEN_FOCUSED) {
            return (InputMap) UIManager.get("TabbedPane.focusInputMap", tabPane.getLocale());
        }
        return null;
    }

    /**
     * Creates a focus listener.
     *
     * @return a focus listener
     */
    protected FocusListener createFocusListener() {
        return getHandler();
    }

    /**
     * Adds the specified mnemonic at the specified index.
     *
     * @param index the index
     * @param mnemonic the mnemonic
     */
    @SuppressWarnings("MagicConstant")
    protected void addMnemonic(final int index, final int mnemonic) {
        if (mnemonicToIndexMap == null) {
            initMnemonics();
        }
        mnemonicInputMap.put(KeyStroke.getKeyStroke(mnemonic, SwingUtil.getFocusAcceleratorKeyMask()),
                "setSelectedIndex");
        mnemonicInputMap.put(KeyStroke.getKeyStroke(mnemonic,
                InputEvent.ALT_GRAPH_DOWN_MASK | SwingUtil.getFocusAcceleratorKeyMask()), "setSelectedIndex");
        mnemonicToIndexMap.put(mnemonic, index);
    }

    /**
     * Sets the tab the mouse is over by location. This is a cover method for <code>
     * setRolloverTab(tabForCoordinate(x,
     * y, false))</code>.
     *
     * @param x the x
     * @param y the y
     */
    protected void setRolloverTab(final int x, final int y) {
        // NOTE:
        // This calls in with false otherwise it could trigger a validate,
        // which should NOT happen if the user is only dragging the
        // mouse around.
        setRolloverTab(tabForCoordinate(tabPane, x, y, false));
    }

    /**
     * Reloads the mnemonics. This should be invoked when a memonic changes, when the title of a
     * mnemonic changes, or when tabs are added/removed.
     */
    protected void updateMnemonics() {
        resetMnemonics();
        for (int counter = tabPane.getTabCount() - 1; counter >= 0; counter--) {
            int mnemonic = tabPane.getMnemonicAt(counter);

            if (mnemonic > 0) {
                addMnemonic(counter, mnemonic);
            }
        }
    }

    /** Resets the mnemonics bindings to an empty state. */
    protected void resetMnemonics() {
        if (mnemonicToIndexMap != null) {
            mnemonicToIndexMap.clear();
            mnemonicInputMap.clear();
        }
    }

    /**
     * Tab for coordinate int.
     *
     * @param pane the pane
     * @param x the x
     * @param y the y
     * @param validateIfNecessary the validate if necessary
     * @return the int
     */
    protected int tabForCoordinate(final JTabbedPane pane, final int x, final int y,
            final boolean validateIfNecessary) {
        if (validateIfNecessary) {
            ensureCurrentLayout();
        }
        if (isRunsDirty) {
            // We didn't recalculate the layout, runs and tabCount may not
            // line up, bail.
            return -1;
        }
        Point p = new Point(x, y);

        if (scrollableTabLayoutEnabled()) {
            translatePointToTabPanel(x, y, p);
            Rectangle viewRect = tabScroller.viewport.getViewRect();
            if (!viewRect.contains(p)) {
                return -1;
            }
        }
        int tabCount = tabPane.getTabCount();
        for (int i = 0; i < tabCount; i++) {
            if (rects[i].contains(p.x, p.y)) {
                return i;
            }
        }
        return -1;
    }

    /** Installs the state needed for mnemonics. */
    protected void initMnemonics() {
        mnemonicToIndexMap = new Hashtable<>();
        mnemonicInputMap = new ComponentInputMapUIResource(tabPane);
        mnemonicInputMap.setParent(SwingUtilities.getUIInputMap(tabPane, JComponent.WHEN_IN_FOCUSED_WINDOW));
        SwingUtilities.replaceUIInputMap(tabPane, JComponent.WHEN_IN_FOCUSED_WINDOW, mnemonicInputMap);
    }

    /** Ensure current layout. */
    protected void ensureCurrentLayout() {
        if (!tabPane.isValid()) {
            tabPane.validate();
        }
        /*
         * If tabPane doesn't have a peer yet, the validate() call will silently fail. We handle that by
         * forcing a layout if tabPane is still invalid. See bug 4237677.
         */
        if (!tabPane.isValid()) {
            TabbedPaneLayout layout = (TabbedPaneLayout) tabPane.getLayout();
            layout.calculateLayoutInfo();
        }
    }

    /**
     * Returns a point which is translated from the specified point in the JTabbedPane's coordinate
     * space to the coordinate space of the ScrollableTabPanel. This is used for SCROLL_TAB_LAYOUT ONLY.
     *
     * @param srcx the srcx
     * @param srcy the srcy
     * @param dest the dest
     * @return the point
     */
    protected Point translatePointToTabPanel(final int srcx, final int srcy, final Point dest) {
        Point vpp = tabScroller.viewport.getLocation();
        Point viewp = tabScroller.viewport.getViewPosition();
        dest.x = srcx - vpp.x + viewp.x;
        dest.y = srcy - vpp.y + viewp.y;
        return dest;
    }

    /**
     * Calculate baseline if necessary int.
     *
     * @return the int
     */
    protected int calculateBaselineIfNecessary() {
        if (!calculatedBaseline) {
            calculatedBaseline = true;
            baseline = -1;
            if (tabPane.getTabCount() > 0) {
                calculateBaseline();
            }
        }
        return baseline;
    }

    /**
     * Returns the tab the mouse is currently over, or {@code -1} if the mouse is no longer over any
     * tab.
     *
     * @return the tab the mouse is currently over, or {@code -1} if the mouse is no longer over any tab
     * @since 1.5
     */
    protected int getRolloverTab() {
        return rolloverTabIndex;
    }

    /**
     * Sets the tab the mouse is currently over to <code>index</code>. <code>index</code> will be -1 if
     * the mouse is no longer over any tab. No checking is done to ensure the passed in index identifies
     * a valid tab.
     *
     * @param index Index of the tab the mouse is over.
     * @since 1.5
     */
    protected void setRolloverTab(final int index) {
        rolloverTabIndex = index;
    }

    /**
     * Returns the baseline for the specified tab.
     *
     * @param tab index of tab to get baseline for
     * @return baseline or a value &lt; 0 indicating there is no reasonable baseline
     * @throws IndexOutOfBoundsException if index is out of range (index &lt; 0 || index &gt;= tab
     *         count)
     * @since 1.6
     */
    protected int getBaseline(final int tab) {
        if (tabPane.getTabComponentAt(tab) != null) {
            int offset = getBaselineOffset();
            if (offset != 0) {
                // The offset is not applied to the tab component, and so
                // in general we can't get good alignment like with components
                // in the tab.
                return -1;
            }
            Component c = tabPane.getTabComponentAt(tab);
            Dimension pref = c.getPreferredSize();
            Insets tabInsets = getTabInsets(tabPane.getTabPlacement(), tab);
            int cellHeight = maxTabHeight - tabInsets.top - tabInsets.bottom;
            return c.getBaseline(pref.width, pref.height) + (cellHeight - pref.height) / 2 + tabInsets.top;
        } else {
            View view = getTextViewForTab(tab);
            if (view != null) {
                int viewHeight = (int) view.getPreferredSpan(View.Y_AXIS);
                int baseline = BasicHTML.getHTMLBaseline(view, (int) view.getPreferredSpan(View.X_AXIS), viewHeight);
                if (baseline >= 0) {
                    return maxTabHeight / 2 - viewHeight / 2 + baseline + getBaselineOffset();
                }
                return -1;
            }
        }
        FontMetrics metrics = getFontMetrics();
        int fontHeight = metrics.getHeight();
        int fontBaseline = metrics.getAscent();
        return maxTabHeight / 2 - fontHeight / 2 + fontBaseline + getBaselineOffset();
    }

    /**
     * Returns the amount the baseline is offset by. This is typically the same as <code>
     * getTabLabelShiftY</code>.
     *
     * @return amount to offset the baseline by
     * @since 1.6
     */
    protected int getBaselineOffset() {
        switch (tabPane.getTabPlacement()) {
            case JTabbedPane.TOP:
                if (tabPane.getTabCount() > 1) {
                    return 1;
                } else {
                    return -1;
                }
            case JTabbedPane.BOTTOM:
                if (tabPane.getTabCount() > 1) {
                    return -1;
                } else {
                    return 1;
                }
            default: // RIGHT|LEFT
                return (maxTabHeight % 2);
        }
    }

    /**
     * Returns the tab index which intersects the specified point in the JTabbedPane's coordinate space.
     */
    public int tabForCoordinate(final JTabbedPane pane, final int x, final int y) {
        return tabForCoordinate(pane, x, y, true);
    }

    // TabbedPaneUI methods

    /**
     * Returns the bounds of the specified tab index. The bounds are with respect to the JTabbedPane's
     * coordinate space.
     */
    public Rectangle getTabBounds(final JTabbedPane pane, final int i) {
        ensureCurrentLayout();
        Rectangle tabRect = new Rectangle();
        return getTabBounds(i, tabRect);
    }

    public int getTabRunCount(final JTabbedPane pane) {
        ensureCurrentLayout();
        return runCount;
    }

    /** Calculate baseline. */
    protected void calculateBaseline() {
        int tabCount = tabPane.getTabCount();
        int tabPlacement = tabPane.getTabPlacement();
        maxTabHeight = calculateMaxTabHeight(tabPlacement);
        baseline = getBaseline(0);
        if (isHorizontalTabPlacement()) {
            for (int i = 1; i < tabCount; i++) {
                if (getBaseline(i) != baseline) {
                    baseline = -1;
                    break;
                }
            }
        } else {
            // left/right, tabs may be different sizes.
            FontMetrics fontMetrics = getFontMetrics();
            int fontHeight = fontMetrics.getHeight();
            int height = calculateTabHeight(tabPlacement, 0, fontHeight);
            for (int i = 1; i < tabCount; i++) {
                int newHeight = calculateTabHeight(tabPlacement, i, fontHeight);
                if (height != newHeight) {
                    // assume different baseline
                    baseline = -1;
                    break;
                }
            }
        }
    }

    /**
     * Is horizontal tab placement boolean.
     *
     * @return the boolean
     */
    protected boolean isHorizontalTabPlacement() {
        return tabPane.getTabPlacement() == TOP || tabPane.getTabPlacement() == BOTTOM;
    }

    // BasicTabbedPaneUI methods

    /**
     * Assure the rectangles are created.
     *
     * @param tabCount the tab count
     */
    protected void assureRectsCreated(final int tabCount) {
        int rectArrayLen = rects.length;
        if (tabCount != rectArrayLen) {
            Rectangle[] tempRectArray = new Rectangle[tabCount];
            System.arraycopy(rects, 0, tempRectArray, 0, Math.min(rectArrayLen, tabCount));
            rects = tempRectArray;
            for (int rectIndex = rectArrayLen; rectIndex < tabCount; rectIndex++) {
                rects[rectIndex] = new Rectangle();
            }
        }
    }

    /** Expands the tab runs array. */
    protected void expandTabRunsArray() {
        int rectLen = tabRuns.length;
        int[] newArray = new int[rectLen + 10];
        System.arraycopy(tabRuns, 0, newArray, 0, runCount);
        tabRuns = newArray;
    }

    /**
     * Returns the run for a tab.
     *
     * @param tabCount the tab count
     * @param tabIndex the tab index.
     * @return the run for a tab
     */
    protected int getRunForTab(final int tabCount, final int tabIndex) {
        for (int i = 0; i < runCount; i++) {
            int first = tabRuns[i];
            int last = lastTabInRun(tabCount, i);
            if (tabIndex >= first && tabIndex <= last) {
                return i;
            }
        }
        return 0;
    }

    /**
     * Returns the last tab in a run.
     *
     * @param tabCount the tab count
     * @param run the run
     * @return the last tab in a run
     */
    protected int lastTabInRun(final int tabCount, final int run) {
        if (runCount == 1) {
            return tabCount - 1;
        }
        int nextRun = (run == runCount - 1 ? 0 : run + 1);
        if (tabRuns[nextRun] == 0) {
            return tabCount - 1;
        }
        return tabRuns[nextRun] - 1;
    }

    /**
     * Returns the tab run overlay.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return the tab run overlay
     */
    protected int getTabRunOverlay(final int tabPlacement) {
        return tabRunOverlay;
    }

    /**
     * Returns the tab run indent.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param run the tab run
     * @return the tab run indent
     */
    protected int getTabRunIndent(final int tabPlacement, final int run) {
        return 0;
    }

    /**
     * Returns whether or not the tab run should be padded.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param run the tab run
     * @return whether or not the tab run should be padded
     */
    protected boolean shouldPadTabRun(final int tabPlacement, final int run) {
        return runCount > 1;
    }

    /**
     * Returns whether or not the tab run should be rotated.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return whether or not the tab run should be rotated
     */
    protected boolean shouldRotateTabRuns(final int tabPlacement) {
        return true;
    }

    /**
     * Calculates the tab height.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabIndex the index of the tab with respect to other tabs
     * @param fontHeight the font height
     * @return the tab height
     */
    protected int calculateTabHeight(final int tabPlacement, final int tabIndex, final int fontHeight) {
        int height = 0;
        Component c = tabPane.getTabComponentAt(tabIndex);
        if (c != null) {
            height = c.getPreferredSize().height;
        } else {
            View v = getTextViewForTab(tabIndex);
            if (v != null) {
                // html
                height += (int) v.getPreferredSpan(View.Y_AXIS);
            } else {
                // plain text
                height += fontHeight;
            }
            Icon icon = getIconForTab(tabIndex);

            if (icon != null) {
                height = Math.max(height, icon.getIconHeight());
            }
        }
        Insets tabInsets = getTabInsets(tabPlacement, tabIndex);
        height += tabInsets.top + tabInsets.bottom + 2;
        return height;
    }

    /**
     * Calculates the maximum tab height.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return the maximum tab height
     */
    protected int calculateMaxTabHeight(final int tabPlacement) {
        FontMetrics metrics = getFontMetrics();
        int tabCount = tabPane.getTabCount();
        int result = 0;
        int fontHeight = metrics.getHeight();
        for (int i = 0; i < tabCount; i++) {
            result = Math.max(calculateTabHeight(tabPlacement, i, fontHeight), result);
        }
        return result;
    }

    /**
     * Calculates the maximum tab width.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return the maximum tab width
     */
    protected int calculateMaxTabWidth(final int tabPlacement) {
        FontMetrics metrics = getFontMetrics();
        int tabCount = tabPane.getTabCount();
        int result = 0;
        for (int i = 0; i < tabCount; i++) {
            result = Math.max(calculateTabWidth(tabPlacement, i, metrics), result);
        }
        return result;
    }

    /**
     * Returns the font metrics.
     *
     * @return the font metrics
     */
    protected FontMetrics getFontMetrics() {
        Font font = tabPane.getFont();
        return tabPane.getFontMetrics(font);
    }

    /**
     * Calculates the tab width.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabIndex the index of the tab with respect to other tabs
     * @param metrics the font metrics
     * @return the tab width
     */
    protected int calculateTabWidth(final int tabPlacement, final int tabIndex, final FontMetrics metrics) {
        Insets tabInsets = getTabInsets(tabPlacement, tabIndex);
        int width = tabInsets.left + tabInsets.right + 3;
        Component tabComponent = tabPane.getTabComponentAt(tabIndex);
        if (tabComponent != null) {
            width += tabComponent.getPreferredSize().width;
        } else {
            Icon icon = getIconForTab(tabIndex);
            if (icon != null) {
                width += icon.getIconWidth() + textIconGap;
            }
            View v = getTextViewForTab(tabIndex);
            if (v != null) {
                // html
                width += (int) v.getPreferredSpan(View.X_AXIS);
            } else {
                // plain text
                String title = tabPane.getTitleAt(tabIndex);
                width += SwingUtil.stringWidth(tabPane, metrics, title);
            }
        }
        return width;
    }

    /**
     * Returns the tab insets.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabIndex the tab index
     * @return the tab insets
     */
    protected Insets getTabInsets(final int tabPlacement, final int tabIndex) {
        return tabInsets;
    }

    /**
     * Calculates the tab area height.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param horizRunCount horizontal run count
     * @param maxTabHeight maximum tab height
     * @return the tab area height
     */
    protected int calculateTabAreaHeight(final int tabPlacement, final int horizRunCount, final int maxTabHeight) {
        Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
        int tabRunOverlay = getTabRunOverlay(tabPlacement);
        return (horizRunCount > 0
                ? horizRunCount * (maxTabHeight - tabRunOverlay) + tabRunOverlay + tabAreaInsets.top
                        + tabAreaInsets.bottom
                : 0);
    }

    /**
     * Calculates the tab area width.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param vertRunCount vertical run count
     * @param maxTabWidth maximum tab width
     * @return the tab area width
     */
    protected int calculateTabAreaWidth(final int tabPlacement, final int vertRunCount, final int maxTabWidth) {
        Insets tabAreaInsets = getTabAreaInsets(tabPlacement);
        int tabRunOverlay = getTabRunOverlay(tabPlacement);
        return (vertRunCount > 0
                ? vertRunCount * (maxTabWidth - tabRunOverlay) + tabRunOverlay + tabAreaInsets.left
                        + tabAreaInsets.right
                : 0);
    }

    /**
     * Returns the selected tab pad insets.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return the selected tab pad insets
     */
    protected Insets getSelectedTabPadInsets(final int tabPlacement) {
        rotateInsets(selectedTabPadInsets, currentPadInsets, tabPlacement);
        return currentPadInsets;
    }

    /**
     * Rotates the insets.
     *
     * @param topInsets the top insets
     * @param targetInsets the target insets
     * @param targetPlacement the target placement
     */
    @SuppressWarnings("SuspiciousNameCombination")
    protected static void rotateInsets(final Insets topInsets, final Insets targetInsets, final int targetPlacement) {

        switch (targetPlacement) {
            case LEFT:
                targetInsets.top = topInsets.left;
                targetInsets.left = topInsets.top;
                targetInsets.bottom = topInsets.right;
                targetInsets.right = topInsets.bottom;
                break;
            case BOTTOM:
                targetInsets.top = topInsets.bottom;
                targetInsets.left = topInsets.left;
                targetInsets.bottom = topInsets.top;
                targetInsets.right = topInsets.right;
                break;
            case RIGHT:
                targetInsets.top = topInsets.left;
                targetInsets.left = topInsets.bottom;
                targetInsets.bottom = topInsets.right;
                targetInsets.right = topInsets.top;
                break;
            case TOP:
            default:
                targetInsets.top = topInsets.top;
                targetInsets.left = topInsets.left;
                targetInsets.bottom = topInsets.bottom;
                targetInsets.right = topInsets.right;
        }
    }

    /**
     * Returns the tab area insets.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return the pad area insets
     */
    protected Insets getTabAreaInsets(final int tabPlacement) {
        rotateInsets(tabAreaInsets, currentTabAreaInsets, tabPlacement);
        return currentTabAreaInsets;
    }

    /**
     * Returns the content border insets.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @return the content border insets
     */
    protected Insets getContentBorderInsets(final int tabPlacement) {
        return contentBorderInsets;
    }

    /**
     * Navigate the selected tab.
     *
     * @param direction the direction
     */
    protected void navigateSelectedTab(final int direction) {
        int tabPlacement = tabPane.getTabPlacement();
        int current = !Boolean.FALSE.equals(UIManager.get("TabbedPane.selectionFollowsFocus"))
                ? tabPane.getSelectedIndex()
                : getFocusIndex();
        int tabCount = tabPane.getTabCount();
        boolean leftToRight = tabPane.getComponentOrientation().isLeftToRight();

        // If we have no tabs then don't navigate.
        if (tabCount <= 0) {
            return;
        }

        int offset;
        switch (tabPlacement) {
            case LEFT:
            case RIGHT:
                switch (direction) {
                    case NEXT:
                        selectNextTab(current);
                        break;
                    case PREVIOUS:
                        selectPreviousTab(current);
                        break;
                    case NORTH:
                        selectPreviousTabInRun(current);
                        break;
                    case SOUTH:
                        selectNextTabInRun(current);
                        break;
                    case WEST:
                        offset = getTabRunOffset(tabPlacement, tabCount, current, false);
                        selectAdjacentRunTab(tabPlacement, current, offset);
                        break;
                    case EAST:
                        offset = getTabRunOffset(tabPlacement, tabCount, current, true);
                        selectAdjacentRunTab(tabPlacement, current, offset);
                        break;
                    default:
                }
                break;
            case BOTTOM:
            case TOP:
            default:
                switch (direction) {
                    case NEXT:
                        selectNextTab(current);
                        break;
                    case PREVIOUS:
                        selectPreviousTab(current);
                        break;
                    case NORTH:
                        offset = getTabRunOffset(tabPlacement, tabCount, current, false);
                        selectAdjacentRunTab(tabPlacement, current, offset);
                        break;
                    case SOUTH:
                        offset = getTabRunOffset(tabPlacement, tabCount, current, true);
                        selectAdjacentRunTab(tabPlacement, current, offset);
                        break;
                    case EAST:
                        if (leftToRight) {
                            selectNextTabInRun(current);
                        } else {
                            selectPreviousTabInRun(current);
                        }
                        break;
                    case WEST:
                        if (leftToRight) {
                            selectPreviousTabInRun(current);
                        } else {
                            selectNextTabInRun(current);
                        }
                        break;
                    default:
                }
        }
    }

    /**
     * Select the next tab in the run.
     *
     * @param current the current tab
     */
    protected void selectNextTabInRun(final int current) {
        int tabCount = tabPane.getTabCount();
        int tabIndex = getNextTabIndexInRun(tabCount, current);

        while (tabIndex != current && !tabPane.isEnabledAt(tabIndex)) {
            tabIndex = getNextTabIndexInRun(tabCount, tabIndex);
        }
        navigateTo(tabIndex);
    }

    // Tab Navigation methods

    /**
     * Select the previous tab in the run.
     *
     * @param current the current tab
     */
    protected void selectPreviousTabInRun(final int current) {
        int tabCount = tabPane.getTabCount();
        int tabIndex = getPreviousTabIndexInRun(tabCount, current);

        while (tabIndex != current && !tabPane.isEnabledAt(tabIndex)) {
            tabIndex = getPreviousTabIndexInRun(tabCount, tabIndex);
        }
        navigateTo(tabIndex);
    }

    /**
     * Select the next tab.
     *
     * @param current the current tab
     */
    protected void selectNextTab(final int current) {
        int tabIndex = getNextTabIndex(current);

        while (tabIndex != current && !tabPane.isEnabledAt(tabIndex)) {
            tabIndex = getNextTabIndex(tabIndex);
        }
        navigateTo(tabIndex);
    }

    /**
     * Select the previous tab.
     *
     * @param current the current tab
     */
    protected void selectPreviousTab(final int current) {
        int tabIndex = getPreviousTabIndex(current);

        while (tabIndex != current && !tabPane.isEnabledAt(tabIndex)) {
            tabIndex = getPreviousTabIndex(tabIndex);
        }
        navigateTo(tabIndex);
    }

    /**
     * Selects an adjacent run of tabs.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabIndex the index of the tab with respect to other tabs
     * @param offset selection offset
     */
    protected void selectAdjacentRunTab(final int tabPlacement, final int tabIndex, final int offset) {
        if (runCount < 2) {
            return;
        }
        int newIndex;
        Rectangle r = rects[tabIndex];
        switch (tabPlacement) {
            case LEFT:
            case RIGHT:
                newIndex = tabForCoordinate(tabPane, r.x + r.width / 2 + offset, r.y + r.height / 2);
                break;
            case BOTTOM:
            case TOP:
            default:
                newIndex = tabForCoordinate(tabPane, r.x + r.width / 2, r.y + r.height / 2 + offset);
        }
        if (newIndex != -1) {
            while (!tabPane.isEnabledAt(newIndex) && newIndex != tabIndex) {
                newIndex = getNextTabIndex(newIndex);
            }
            navigateTo(newIndex);
        }
    }

    /**
     * Navigate to.
     *
     * @param index the index
     */
    protected void navigateTo(final int index) {
        if (!Boolean.FALSE.equals(UIManager.get("TabbedPane.selectionFollowsFocus"))) {
            tabPane.setSelectedIndex(index);
        } else {
            // Just move focus (not selection)
            setFocusIndex(index, true);
        }
    }

    /** Makes sure the focusIndex is valid. */
    protected void validateFocusIndex() {
        if (focusIndex >= tabPane.getTabCount()) {
            setFocusIndex(tabPane.getSelectedIndex(), false);
        }
    }

    /**
     * Sets focus index.
     *
     * @param index the index
     * @param repaint the repaint
     */
    void setFocusIndex(final int index, final boolean repaint) {
        if (repaint && !isRunsDirty) {
            repaintTab(focusIndex);
            focusIndex = index;
            repaintTab(focusIndex);
        } else {
            focusIndex = index;
        }
    }

    /**
     * Repaints the specified tab.
     *
     * @param index the index
     */
    protected void repaintTab(final int index) {
        // If we're not valid that means we will shortly be validated and
        // painted, which means we don't have to do anything here.
        if (!isRunsDirty && index >= 0 && index < tabPane.getTabCount()) {
            tabPane.repaint(getTabBounds(tabPane, index));
        }
    }

    /**
     * Returns the bounds of the specified tab in the coordinate space of the JTabbedPane component.
     * This is required because the tab rects are by default defined in the coordinate space of the
     * component where they are rendered, which could be the JTabbedPane (for WRAP_TAB_LAYOUT) or a
     * ScrollableTabPanel (SCROLL_TAB_LAYOUT). This method should be used whenever the tab rectangle
     * must be relative to the JTabbedPane itself and the result should be placed in a designated
     * Rectangle object (rather than instantiating and returning a new Rectangle each time). The tab
     * index parameter must be a valid tabbed pane tab index (0 to tab count - 1, inclusive). The
     * destination rectangle parameter must be a valid <code>Rectangle</code> instance. The handling of
     * invalid parameters is unspecified.
     *
     * @param tabIndex the index of the tab
     * @param dest the rectangle where the result should be placed
     * @return the resulting rectangle
     * @since 1.4
     */
    protected Rectangle getTabBounds(final int tabIndex, final Rectangle dest) {
        dest.width = rects[tabIndex].width;
        dest.height = rects[tabIndex].height;

        if (scrollableTabLayoutEnabled()) { // SCROLL_TAB_LAYOUT
            // Need to translate coordinates based on viewport location &
            // view position
            Point vpp = tabScroller.viewport.getLocation();
            Point viewp = tabScroller.viewport.getViewPosition();
            dest.x = rects[tabIndex].x + vpp.x - viewp.x;
            dest.y = rects[tabIndex].y + vpp.y - viewp.y;

        } else { // WRAP_TAB_LAYOUT
            dest.x = rects[tabIndex].x;
            dest.y = rects[tabIndex].y;
        }
        return dest;
    }

    /**
     * Returns the index of the tab closest to the passed in location, note that the returned tab may
     * not contain the location x,y.
     *
     * @param x the x
     * @param y the y
     * @return the closest tab
     */
    protected int getClosestTab(final int x, final int y) {
        int min = 0;
        int tabCount = Math.min(rects.length, tabPane.getTabCount());
        int max = tabCount;
        int tabPlacement = tabPane.getTabPlacement();
        boolean useX = (tabPlacement == TOP || tabPlacement == BOTTOM);
        int want = (useX) ? x : y;

        while (min != max) {
            int current = (max + min) / 2;
            int minLoc;
            int maxLoc;

            if (useX) {
                minLoc = rects[current].x;
                maxLoc = minLoc + rects[current].width;
            } else {
                minLoc = rects[current].y;
                maxLoc = minLoc + rects[current].height;
            }
            if (want < minLoc) {
                max = current;
                if (min == max) {
                    return Math.max(0, current - 1);
                }
            } else if (want >= maxLoc) {
                min = current;
                if (max - min <= 1) {
                    return Math.max(current + 1, tabCount - 1);
                }
            } else {
                return current;
            }
        }
        return min;
    }

    /**
     * Returns the index of the tab that has focus.
     *
     * @return index of tab that has focus
     * @since 1.5
     */
    protected int getFocusIndex() {
        return focusIndex;
    }

    /**
     * Returns the tab run offset.
     *
     * @param tabPlacement the placement (left, right, bottom, top) of the tab
     * @param tabCount the tab count
     * @param tabIndex the index of the tab with respect to other tabs
     * @param forward forward or not
     * @return the tab run offset
     */
    protected int getTabRunOffset(final int tabPlacement, final int tabCount, final int tabIndex,
            final boolean forward) {
        int run = getRunForTab(tabCount, tabIndex);
        int offset;
        switch (tabPlacement) {
            case LEFT: {
                if (run == 0) {
                    offset = (forward ? -(calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth) - maxTabWidth)
                            : -maxTabWidth);

                } else if (run == runCount - 1) {
                    offset = (forward ? maxTabWidth
                            : calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth) - maxTabWidth);
                } else {
                    offset = (forward ? maxTabWidth : -maxTabWidth);
                }
                break;
            }
            case RIGHT: {
                if (run == 0) {
                    offset = (forward ? maxTabWidth
                            : calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth) - maxTabWidth);
                } else if (run == runCount - 1) {
                    offset = (forward ? -(calculateTabAreaWidth(tabPlacement, runCount, maxTabWidth) - maxTabWidth)
                            : -maxTabWidth);
                } else {
                    offset = (forward ? maxTabWidth : -maxTabWidth);
                }
                break;
            }
            case BOTTOM: {
                if (run == 0) {
                    offset = (forward ? maxTabHeight
                            : calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight) - maxTabHeight);
                } else if (run == runCount - 1) {
                    offset = (forward ? -(calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight) - maxTabHeight)
                            : -maxTabHeight);
                } else {
                    offset = (forward ? maxTabHeight : -maxTabHeight);
                }
                break;
            }
            case TOP:
            default: {
                if (run == 0) {
                    offset = (forward ? -(calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight) - maxTabHeight)
                            : -maxTabHeight);
                } else if (run == runCount - 1) {
                    offset = (forward ? maxTabHeight
                            : calculateTabAreaHeight(tabPlacement, runCount, maxTabHeight) - maxTabHeight);
                } else {
                    offset = (forward ? maxTabHeight : -maxTabHeight);
                }
            }
        }
        return offset;
    }

    /**
     * Returns the previous tab index.
     *
     * @param base the base
     * @return the previous tab index
     */
    protected int getPreviousTabIndex(final int base) {
        int tabIndex = (base - 1 >= 0 ? base - 1 : tabPane.getTabCount() - 1);
        return (Math.max(tabIndex, 0));
    }

    /**
     * Returns the next tab index.
     *
     * @param base the base
     * @return the next tab index
     */
    protected int getNextTabIndex(final int base) {
        return (base + 1) % tabPane.getTabCount();
    }

    /**
     * Returns the next tab index in the run.
     *
     * @param tabCount the tab count
     * @param base the base
     * @return the next tab index in the run
     */
    protected int getNextTabIndexInRun(final int tabCount, final int base) {
        if (runCount < 2) {
            return getNextTabIndex(base);
        }
        int currentRun = getRunForTab(tabCount, base);
        int next = getNextTabIndex(base);
        if (next == tabRuns[getNextTabRun(currentRun)]) {
            return tabRuns[currentRun];
        }
        return next;
    }

    /**
     * Returns the previous tab index in the run.
     *
     * @param tabCount the tab count
     * @param base the base
     * @return the previous tab index in the run
     */
    protected int getPreviousTabIndexInRun(final int tabCount, final int base) {
        if (runCount < 2) {
            return getPreviousTabIndex(base);
        }
        int currentRun = getRunForTab(tabCount, base);
        if (base == tabRuns[currentRun]) {
            int previous = tabRuns[getNextTabRun(currentRun)] - 1;
            return (previous != -1 ? previous : tabCount - 1);
        }
        return getPreviousTabIndex(base);
    }

    /**
     * Returns the previous tab run.
     *
     * @param baseRun the base run
     * @return the previous tab run
     */
    protected int getPreviousTabRun(final int baseRun) {
        int runIndex = (baseRun - 1 >= 0 ? baseRun - 1 : runCount - 1);
        return (Math.max(runIndex, 0));
    }

    /**
     * Returns the next tab run.
     *
     * @param baseRun the base run
     * @return the next tab run
     */
    protected int getNextTabRun(final int baseRun) {
        return (baseRun + 1) % runCount;
    }

    /**
     * Request focus for visible component boolean.
     *
     * @return the boolean
     */
    // REMIND(aim,7/29/98): This method should be made
    // protected in the next release where
    // API changes are allowed
    boolean requestFocusForVisibleComponent() {
        return SwingUtil.tabbedPaneChangeFocusTo(getVisibleComponent());
    }

    /**
     * Returns the visible component.
     *
     * @return the visible component
     */
    protected Component getVisibleComponent() {
        return visibleComponent;
    }

    /**
     * Sets the visible component.
     *
     * @param component the component
     */
    protected void setVisibleComponent(final Component component) {
        if (visibleComponent != null && visibleComponent != component && visibleComponent.getParent() == tabPane
                && visibleComponent.isVisible()) {

            visibleComponent.setVisible(false);
        }
        if (component != null && !component.isVisible()) {
            component.setVisible(true);
        }
        visibleComponent = component;
    }

    /** The type Actions. */
    protected static class Actions extends UIAction {
        /** The Next. */
        static final String NEXT = "navigateNext";
        /** The Previous. */
        static final String PREVIOUS = "navigatePrevious";
        /** The Right. */
        static final String RIGHT = "navigateRight";
        /** The Left. */
        static final String LEFT = "navigateLeft";
        /** The Up. */
        static final String UP = "navigateUp";
        /** The Down. */
        static final String DOWN = "navigateDown";
        /** The Page up. */
        static final String PAGE_UP = "navigatePageUp";
        /** The Page down. */
        static final String PAGE_DOWN = "navigatePageDown";
        /** The Request focus. */
        static final String REQUEST_FOCUS = "requestFocus";
        /** The Request focus for visible. */
        static final String REQUEST_FOCUS_FOR_VISIBLE = "requestFocusForVisibleComponent";
        /** The Set selected. */
        static final String SET_SELECTED = "setSelectedIndex";
        /** The Select focused. */
        static final String SELECT_FOCUSED = "selectTabWithFocus";
        /** The Scroll forward. */
        static final String SCROLL_FORWARD = "scrollTabsForwardAction";
        /** The Scroll backward. */
        static final String SCROLL_BACKWARD = "scrollTabsBackwardAction";

        /**
         * Instantiates a new Actions.
         *
         * @param key the key
         */
        Actions(final String key) {
            super(key);
        }

        public void actionPerformed(final ActionEvent e) {
            String key = getName();
            JTabbedPane pane = (JTabbedPane) e.getSource();
            DarkTabbedPaneUIBridge ui = DarkUIUtil.getUIOfType(pane.getUI(), DarkTabbedPaneUIBridge.class);

            if (ui == null) {
                return;
            }
            if (Objects.equals(key, NEXT)) {
                ui.navigateSelectedTab(SwingConstants.NEXT);
            } else if (Objects.equals(key, PREVIOUS)) {
                ui.navigateSelectedTab(SwingConstants.PREVIOUS);
            } else if (Objects.equals(key, RIGHT)) {
                ui.navigateSelectedTab(SwingConstants.EAST);
            } else if (Objects.equals(key, LEFT)) {
                ui.navigateSelectedTab(SwingConstants.WEST);
            } else if (Objects.equals(key, UP)) {
                ui.navigateSelectedTab(SwingConstants.NORTH);
            } else if (Objects.equals(key, DOWN)) {
                ui.navigateSelectedTab(SwingConstants.SOUTH);
            } else if (Objects.equals(key, PAGE_UP)) {
                int tabPlacement = pane.getTabPlacement();
                if (tabPlacement == TOP || tabPlacement == BOTTOM) {
                    ui.navigateSelectedTab(SwingConstants.WEST);
                } else {
                    ui.navigateSelectedTab(SwingConstants.NORTH);
                }
            } else if (Objects.equals(key, PAGE_DOWN)) {
                int tabPlacement = pane.getTabPlacement();
                if (tabPlacement == TOP || tabPlacement == BOTTOM) {
                    ui.navigateSelectedTab(SwingConstants.EAST);
                } else {
                    ui.navigateSelectedTab(SwingConstants.SOUTH);
                }
            } else if (Objects.equals(key, REQUEST_FOCUS)) {
                pane.requestFocusInWindow();
            } else if (Objects.equals(key, REQUEST_FOCUS_FOR_VISIBLE)) {
                ui.requestFocusForVisibleComponent();
            } else if (Objects.equals(key, SET_SELECTED)) {
                String command = e.getActionCommand();

                if (command != null && command.length() > 0) {
                    int mnemonic = e.getActionCommand().charAt(0);
                    if (mnemonic >= 'a' && mnemonic <= 'z') {
                        mnemonic -= ('a' - 'A');
                    }
                    Integer index = ui.mnemonicToIndexMap.get(mnemonic);
                    if (index != null && pane.isEnabledAt(index)) {
                        pane.setSelectedIndex(index);
                    }
                }
            } else if (Objects.equals(key, SELECT_FOCUSED)) {
                int focusIndex = ui.getFocusIndex();
                if (focusIndex != -1) {
                    pane.setSelectedIndex(focusIndex);
                }
            }
        }
    }

    // Controller: event listeners

    /** The type Tab container. */
    protected class TabContainer extends JPanel implements UIResource {
        /** The Notify tabbed pane. */
        protected boolean notifyTabbedPane = true;

        /** Instantiates a new Tab container. */
        public TabContainer() {
            super(null);
            setOpaque(false);
        }

        public void remove(final Component comp) {
            int index = tabPane.indexOfTabComponent(comp);
            super.remove(comp);
            if (notifyTabbedPane && index != -1) {
                tabPane.setTabComponentAt(index, null);
            }
        }

        public void doLayout() {
            // We layout tabComponents in JTabbedPane's layout manager
            // and use this method as a hook for repainting tabs
            // to update tabs area e.g. when the size of tabComponent was changed
            if (scrollableTabLayoutEnabled()) {
                tabScroller.tabPanel.repaint();
            } else {
                tabPane.repaint(getBounds());
            }
        }

        /** Remove unused tab components. */
        protected void removeUnusedTabComponents() {
            for (Component c : getComponents()) {
                if (!(c instanceof UIResource)) {
                    int index = tabPane.indexOfTabComponent(c);
                    if (index == -1) {
                        super.remove(c);
                    }
                }
            }
        }

        public boolean isOptimizedDrawingEnabled() {
            return true;
        }
    }
}
