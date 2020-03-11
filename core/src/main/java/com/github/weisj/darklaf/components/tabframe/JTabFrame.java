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
package com.github.weisj.darklaf.components.tabframe;

import com.github.weisj.darklaf.ui.tabframe.TabFrameTransferHandler;
import com.github.weisj.darklaf.util.Alignment;

import javax.swing.*;
import javax.swing.plaf.ComponentUI;
import java.awt.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

/**
 * Frame that supports popup components.
 *
 * @author Jannis Weis
 */
public class JTabFrame extends JComponent {

    public static final String KEY_DND = "dndEnabled";

    private final JComponent bottomTabs = createTabContainer();
    private final JComponent topTabs = createTabContainer();
    private final JComponent leftTabs = createTabContainer();
    private final JComponent rightTabs = createTabContainer();

    private final TabFrameContent content = createContentPane();

    private final List<TabFrameTab>[] tabLists;
    private final List<TabFramePopup>[] popupLists;
    private final int[] selectedIndices;

    private int tabSize = -1;
    private int maxTabWidth = -1;
    private boolean inTransfer;
    private Alignment transferAlign;
    private int transferIndex;
    private boolean dndEnabled;

    /**
     * Creates new {@link JTabFrame}. A TabFrame displays one center component and multiple popups around that can be
     * toggles with a TabbedPane like tabArea along the border.
     */
    public JTabFrame() {
        super();
        updateUI();
        add(content.getComponent());

        int count = Alignment.values().length;
        //noinspection unchecked
        tabLists = (ArrayList<TabFrameTab>[]) new ArrayList[count];
        //noinspection unchecked
        popupLists = (ArrayList<TabFramePopup>[]) new ArrayList[count];
        for (int i = 0; i < count; i++) {
            tabLists[i] = new ArrayList<>();
            popupLists[i] = new ArrayList<>();
        }
        selectedIndices = new int[count];
        setDndEnabled(true);
    }

    @Override
    public void updateUI() {
        setUI(UIManager.getUI(this));
    }

    /**
     * Get the ui.
     *
     * @return the ui.
     */
    public TabFrameUI getUI() {
        return (TabFrameUI) ui;
    }

    @Override
    protected void setUI(final ComponentUI newUI) {
        if (!(newUI instanceof TabFrameUI)) {
            throw new IllegalArgumentException("UI class must be of type TabFrameUI");
        }
        super.setUI(newUI);
    }

    @Override
    public String getUIClassID() {
        return "TabFrameUI";
    }

    /**
     * Returns the height/width of the tab container.
     *
     * @return the tab size.
     */
    public int getTabSize() {
        return tabSize >= 0 ? tabSize : getUI().getTabSize(this);
    }

    /**
     * Sets the height/width of the tab container.
     * <p>
     * A negative value means the ui should calculate the appropriate size.
     *
     * @param size the size of the tab container.
     */
    public void setTabSize(final int size) {
        this.tabSize = size;
    }

    /**
     * Get the maximum width a tab can inhibit. A negative value indicated the tabs should use as much space as their
     * proffered size indicates.
     *
     * @return the maximum tab width.
     */
    public int getMaxTabWidth() {
        return maxTabWidth;
    }

    /**
     * Sets the maximum width a tab can inhibit. A negative value indicated the tabs should use as much space as their
     * proffered size indicates.
     *
     * @param maxTabWidth the maximum tab width.
     */
    public void setMaxTabWidth(final int maxTabWidth) {
        this.maxTabWidth = maxTabWidth;
    }

    /**
     * Get the number of tabs at the top.
     *
     * @return number of tabs at the top.
     */
    public int getTopTabCount() {
        return getTabCountAt(Alignment.NORTH) + getTabCountAt(Alignment.NORTH_EAST);
    }

    /**
     * Get the number of tabs at the given alignment position.
     *
     * @param a the alignment position.
     * @return number of tabs.
     */
    public int getTabCountAt(final Alignment a) {
        return tabsForAlignment(a).size();
    }

    /**
     * Insert a tab. A default tab component and popup component will be created.
     *
     * @param c     the component to add.
     * @param a     the alignment position to add at.{@link TabFramePosition#getAlignment()}
     * @param index the index to insert at.{@link TabFramePosition#getIndex()}
     */
    public void insertTab(final Component c, final Alignment a, final int index) {
        String title = c.getName();
        insertTab(c, title == null ? "" : title, a, index);
    }

    /**
     * Get the number of tabs at the bottom.
     *
     * @return number of tabs at the bottom.
     */
    public int getBottomTabCount() {
        return getTabCountAt(Alignment.SOUTH) + getTabCountAt(Alignment.SOUTH_WEST);
    }

    /**
     * Get the number of tabs at the left.
     *
     * @return number of tabs at the left.
     */
    public int getLeftTabCount() {
        return getTabCountAt(Alignment.WEST) + getTabCountAt(Alignment.NORTH_WEST);
    }

    /**
     * Get the number of tabs at the right.
     *
     * @return number of tabs at the right.
     */
    public int getRightTabCount() {
        return getTabCountAt(Alignment.EAST) + getTabCountAt(Alignment.SOUTH_EAST);
    }

    /**
     * Get the center component.
     *
     * @return the center component.
     */
    public Component getContent() {
        return content.getContent();
    }

    /**
     * Set the center component.
     *
     * @param c the center component.
     */
    public void setContent(final Component c) {
        content.setContent(c);
    }

    /**
     * Create a tab container.
     *
     * @return a tab container.
     */
    protected JComponent createTabContainer() {
        return new TabArea();
    }

    /**
     * Create the content pane.
     *
     * @return the content pane.
     */
    protected TabFrameContent createContentPane() {
        return new TabFrameContentPane();
    }

    /**
     * Insert a tab. A default tab component and popup component will be created.
     *
     * @param c     the component to add.
     * @param title the title of the component.
     * @param a     the alignment position to add at.{@link TabFramePosition#getAlignment()}
     * @param index the index to insert at.{@link TabFramePosition#getIndex()}
     */
    public void insertTab(final Component c, final String title, final Alignment a, final int index) {
        insertTab(c, title, null, a, index);
    }

    /**
     * Insert a tab. A default tab component and popup component will be created.
     *
     * @param c     the component to add.
     * @param title the title of the component.
     * @param icon  the icon
     * @param a     the alignment position to add at.{@link TabFramePosition#getAlignment()}
     * @param index the index to insert at.{@link TabFramePosition#getIndex()}
     */
    public void insertTab(final Component c, final String title, final Icon icon, final Alignment a,
                          final int index) {
        TabFramePopup popup = new PanelPopup(title, icon, c);
        insertTab(popup, title, icon, a, index);
    }

    /**
     * Insert a tab. A default tab component will be created.
     *
     * @param c     the popup to add.
     * @param title the title of the component.
     * @param icon  the icon
     * @param a     the alignment position to add at.{@link TabFramePosition#getAlignment()}
     * @param index the index to insert at.{@link TabFramePosition#getIndex()}
     */
    public void insertTab(final TabFramePopup c, final String title, final Icon icon, final Alignment a,
                          final int index) {
        if (a == Alignment.CENTER) {
            return;
        }
        insertTab(c, createDefaultTab(title, icon, a, index), a, index);
    }

    /**
     * Insert a tab. A default tab component will be created.
     *
     * @param c     the popup to add.
     * @param tab   the corresponding tab.
     * @param a     the alignment position to add at.{@link TabFramePosition#getAlignment()}
     * @param index the index to insert at.{@link TabFramePosition#getIndex()}
     */
    public void insertTab(final TabFramePopup c, final TabFrameTab tab, final Alignment a, final int index) {
        if (a == Alignment.CENTER) {
            return;
        }
        insertTabComp(tab, a, index);
        compsForAlignment(a).add(index, c);
        c.setEnabled(false);
        c.setTabFrame(this);
        c.setAlignment(a);
        c.setIndex(index);
    }

    /*
     * Inserts a tab component at the given position.
     */
    private void insertTabComp(final TabFrameTab tabComp, final Alignment a, final int index) {
        tabComp.setOrientation(a);
        getTabContainer(a).add(tabComp.getComponent());
        List<TabFrameTab> tabs = tabsForAlignment(a);
        //Adjust indices for tabs.
        Iterator<TabFrameTab> iterator = tabs.listIterator(index);
        while (iterator.hasNext()) {
            TabFrameTab tab = iterator.next();
            tab.setIndex(tab.getIndex() + 1);
        }
        tabComp.setIndex(index);
        tabComp.setOrientation(a);
        tabs.add(index, tabComp);
    }

    protected TabFrameTab createDefaultTab(final String text, final Icon icon, final Alignment a, final int index) {
        return new TabFrameTabLabel(text, icon, a, index, this);
    }

    /**
     * Get a list of components at the given alignment position.
     *
     * @param a the alignment position.
     * @return list of components at position.
     */
    public List<TabFramePopup> compsForAlignment(final Alignment a) {
        return popupLists[a.ordinal()];
    }

    /**
     * Get the tab container for the given alignment position.
     *
     * @param a the alignment position.{@link TabFramePosition#getAlignment()}
     * @return the tab container.
     */

    public JComponent getTabContainer(final Alignment a) {
        switch (a) {
            case NORTH:
            case NORTH_EAST:
                return getTopTabContainer();
            case SOUTH:
            case SOUTH_WEST:
                return getBottomTabContainer();
            case EAST:
            case SOUTH_EAST:
                return getRightTabContainer();
            case WEST:
            case NORTH_WEST:
                return getLeftTabContainer();
            case CENTER:
                throw new IllegalArgumentException("invalid alignment: " + a);
            default:
                throw new IllegalArgumentException();
        }
    }

    /**
     * Get a list of tab components at the given alignment position.
     *
     * @param a the alignment position.
     * @return list of tab components at position.
     */
    public List<TabFrameTab> tabsForAlignment(final Alignment a) {
        return tabLists[a.ordinal()];
    }

    /**
     * Get the container that holds the top tab components.
     *
     * @return the top tab container.
     */
    public JComponent getTopTabContainer() {
        return topTabs;
    }

    /**
     * Get the container that holds the bottom tab components.
     *
     * @return the bottom tab container.
     */
    public JComponent getBottomTabContainer() {
        return bottomTabs;
    }

    /**
     * Get the container that holds the right tab components.
     *
     * @return the right tab container.
     */
    public JComponent getRightTabContainer() {
        return rightTabs;
    }

    /**
     * Get the container that holds the left tab components.
     *
     * @return the left tab container.
     */
    public JComponent getLeftTabContainer() {
        return leftTabs;
    }

    /**
     * Sets the popup at the given position.
     *
     * @param c     the popup to place at the position.
     * @param title the title of the popup.
     * @param icon  the icon of the popup.
     * @param a     the alignment position to place at.{@link TabFramePosition#getAlignment()}
     * @param index the index to place at.{@link TabFramePosition#getIndex()}
     */
    public void setTabAt(final TabFramePopup c, final String title, final Icon icon,
                         final Alignment a, final int index) {
        if (a == Alignment.CENTER) {
            return;
        }
        String text = title == null ? c.getComponent().getName() : title;
        text = text == null ? c.getTitle() : text;
        TabFrameTab tabComponent = createDefaultTab(text, icon, a, index);
        c.setTitle(text);
        c.setIcon(icon);
        c.setTabFrame(this);
        c.setAlignment(a);
        c.setIndex(index);

        tabComponent.setSelected(getTabComponentAt(a, index).isSelected());
        setTabComponent(tabComponent, a, index);
        compsForAlignment(a).set(index, c);
        if (tabComponent.isSelected()) {
            getComponentAt(a, index).doLayout();
            getComponentAt(a, index).repaint();
        }
    }

    /**
     * Get the tab component at the given position.
     *
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index.{@link TabFramePosition#getIndex()}
     * @return the tab component.
     */
    public TabFrameTab getTabComponentAt(final Alignment a, final int index) {
        List<TabFrameTab> tabs = tabsForAlignment(a);
        return tabs.get(index);
    }

    /*
     * Set the tab component at the given position.
     */
    private void setTabComponent(final TabFrameTab tab, final Alignment a, final int index) {
        List<TabFrameTab> tabs = tabsForAlignment(a);
        TabFrameTab oldComp = tabs.get(index);
        getTabContainer(a).remove(oldComp.getComponent());
        getTabContainer(a).add(tab.getComponent());
        tabs.set(index, tab);
    }

    /**
     * Get the component at the given position.
     *
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index. {@link TabFramePosition#getIndex()} ()}
     * @return the popup component specified by {@link TabFramePopup#getContentPane()}.
     */
    public Component getComponentAt(final Alignment a, final int index) {
        List<TabFramePopup> tabs = compsForAlignment(a);
        return tabs.get(index).getContentPane();
    }

    /**
     * Close a popup.
     *
     * @param c the component to close.
     */
    public void closeTab(final Component c) {
        TabFramePosition res = findComponent(c);
        if (res != null) {
            closeTab(res.getAlignment(), res.getIndex());
        }
    }

    /**
     * Gets the position of the given component or null if it isn't currently added.
     *
     * @param c the component to find.
     * @return the position in the tabFrame.{@link TabFramePosition}
     */
    public TabFramePosition findComponent(final Component c) {
        for (Alignment a : Alignment.values()) {
            List<TabFramePopup> list = popupLists[a.ordinal()];
            for (int i = 0; i < list.size(); i++) {
                if (Objects.equals(list.get(i).getContentPane(), c)) {
                    return new TabFramePosition(a, i);
                }
            }
        }
        return null;
    }

    /**
     * Close a popup.
     *
     * @param a     the alignment position of the popup.{@link TabFramePosition#getAlignment()}
     * @param index the index of the tab.{@link TabFramePosition#getIndex()}
     */
    public void closeTab(final Alignment a, final int index) {
        toggleTab(a, index, false);
    }

    /**
     * Toggles the visibility of a tab.
     *
     * @param a       the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index   the index.{@link TabFramePosition#getIndex()}
     * @param enabled true if visible.
     */
    public void toggleTab(final Alignment a, final int index, final boolean enabled) {
        int oldIndex = selectedIndices[a.getIndex()];
        if (content.isEnabled(a) == enabled && oldIndex == index) return;
        TabFrameTab compAtIndex = getTabComponentAt(a, index);
        compAtIndex.setSelected(enabled);
        notifySelectionChange(compAtIndex);
        setPopupVisibility(compAtIndex, enabled);
        if (enabled) {
            getPopupComponentAt(a).doLayout();
            getPopupComponentAt(a).requestFocus();
        }
        firePropertyChange(TabFramePopup.KEY_VISIBLE_TAB, new TabFramePosition(a, oldIndex), new TabFramePosition(a, index));
    }

    /**
     * Notify the tabFrame that a selection has changed.
     *
     * @param tabComponent the tab at which the selection has changed.
     */
    public void notifySelectionChange(final TabFrameTab tabComponent) {
        if (tabComponent == null) {
            return;
        }
        Alignment a = tabComponent.getOrientation();
        selectedIndices[a.ordinal()] = tabComponent.isSelected() ? tabComponent.getIndex() : -1;
        if (tabComponent.isSelected()) {
            for (TabFrameTab tc : tabsForAlignment(a)) {
                if (tc != null && tc != tabComponent) {
                    tc.setSelected(false);
                }
            }
        }
    }

    /*
     * Set the visibility of the popup.
     */
    private void setPopupVisibility(final TabFrameTab tabComponent,
                                    final boolean selected) {
        Alignment a = tabComponent.getOrientation();
        TabFramePopup c = compsForAlignment(a).get(tabComponent.getIndex());
        c.setEnabled(selected);
        content.setComponentAt(a, c.getComponent());
        content.setEnabled(a, selected);
        doLayout();
    }

    /**
     * Get the popup component at the given position that is currently active.
     *
     * @param a the alignment position. {@link TabFramePosition#getAlignment()}
     * @return the popup component specified by {@link TabFramePopup#getComponent()}.
     */
    public Component getPopupComponentAt(final Alignment a) {
        List<TabFramePopup> tabs = compsForAlignment(a);
        return tabs.get(Math.max(Math.min(tabs.size() - 1, selectedIndices[a.ordinal()]), 0)).getComponent();
    }

    /**
     * Close a popup.
     *
     * @param c the popup to close.
     */
    public void closeTab(final TabFramePopup c) {
        if (c == null) return;
        closeTab(c.getAlignment(), c.getIndex());
    }

    /**
     * Open a popup.
     *
     * @param c the component to open.
     */
    public void openTab(final Component c) {
        TabFramePosition res = findComponent(c);
        if (res != null) {
            openTab(res.getAlignment(), res.getIndex());
        }
    }

    /**
     * Open a popup.
     *
     * @param a     the alignment position of the popup.{@link TabFramePosition#getAlignment()}
     * @param index the index of the tab.{@link TabFramePosition#getIndex()}
     */
    public void openTab(final Alignment a, final int index) {
        toggleTab(a, index, true);
    }

    /**
     * Open a popup.
     *
     * @param c the popup to open.
     */
    public void openTab(final TabFramePopup c) {
        if (c == null) return;
        openTab(c.getAlignment(), c.getIndex());
    }

    /**
     * Add a popup.
     *
     * @param c     the content component.
     * @param title the title.
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     */
    public void addTab(final Component c, final String title, final Alignment a) {
        addTab(c, title, null, a);
    }

    /**
     * Add a popup.
     *
     * @param c     the content component.
     * @param title the title.
     * @param icon  the icon.
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     */
    public void addTab(final Component c, final String title, final Icon icon, final Alignment a) {
        insertTab(c, title, icon, a, tabsForAlignment(a).size());
    }

    /**
     * Add a popup.
     *
     * @param c     the popup.
     * @param title the title.
     * @param icon  the icon.
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     */
    public void addTab(final TabFramePopup c, final String title, final Icon icon, final Alignment a) {
        insertTab(c, title, icon, a, tabsForAlignment(a).size());
    }

    /**
     * Move a tab to a new position.
     *
     * @param tabComp the tab to move.
     * @param a       the new alignment position.{@link TabFramePosition#getAlignment()}
     */
    public void moveTab(final TabFrameTab tabComp, final Alignment a) {
        if (a == tabComp.getOrientation()) {
            return;
        }
        boolean oldSelected = tabComp.isSelected();

        Alignment oldAlign = tabComp.getOrientation();
        int oldIndex = tabComp.getIndex();

        closeTab(oldAlign, oldIndex);

        TabFramePopup comp = compsForAlignment(oldAlign).get(oldIndex);
        removeTab(oldAlign, oldIndex);
        addTab(comp, tabComp, a);

        if (oldSelected) {
            openTab(a, tabComp.getIndex());
        }

        doLayout();
        getTabContainer(a).repaint();
    }

    /**
     * Remove a popup.
     *
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index of the tab.{@link TabFramePosition#getIndex()}
     */
    public void removeTab(final Alignment a, final int index) {
        TabFramePopup comp = compsForAlignment(a).get(index);
        comp.close();
        comp.setTabFrame(null);
        comp.setIndex(-1);
        removeTabComp(a, index);
        doLayout();
        getTabContainer(a).repaint();
    }

    /**
     * Add a popup.
     *
     * @param c   the popup.
     * @param tab the corresponding tab.
     * @param a   the alignment position.{@link TabFramePosition#getAlignment()}
     */
    public void addTab(final TabFramePopup c, final TabFrameTab tab, final Alignment a) {
        insertTab(c, tab, a, tabsForAlignment(a).size());
    }

    /*
     * Remove a tab component from the given position.
     */
    private void removeTabComp(final Alignment a, final int index) {
        List<TabFrameTab> tabs = tabsForAlignment(a);
        //Adjust indices for tabs.
        Iterator<TabFrameTab> iterator = tabs.listIterator(index);
        while (iterator.hasNext()) {
            TabFrameTab tab = iterator.next();
            tab.setIndex(tab.getIndex() - 1);
        }
        TabFrameTab tab = tabs.remove(index);
        getTabContainer(a).remove(tab.getComponent());
    }

    /**
     * Get the popup component at the given position.
     *
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index.{@link TabFramePosition#getIndex()}
     * @return the popup component specified by {@link TabFramePopup#getComponent()}.
     */
    public Component getPopupComponentAt(final Alignment a, final int index) {
        List<TabFramePopup> tabs = compsForAlignment(a);
        return tabs.get(index).getComponent();
    }

    /**
     * Get the component at the given position.
     *
     * @param a the alignment position. {@link TabFramePosition#getAlignment()}
     * @return the component specified by {@link TabFramePopup#getContentPane()}.
     */
    public Component getComponentAt(final Alignment a) {
        List<TabFramePopup> tabs = compsForAlignment(a);
        return tabs.get(Math.max(Math.min(tabs.size() - 1, selectedIndices[a.ordinal()]), 0)).getContentPane();
    }

    /**
     * Get the custom tab component at the given position.
     *
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index.{@link TabFramePosition#getIndex()}
     * @return the user tab component or null if none is installed.
     */
    public Component getUserTabComponentAt(final Alignment a, final int index) {
        TabFrameTab tab = getTabComponentAt(a, index);
        if (tab instanceof TabFrameTabContainer) {
            return ((TabFrameTabContainer) tab).getContent();
        } else {
            return null;
        }
    }

    /**
     * Set the custom tab component at the given position.
     *
     * @param component the custom tab component.
     * @param a         the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index     the index.{@link TabFramePosition#getIndex()}
     */
    public void setUserTabComponentAt(final JComponent component, final Alignment a, final int index) {
        List<TabFrameTab> tabs = tabsForAlignment(a);
        TabFrameTab tabContainer = tabs.get(index);
        if (component == null) {
            if (tabContainer instanceof TabFrameTabContainer) {
                setTabComponent(((TabFrameTabContainer) tabContainer).oldTab, a, index);
            }
        } else {
            if (tabContainer instanceof TabFrameTabContainer) {
                ((TabFrameTabContainer) tabContainer).setContent(component);
            } else {
                TabFrameTabContainer cont = new TabFrameTabContainer(this, component, tabContainer, a, index);
                setTabComponent(cont, a, index);
            }
        }
    }

    /**
     * Set the accelerator index at the given position.
     *
     * @param accelerator the accelerator. (a negative value being no accelerator).
     * @param a           the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index       the index.{@link TabFramePosition#getIndex()}
     */
    public void setAcceleratorAt(final int accelerator, final Alignment a, final int index) {
        getTabComponentAt(a, index).setAccelerator(accelerator);
    }

    /**
     * Get the position of the alignment peer. That being the other position that occupies the same tab container given
     * by {@link #getTabContainer(Alignment)}.
     * <p>
     * NORTH  = NORTH_EAST
     * </p><p>
     * EAST   = SOUTH_EAST
     * </p><p>
     * SOUTH  = SOUTH_WEST
     * </p><p>
     * WEST   = NORTH_WEST
     *
     * @param a the alignment position.{@link TabFramePosition#getAlignment()}
     * @return the peer position.{@link TabFramePosition#getAlignment()}
     */
    public Alignment getPeer(final Alignment a) {
        switch (a) {
            case NORTH:
            case SOUTH:
            case WEST:
            case EAST:
                return a.clockwise();
            case NORTH_EAST:
            case NORTH_WEST:
            case SOUTH_EAST:
            case SOUTH_WEST:
                return a.anticlockwise();
        }
        return a;
    }

    /**
     * Returns whether there is transfer ongoing. How this is realized may depend on the specific UI implementation.
     * This needn't necessarily represent a DnD operation.
     *
     * @return true if a transfer is ongoing.
     */
    public boolean isInTransfer() {
        return inTransfer;
    }

    /**
     * Initialize a tranfer.
     *
     * @param a     the alignment position to be transferred.{@link TabFramePosition#getAlignment()}
     * @param index the index to be transferred.{@link TabFramePosition#getIndex()}
     */
    public void initTransfer(final Alignment a, final int index) {
        getContentPane().getComponent().setEnabled(false);
        this.inTransfer = true;
        this.transferAlign = a;
        this.transferIndex = index;
    }

    /**
     * Get the content pane.
     *
     * @return the content pane.
     */
    public TabFrameContent getContentPane() {
        return content;
    }

    /**
     * Returns whether the given tab is selected.
     *
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index.{@link TabFramePosition#getIndex()}
     * @return true if selected.
     */
    public boolean isSelected(final Alignment a, final int index) {
        if (a == null) return false;
        return selectedIndices[a.ordinal()] == index;
    }

    /**
     * Get the index that is currently selected at the given location or -1 if none is selected.
     *
     * @param a the alignment position.{@link TabFramePosition#getAlignment()}
     * @return the current selected index at the alignment position.
     */
    public int getSelectedIndex(final Alignment a) {
        return selectedIndices[a.ordinal()];
    }

    /**
     * Notify that a transfer has ended.
     */
    public void endTransfer() {
        getContentPane().getComponent().setEnabled(true);
        inTransfer = false;
        transferAlign = null;
        transferIndex = -10;
    }

    /**
     * Get the current transfer info. This contains the position of the tab prepared the be transferred by {@link
     * #initTransfer(Alignment, int)}.
     *
     * @return the transfer position.
     */
    public TabFramePosition getTransferInfo() {
        return new TabFramePosition(transferAlign, transferIndex);
    }

    /**
     * Returns whether DnD operations are enabled.
     *
     * @return true if DnD is enabled.
     */
    public boolean isDndEnabled() {
        return dndEnabled && getTransferHandler() instanceof TabFrameTransferHandler;
    }

    /**
     * Sets whether DnD operations are enabled.
     *
     * @param dndEnabled true if enabled.
     */
    public void setDndEnabled(final boolean dndEnabled) {
        boolean old = this.dndEnabled;
        this.dndEnabled = dndEnabled;
        if (getDropTarget() != null) {
            getDropTarget().setActive(dndEnabled);
        }
        firePropertyChange(KEY_DND, old, dndEnabled);
    }

    /**
     * Set the component at the given position.
     *
     * @param c     the component.
     * @param a     the alignment position.{@link TabFramePosition#getAlignment()}
     * @param index the index.{@link TabFramePosition#getIndex()}
     */
    public void setComponentAt(final Component c, final Alignment a, final int index) {
        compsForAlignment(a).get(index).setContentPane(c);
    }

    /**
     * This class represents a position inside the tabFrame.
     */
    public static class TabFramePosition {
        private Alignment a;
        private int index;


        public TabFramePosition(final Alignment a, final int index) {
            this.a = a;
            this.index = index;
        }

        /**
         * The alignment position. This specifies at what location the tab is placed.
         *
         * @return the alignment position.
         */
        public Alignment getAlignment() {
            return a;
        }

        /**
         * Set the alignment.
         *
         * @param a the alignment.
         */
        public void setAlignment(final Alignment a) {
            this.a = a;
        }

        /**
         * The index inside the alignment position.
         *
         * @return the index.
         */
        public int getIndex() {
            return index;
        }

        /**
         * Set the index.
         *
         * @param index the index.
         */
        public void setIndex(final int index) {
            this.index = index;
        }

        @Override
        public String toString() {
            return "[" + a + "," + index + "]";
        }
    }
}
