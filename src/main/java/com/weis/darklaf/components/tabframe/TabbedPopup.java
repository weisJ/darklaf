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
package com.weis.darklaf.components.tabframe;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

/**
 * Tabbed Popup Component for {@link JTabFrame}.
 *
 * @author Jannis Weis
 */
public class TabbedPopup extends PanelPopup {

    private JTabbedPane tabbedPane;

    /**
     * Creates a Popup that can hold multiple content panes using a TabbedPane.
     *
     * @param title the title of the popup.
     */
    public TabbedPopup(final String title) {
        this(title, null, null);
    }

    /**
     * Creates a Popup that can hold multiple content panes using a TabbedPane.
     *
     * @param title   the title of the popup.
     * @param icon    the icon of the popup.
     * @param content the initial content pane.
     */
    public TabbedPopup(final String title, final Icon icon, final Component content) {
        super(title, icon, content);
        setIcon(icon);
        setTitle(title);
        setContentPane(content);
        setEnabled(false);
    }

    /**
     * Creates a Popup that can hold multiple content panes using a TabbedPane.
     *
     * @param title the title of the popup.
     * @param icon  the icon of the popup.
     */
    public TabbedPopup(final String title, final Icon icon) {
        this(title, icon, null);
    }

    /**
     * Creates a Popup that can hold multiple content panes using a TabbedPane.
     *
     * @param title   the title of the popup.
     * @param content the initial content pane.
     */
    public TabbedPopup(final String title, final Component content) {
        this(title, null, content);
    }

    /**
     * Returns all currently installed content panes.
     * i.e. this area all components currently added as tabs to
     * the TabbedPane.
     * {@see {@link #getTabbedPane()}}.
     *
     * @return a collection of components.
     */
    public Collection<Component> getContentPanes() {
        int size = getTabbedPane().getTabCount();
        List<Component> compList = new ArrayList<>(size);
        for (int i = 0; i < size; i++) {
            compList.add(tabbedPane.getComponentAt(i));
        }
        return compList;
    }

    /**
     * Get the tabbed pane.
     *
     * @return the tabbed pane.
     */
    public JTabbedPane getTabbedPane() {
        if (tabbedPane == null) {
            tabbedPane = createTabbedPane();
        }
        return tabbedPane;
    }

    /**
     * Creates the tabbedPane.
     * Overwrite this method to customize the tabbedPane used.
     *
     * @return a tabbed pane.
     */
    protected JTabbedPane createTabbedPane() {
        return new JTabbedPane();
    }

    @Override
    public String getUIClassID() {
        return "TabFrameTabbedPopupUI";
    }

    /**
     * Gets the currently selected component from the TabbedPane.
     * {@see {@link #getTabbedPane()}}.
     *
     * @return the selected component.
     */
    public Component getContentPane() {
        return tabbedPane.getSelectedComponent();
    }

    /**
     * Adds the component to the tabbed pane if it isn't already added.
     * This method exists to conform the interface methods.
     * It is preferred to directly add to the TabbedPane obtained by
     * {@link #getTabbedPane()}.
     *
     * @param component the component to add.
     */
    public void setContentPane(final Component component) {
        if (component == null) return;
        if (getContentPanes().contains(component)) {
            return;
        }
        getTabbedPane().addTab(component.getName(), component);
    }
}
