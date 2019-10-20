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
package com.github.weisj.darklaf.components.tabframe;

import com.github.weisj.darklaf.components.alignment.Alignment;

import javax.swing.*;
import java.awt.*;

public interface TabFramePopup {

    /**
     * Get the current content pane. A popup may hold more than one content pane. In this case this method should return
     * the component that is currently displayed.
     *
     * @return the current content pane.
     */
    Component getContentPane();

    /**
     * Sets the content pane. This needn't replace the old content pane if this popup supports multiple content panes.
     */
    void setContentPane(Component contentPane);

    /**
     * Get the component that realizes this popup.
     *
     * @return the component.
     */
    Component getComponent();

    /**
     * Close the popup.
     */
    default void close() {
        if (getTabFrame() != null && getAlignment() != null && getIndex() >= 0
                && getTabFrame().isSelected(getAlignment(), getIndex())) {
            getTabFrame().closeTab(getAlignment(), getIndex());
        }
    }

    /**
     * Get the {{@link JTabFrame}} this popup belongs to.
     *
     * @return the {{@link JTabFrame}}.
     */
    JTabFrame getTabFrame();

    /**
     * Sets the {{@link JTabFrame}} this popup belongs to.
     *
     * @param tabFrame the {{@link JTabFrame}}.
     */
    void setTabFrame(JTabFrame tabFrame);

    /**
     * Gets the alignment position in the {{@link JTabFrame}}.
     *
     * @return the alignment position.
     */
    Alignment getAlignment();

    /**
     * Get the index of the popup.
     *
     * @return the index.
     */
    int getIndex();

    /**
     * Set the index of the popup. This method should only be called from {{@link JTabFrame}}.
     *
     * @param index the index.
     */
    void setIndex(int index);

    /**
     * Sets the alignment position in the {{@link JTabFrame}}. This method should only be called from {{@link
     * JTabFrame}}.
     *
     * @param alignment the alignment position.
     */
    void setAlignment(Alignment alignment);

    /**
     * Open the popup.
     */
    default void open() {
        if (getTabFrame() != null && getAlignment() != null && getIndex() >= 0
                && !getTabFrame().isSelected(getAlignment(), getIndex())) {
            getTabFrame().closeTab(getAlignment(), getIndex());
        }
    }

    /**
     * Returns whether this popup is enabled.
     *
     * @return true if enabled.
     */
    boolean isEnabled();

    /**
     * Sets the enabled status of the popup.
     *
     * @param enabled true if enabled.
     */
    void setEnabled(boolean enabled);

    /**
     * Get the title of the popup.
     *
     * @return the title.
     */
    String getTitle();

    /**
     * Set the title of the popup.
     *
     * @param title the title.
     */
    void setTitle(String title);

    /**
     * Get the icon of the popup.
     *
     * @return the icon.
     */
    Icon getIcon();

    /**
     * Set the icon of the popup.
     *
     * @param icon the icon.
     */
    void setIcon(Icon icon);
}
