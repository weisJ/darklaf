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

import com.github.weisj.darklaf.util.Alignment;

import java.awt.*;

public interface TabFrameTab {
    String KEY_SELECTED = "selected";
    String KEY_CONTENT = "content";
    String KEY_TAB_FRAME_PARENT = "tabFrame";
    String KEY_ORIENTATION = "orientation";
    String KEY_ACCELERATOR = "accelerator";
    String KEY_TITLE = "title";

    /**
     * Get the index of the tab.
     *
     * @return the current index.
     */
    int getIndex();

    /**
     * Sets the index of the tab.
     *
     * @param index the index.
     */
    void setIndex(int index);

    /**
     * Get the component housing the tab.
     *
     * @return the display component.
     */
    Component getComponent();

    /**
     * Get the orientation.
     *
     * @return the orientation.
     */
    Alignment getOrientation();

    /**
     * Sets the orientation.
     *
     * @param a the orientation.
     */
    void setOrientation(Alignment a);

    /**
     * Returns whether the tab is selected.
     *
     * @return true if selected.
     */
    boolean isSelected();

    /**
     * Set the selected status of the tab.
     *
     * @param selected true if selected.
     */
    void setSelected(boolean selected);

    /**
     * Returns whether the tab is enabled.
     *
     * @return true if enabled.
     */
    boolean isEnabled();

    /**
     * Sets the enabled status of the tab.
     *
     * @param enabled true if enabled.
     */
    void setEnabled(final boolean enabled);

    /**
     * Get the accelerator.
     *
     * @return the accelerator.
     */
    int getAccelerator();

    /**
     * Set the accelerator.
     *
     * @param accelerator accelerator (positive).
     */
    void setAccelerator(int accelerator);

    /**
     * Get the tab frame this tab currently belongs to.
     *
     * @return the TabFrame.
     */
    JTabFrame getTabFrame();

    /**
     * Set the tab frame this tab currently belongs to.
     *
     * @param tabFrame the TabFrame.
     */
    void setTabFrame(JTabFrame tabFrame);

}
