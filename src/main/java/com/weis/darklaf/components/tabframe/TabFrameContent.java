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

import com.weis.darklaf.components.alignment.Alignment;

import java.awt.*;

public interface TabFrameContent {

    /**
     * Get the current component displayed in the middle.
     *
     * @return the content component.
     */
    Component getContent();

    /**
     * Sets the content to displayed in the middle.
     *
     * @param content the content component.
     */
    void setContent(final Component content);

    /**
     * Enables a popup if possible.
     *
     * @param a       the alignment.
     * @param enabled true if enabled.
     */
    void setEnabled(Alignment a, boolean enabled);

    /**
     * Returns whether the given popup is enabled.
     *
     * @param a the alignment of the popup.
     * @return true if enabled.
     */
    boolean isEnabled(Alignment a);

    /**
     * Sets the component at the specified position.
     *
     * @param a         the alignment.
     * @param component the component.
     */
    void setComponentAt(final Alignment a, final Component component);

    /**
     * Get the component that displays the content.
     *
     * @return the display component.
     */
    Component getComponent();

    /**
     * Returns an array with the enabled status of each alignment.
     *
     * @return the enabled status.
     */
    boolean[] getStatus();

    /**
     * Get the popup container at the given location.
     *
     * @param alignment the alignment position.
     * @return the popup container.
     */
    PopupContainer getContainer(Alignment alignment);
}
