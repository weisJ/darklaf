/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
package com.github.weisj.darklaf.components.text;

import java.util.List;

import javax.swing.text.Document;

import com.github.weisj.darklaf.ui.text.DarkTextFieldUI;

/**
 * {@link SearchTextField} that has a popup that displays the search history. A search entry is
 * added
 *
 * @author Jannis Weis
 */
public class SearchTextFieldWithHistory extends SearchTextField {

    protected final TextFieldHistoryPopup history;

    /**
     * Constructs a new <code>TextField</code>. A default model is created, the initial string is
     * <code>null</code>, and the number of columns is set to 0.
     */
    public SearchTextFieldWithHistory() {
        this(null, null, 0);
    }

    /**
     * Constructs a new <code>JTextField</code> that uses the given text storage model and the given
     * number of columns. This is the constructor through which the other constructors feed. If the
     * document is <code>null</code>, a default model is created.
     *
     * @param doc the text storage to use; if this is <code>null</code>, a default will be provided by
     *        calling the <code>createDefaultModel</code> method
     * @param text the initial string to display, or <code>null</code>
     * @param columns the number of columns to use to calculate the preferred width &gt;= 0; if <code>
     *     columns</code> is set to zero, the preferred width will be whatever naturally results from
     *        the component implementation
     * @throws IllegalArgumentException if <code>columns</code> &lt; 0
     */
    public SearchTextFieldWithHistory(final Document doc, final String text, final int columns) {
        super(doc, text, columns);
        history = new TextFieldHistoryPopup(this, 100, 800);
        putClientProperty(DarkTextFieldUI.KEY_FIND_POPUP, history);
    }

    /**
     * Constructs a new <code>TextField</code> initialized with the specified text. A default model is
     * created and the number of columns is 0.
     *
     * @param text the text to be displayed, or <code>null</code>
     */
    public SearchTextFieldWithHistory(final String text) {
        this(null, text, 0);
    }

    /**
     * Constructs a new empty <code>TextField</code> with the specified number of columns. A default
     * model is created and the initial string is set to <code>null</code>.
     *
     * @param columns the number of columns to use to calculate the preferred width; if columns is set
     *        to zero, the preferred width will be whatever naturally results from the component
     *        implementation
     */
    public SearchTextFieldWithHistory(final int columns) {
        this(null, null, columns);
    }

    /**
     * Constructs a new <code>TextField</code> initialized with the specified text and columns. A
     * default model is created.
     *
     * @param text the text to be displayed, or <code>null</code>
     * @param columns the number of columns to use to calculate the preferred width; if columns is set
     *        to zero, the preferred width will be whatever naturally results from the component
     *        implementation
     */
    public SearchTextFieldWithHistory(final String text, final int columns) {
        this(null, text, columns);
    }

    /**
     * Set the maximum height of the popup. If the size is larger than the specified maximum height the
     * content will be wrapped inside a scroll pane.
     *
     * <p>
     * Note: A value of less than or equal to 0 indicates that the height should not be limited.
     *
     * @param maximumHeight the max height to use.
     */
    public void setMaximumHeight(final int maximumHeight) {
        history.setMaxHeight(maximumHeight);
    }

    /**
     * Get the history as a list.
     *
     * @return the history.
     */
    public List<String> getHistory() {
        return history.getHistory();
    }

    /** Clear all entries from the history. */
    public void clearHistory() {
        history.clearHistory();
    }

    /**
     * Add entry to the history. If the size is greater than the capacity the oldest entry will be
     * deleted.
     *
     * @param entry the entry to add.
     * @see #getLength() getLength
     * @see #setCapacity(int) setCapacity
     * @see #getCapacity() getCapacity
     */
    public void addEntry(final String entry) {
        history.addEntry(entry);
    }

    /**
     * Get the capacity of the history.
     *
     * @return the capacity.
     * @see #setCapacity(int) setCapacity()
     */
    public int getCapacity() {
        return history.getCapacity();
    }

    /**
     * Set the capacity of the history. If the size grows larger than the capacity the oldest entry will
     * be deleted.
     *
     * @param capacity the capacity.
     * @throws IllegalArgumentException if capacity is negative
     */
    public void setCapacity(final int capacity) throws IllegalArgumentException {
        history.setCapacity(capacity);
    }

    /**
     * Get the current length of the history.
     *
     * @return the current length of the history.
     */
    public int getLength() {
        return history.getLength();
    }
}
