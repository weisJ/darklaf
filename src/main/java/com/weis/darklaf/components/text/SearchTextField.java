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
package com.weis.darklaf.components.text;

import javax.swing.*;
import javax.swing.text.Document;

public class SearchTextField extends JTextField {

    public static final int SEARCH = 0;

    /**
     * Constructs a new <code>TextField</code>.  A default model is created, the initial string is <code>null</code>,
     * and the number of columns is set to 0.
     */
    public SearchTextField() {
        this(null, null, 0);
    }

    /**
     * Constructs a new <code>JTextField</code> that uses the given text storage model and the given number of columns.
     * This is the constructor through which the other constructors feed. If the document is <code>null</code>, a
     * default model is created.
     *
     * @param doc     the text storage to use; if this is <code>null</code>, a default will be provided by calling the
     *                <code>createDefaultModel</code> method
     * @param text    the initial string to display, or <code>null</code>
     * @param columns the number of columns to use to calculate the preferred width &gt;= 0; if <code>columns</code> is
     *                set to zero, the preferred width will be whatever naturally results from the component
     *                implementation
     * @throws IllegalArgumentException if <code>columns</code> &lt; 0
     */
    public SearchTextField(final Document doc, final String text, final int columns) {
        super(doc, text, columns);
        putClientProperty("JTextField.variant", "search");
        addActionListener(e -> {
            var list = listenerList.getListeners(SearchListener.class);
            var evt = new SearchEvent(SearchTextField.this, SEARCH, "search", getText());
            for (var listener : list) {
                if (listener != null) {
                    listener.searchPerformed(evt);
                }
            }
        });
    }

    /**
     * Constructs a new <code>TextField</code> initialized with the specified text. A default model is created and the
     * number of columns is 0.
     *
     * @param text the text to be displayed, or <code>null</code>
     */
    public SearchTextField(final String text) {
        this(null, text, 0);
    }

    /**
     * Constructs a new empty <code>TextField</code> with the specified number of columns. A default model is created
     * and the initial string is set to
     * <code>null</code>.
     *
     * @param columns the number of columns to use to calculate the preferred width; if columns is set to zero, the
     *                preferred width will be whatever naturally results from the component implementation
     */
    public SearchTextField(final int columns) {
        this(null, null, columns);
    }

    /**
     * Constructs a new <code>TextField</code> initialized with the specified text and columns.  A default model is
     * created.
     *
     * @param text    the text to be displayed, or <code>null</code>
     * @param columns the number of columns to use to calculate the preferred width; if columns is set to zero, the
     *                preferred width will be whatever naturally results from the component implementation
     */
    public SearchTextField(final String text, final int columns) {
        this(null, text, columns);
    }

    /**
     * Add a {@link SearchListener} to this search text field.
     *
     * @param listener the listen to add.
     */
    public void addSearchListener(final SearchListener listener) {
        listenerList.add(SearchListener.class, listener);
    }

    /**
     * Remove a {@link SearchListener} from this search text field.
     *
     * @param listener the listener to remove
     */
    public void removeSearchListener(final SearchListener listener) {
        listenerList.remove(SearchListener.class, listener);
    }
}
