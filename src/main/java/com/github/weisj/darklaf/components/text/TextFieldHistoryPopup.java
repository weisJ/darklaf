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
package com.github.weisj.darklaf.components.text;

import com.github.weisj.darklaf.components.ScrollPopupMenu;
import com.github.weisj.darklaf.decorators.PlainAction;
import com.github.weisj.darklaf.util.StringUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Jannis Weis
 */
public class TextFieldHistoryPopup extends ScrollPopupMenu implements SearchListener {

    private final Set<String> history;
    private final JTextField textField;
    private int capacity;

    /**
     * Create a search popup Menu.
     *
     * @param textField the text field.
     * @param capacity  the length of the history.
     * @param maxH      maximum height.
     */
    public TextFieldHistoryPopup(@NotNull final SearchTextField textField, final int capacity, final int maxH) {
        super(maxH);
        this.textField = textField;
        textField.addSearchListener(this);
        setCapacity(capacity);
        this.history = Collections.newSetFromMap(new LinkedHashMap<String, Boolean>() {
            protected boolean removeEldestEntry(final Map.Entry<String, Boolean> eldest) {
                return size() > capacity;
            }
        });
    }

    /**
     * Get the history as a list.
     *
     * @return the history.
     */
    public List<String> getHistory() {
        return new ArrayList<>(history);
    }

    /**
     * Get the capacity of the history.
     *
     * @return the capacity.
     * @see #setCapacity(int) setCapacity()
     */
    public int getCapacity() {
        return capacity;
    }

    /**
     * Set the capacity of the history. If the size grows larger than the capacity the oldest entry will be deleted.
     *
     * @param capacity the capacity.
     * @throws IllegalArgumentException if capacity < 0
     */
    public void setCapacity(final int capacity) throws IllegalArgumentException {
        if (capacity < 0) throw new IllegalArgumentException("Negative history size is not supported");
        this.capacity = capacity;
    }

    /**
     * Get the current length of the history.
     *
     * @return the current length of the history.
     */
    public int getLength() {
        return history.size();
    }

    @Override
    public void searchPerformed(@NotNull final SearchEvent e) {
        String text = e.getText();
        if (!StringUtil.isBlank(text)) {
            addEntry(text);
        }
    }

    /**
     * Add entry to the history. If the size is greater than the capacity the oldest entry will be deleted.
     *
     * @param entry the entry to add.
     * @see #getLength() getLength
     * @see #setCapacity(int) setCapacity
     * @see #getCapacity() getCapacity
     */
    public void addEntry(final String entry) {
        history.remove(entry);
        history.add(entry);
    }

    @Override
    public void show(final Component invoker, final int x, final int y) {
        if (history.size() == 0) return;
        super.show(invoker, x, y);
    }

    @Override
    protected void showPopup() {
        if (history.size() == 0) {
            firePopupMenuCanceled();
            return;
        }
        this.removeAll();
        LinkedList<String> list = new LinkedList<>(history);
        Iterator<String> itr = list.descendingIterator();
        while (itr.hasNext()) {
            String item = itr.next();
            add(new JMenuItem(new PlainAction(item, () -> textField.setText(item))));
        }
        super.showPopup();
    }

    /**
     * Clear all entries from the history.
     */
    public void clearHistory() {
        history.clear();
    }
}
