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
 *
 */
package com.github.weisj.darklaf.components.treetable.model;

import javax.swing.*;
import javax.swing.event.ListSelectionListener;

public interface DelegatingListSelectionModel extends ListSelectionModel {
    ListSelectionModel getListDelegate();

    @Override
    default void setSelectionInterval(final int index0, final int index1) {
        getListDelegate().setSelectionInterval(index0, index1);
    }

    @Override
    default void addSelectionInterval(final int index0, final int index1) {
        getListDelegate().addSelectionInterval(index0, index1);

    }

    @Override
    default void removeSelectionInterval(final int index0, final int index1) {
        getListDelegate().removeSelectionInterval(index0, index1);
    }

    @Override
    default int getMinSelectionIndex() {
        return getListDelegate().getMinSelectionIndex();
    }

    @Override
    default int getMaxSelectionIndex() {
        return getListDelegate().getMaxSelectionIndex();
    }

    @Override
    default boolean isSelectedIndex(final int index) {
        return getListDelegate().isSelectedIndex(index);
    }

    @Override
    default int getAnchorSelectionIndex() {
        return getListDelegate().getAnchorSelectionIndex();
    }

    @Override
    default void setAnchorSelectionIndex(final int index) {
        getListDelegate().setAnchorSelectionIndex(index);
    }

    @Override
    default int getLeadSelectionIndex() {
        return getListDelegate().getLeadSelectionIndex();
    }

    @Override
    default void setLeadSelectionIndex(final int index) {
        getListDelegate().setLeadSelectionIndex(index);
    }

    @Override
    default void clearSelection() {
        getListDelegate().clearSelection();
    }

    @Override
    default boolean isSelectionEmpty() {
        return getListDelegate().isSelectionEmpty();
    }

    @Override
    default void insertIndexInterval(final int index, final int length, final boolean before) {
        getListDelegate().insertIndexInterval(index, length, before);
    }

    @Override
    default void removeIndexInterval(final int index0, final int index1) {
        getListDelegate().removeIndexInterval(index0, index1);
    }

    @Override
    default void setValueIsAdjusting(final boolean valueIsAdjusting) {
        getListDelegate().setValueIsAdjusting(valueIsAdjusting);
    }

    @Override
    default boolean getValueIsAdjusting() {
        return getListDelegate().getValueIsAdjusting();
    }

    @Override
    default void setSelectionMode(final int selectionMode) {
        getListDelegate().setSelectionMode(selectionMode);
    }

    @Override
    default int getSelectionMode() {
        return getListDelegate().getSelectionMode();
    }

    @Override
    default void addListSelectionListener(final ListSelectionListener x) {
        getListDelegate().addListSelectionListener(x);
    }

    @Override
    default void removeListSelectionListener(final ListSelectionListener x) {
        getListDelegate().removeListSelectionListener(x);
    }

    default int[] getSelectedIndices() {
        int iMin = getMinSelectionIndex();
        int iMax = getMaxSelectionIndex();

        if ((iMin < 0) || (iMax < 0)) {
            return new int[0];
        }

        int[] rvTmp = new int[1 + (iMax - iMin)];
        int n = 0;
        for (int i = iMin; i <= iMax; i++) {
            if (isSelectedIndex(i)) {
                rvTmp[n++] = i;
            }
        }
        int[] rv = new int[n];
        System.arraycopy(rvTmp, 0, rv, 0, n);
        return rv;
    }

    default int getSelectedItemsCount() {
        int iMin = getMinSelectionIndex();
        int iMax = getMaxSelectionIndex();
        int count = 0;

        for (int i = iMin; i <= iMax; i++) {
            if (isSelectedIndex(i)) {
                count++;
            }
        }
        return count;
    }
}
