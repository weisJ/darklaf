/*
 * MIT License
 *
 * Copyright (c) 2020-2021 Jannis Weis
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
 */
package com.github.weisj.darklaf.ui.spinner;

import java.awt.*;

import javax.swing.*;
import javax.swing.plaf.InsetsUIResource;

import com.github.weisj.darklaf.ui.cell.CellUtil;

public class DarkSpinnerLayout implements LayoutManager {

    private final Insets editorInsets;
    private final Insets editorCellInsets;
    private final Insets arrowButtonInsets;
    private Component nextButton = null;
    private Component previousButton = null;
    private Component editor = null;

    public DarkSpinnerLayout() {
        editorInsets = UIManager.getInsets("Spinner.editorInsets");
        editorCellInsets = UIManager.getInsets("Spinner.cellEditorInsets");
        arrowButtonInsets = UIManager.getInsets("Spinner.arrowButtonInsets");
    }

    @Override
    public void addLayoutComponent(final String name, final Component c) {
        if ("Next".equals(name)) {
            nextButton = c;
        } else if ("Previous".equals(name)) {
            previousButton = c;
        } else if ("Editor".equals(name)) {
            editor = c;
        }
    }

    @Override
    public void removeLayoutComponent(final Component c) {
        if (c == nextButton) {
            nextButton = null;
        } else if (c == previousButton) {
            previousButton = null;
        } else if (c == editor) {
            editor = null;
        }
    }

    private Dimension preferredSize(final Component c) {
        return (c == null) ? new Dimension() : c.getPreferredSize();
    }

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        Dimension nextD = preferredSize(nextButton);
        Dimension previousD = preferredSize(previousButton);
        Dimension editorD = preferredSize(editor);

        /*
         * Force the editors height to be a multiple of 2
         */
        editorD.height = ((editorD.height + 1) / 2) * 2;

        Insets editorIns = getEditorInsets(parent);
        Insets buttonIns = arrowButtonInsets;

        Dimension size = new Dimension(editorD.width, editorD.height);
        size.width += Math.max(nextD.width, previousD.width);
        Insets insets = parent.getInsets();
        size.width += insets.left + insets.right + editorIns.left + editorIns.right + buttonIns.left + buttonIns.right;
        size.height += insets.top + insets.bottom + Math.max(editorIns.top, buttonIns.top)
                + Math.max(editorIns.bottom, buttonIns.bottom);
        return size;
    }

    protected Insets getEditorInsets(final Container c) {
        if (SpinnerConstants.isTreeOrTableCellEditor(c)) {
            return CellUtil.adjustEditorInsets(new InsetsUIResource(editorCellInsets.top, editorCellInsets.left,
                    editorCellInsets.bottom, editorCellInsets.right), c);
        } else {
            return editorInsets;
        }
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        return preferredLayoutSize(parent);
    }

    private void setBounds(final Component c, final int x, final int y, final int width, final int height) {
        if (c != null) {
            c.setBounds(x, y, width, height);
        }
    }

    @Override
    public void layoutContainer(final Container parent) {
        int width = parent.getWidth();
        int height = parent.getHeight();

        Insets insets = parent.getInsets();
        Insets editorIns = getEditorInsets(parent);
        int editorX = insets.left + editorIns.left;
        int editorY = insets.top + editorIns.top;
        int editorWidth = width - editorX - insets.right - editorIns.right;
        int editorHeight = height - editorY - insets.bottom - editorIns.bottom;

        if (nextButton == null && previousButton == null) {
            setBounds(editor, editorX, editorY, editorWidth, editorHeight);
            return;
        }

        Dimension nextD = preferredSize(nextButton);
        Dimension previousD = preferredSize(previousButton);
        int buttonsWidth = Math.max(nextD.width, previousD.width);
        Insets buttonInsets = arrowButtonInsets;
        int fullButtonWidth = buttonsWidth + buttonInsets.left + buttonInsets.right;

        editorWidth -= fullButtonWidth;

        /*
         * Deal with the spinner's componentOrientation property.
         */
        int buttonsX;
        if (parent.getComponentOrientation().isLeftToRight()) {
            buttonsX = width - insets.right - fullButtonWidth;
        } else {
            buttonsX = insets.left;
            editorX = width - insets.right - editorIns.right - editorWidth;
        }

        int nextY = insets.top + buttonInsets.top;
        int nextHeight = (height / 2) + (height % 2) - nextY;
        int previousY = nextY + nextHeight;
        int previousHeight = height - insets.bottom - buttonInsets.bottom - previousY;

        setBounds(editor, editorX, editorY, editorWidth, editorHeight);
        setBounds(nextButton, buttonsX, nextY, fullButtonWidth, nextHeight);
        setBounds(previousButton, buttonsX, previousY, fullButtonWidth, previousHeight);
    }
}
