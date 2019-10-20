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
import com.weis.darklaf.icons.EmptyIcon;
import com.weis.darklaf.ui.tabframe.DarkTabFrameTabLabelUI;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.util.Objects;

/**
 * Tab Component for {@link JTabFrame}.
 *
 * @author Jannis Weis
 */
public class TabFrameTabLabel extends JLabel implements TabFrameTab {

    private JTabFrame parent;
    private Alignment orientation;
    private String title;
    private boolean selected;
    private int accelerator;
    private int index;

    /**
     * Create new TabComponent for the frame of {@link JTabFrame}.
     *
     * @param title       the title.
     * @param icon        the icon.
     * @param orientation the alignment.
     * @param index       the index.
     * @param parent      the parent layout manager.
     */
    public TabFrameTabLabel(final String title, final Icon icon, final Alignment orientation,
                            final int index, @NotNull final JTabFrame parent) {
        this.index = index;
        this.accelerator = -1;
        this.parent = parent;
        setOrientation(orientation);
        setIcon(icon == null ? EmptyIcon.create(0) : icon);
        setTitle(title);
        setText(title);
    }

    @Override
    public String getUIClassID() {
        return "TabFrameTabLabelUI";
    }

    public void setUI(final DarkTabFrameTabLabelUI ui) {
        super.setUI(ui);
    }

    @Override
    public int getIndex() {
        return index;
    }

    @Override
    public void setIndex(final int index) {
        this.index = index;
    }

    @Override
    public Component getComponent() {
        return this;
    }

    @Override
    public Alignment getOrientation() {
        return orientation;
    }

    @Override
    public void setOrientation(final Alignment a) {
        if (this.orientation == a) return;
        var oldOrientation = this.orientation;
        this.orientation = a;
        firePropertyChange("orientation", oldOrientation, orientation);
    }

    @Override
    public boolean isSelected() {
        return selected;
    }

    @Override
    public void setSelected(final boolean selected) {
        if (selected == this.selected) return;
        boolean oldSelected = this.selected;
        this.selected = selected;
        firePropertyChange("selected", oldSelected, selected);
    }

    @Override
    public int getAccelerator() {
        return accelerator;
    }

    @Override
    public void setAccelerator(final int accelerator) {
        if (this.accelerator == accelerator) return;
        int oldAccelerator = this.accelerator;
        this.accelerator = accelerator;
        firePropertyChange("accelerator", oldAccelerator, accelerator);
    }

    @Override
    public JTabFrame getTabFrame() {
        return parent;
    }

    @Override
    public void setTabFrame(final JTabFrame parent) {
        var old = this.parent;
        this.parent = parent;
        firePropertyChange("tabFrame", old, parent);
    }

    /**
     * Returns the current title. This needn't be the same as the displayed text. For this use {@link #getText()}
     * instead.
     *
     * @return the current title.
     */
    public String getTitle() {
        return title;
    }

    /**
     * Set the title of the component.
     *
     * @param title the title
     */
    public void setTitle(@Nullable final String title) {
        if (Objects.equals(title, this.title)) return;
        String oldTitle = this.title;
        this.title = title;
        firePropertyChange("title", oldTitle, selected);
    }
}
