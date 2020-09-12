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
package com.github.weisj.darklaf.components.tabframe;

import java.awt.*;

import javax.swing.*;

import com.github.weisj.darklaf.util.Alignment;

public class TabFrameTabContainer extends JPanel implements TabFrameTab {

    protected final TabFrameTab oldTab;
    private JTabFrame parent;
    private Component content;
    private Alignment orientation;
    private boolean selected;
    private int index;
    private int accelerator;

    public TabFrameTabContainer(final JTabFrame parent, final JComponent content, final TabFrameTab oldTab,
            final Alignment alignment, final int index) {
        super(new BorderLayout());
        this.parent = parent;
        this.oldTab = oldTab;
        this.accelerator = -1;
        setOpaque(false);
        setOrientation(alignment);
        setIndex(index);
        setContent(content);
    }

    @Override
    public String getUIClassID() {
        return "TabFrameTabContainerUI";
    }

    /**
     * Get the content component.
     *
     * @return the content component.
     */
    public Component getContent() {
        return content;
    }

    /**
     * Set the content to display.
     *
     * @param content the content component.
     */
    public void setContent(final Component content) {
        Component old = this.content;
        remove(content);
        this.content = content;
        add(content, BorderLayout.CENTER);
        firePropertyChange(KEY_CONTENT, old, content);
    }

    @Override
    public Color getBackground() {
        if (content != null && !content.isOpaque()) {
            return super.getBackground();
        }
        return content != null ? content.getBackground() : super.getBackground();
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
        if (this.orientation == a)
            return;
        Alignment oldOrientation = this.orientation;
        this.orientation = a;
        firePropertyChange(KEY_ORIENTATION, oldOrientation, orientation);
    }

    @Override
    public boolean isSelected() {
        return selected;
    }

    @Override
    public void setSelected(final boolean selected) {
        if (selected == this.selected)
            return;
        boolean oldSelected = this.selected;
        this.selected = selected;
        firePropertyChange(KEY_SELECTED, oldSelected, selected);
    }

    @Override
    public int getAccelerator() {
        return accelerator;
    }

    @Override
    public void setAccelerator(final int accelerator) {
        if (this.accelerator == accelerator)
            return;
        int oldAccelerator = this.accelerator;
        this.accelerator = accelerator;
        firePropertyChange(KEY_ACCELERATOR, oldAccelerator, accelerator);
    }

    @Override
    public JTabFrame getTabFrame() {
        return parent;
    }

    @Override
    public void setTabFrame(final JTabFrame parent) {
        JTabFrame old = this.parent;
        this.parent = parent;
        firePropertyChange(KEY_TAB_FRAME_PARENT, old, parent);
    }
}
