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

import javax.swing.*;
import javax.swing.plaf.PanelUI;
import java.awt.*;

/**
 * Popup Component for {@link JTabFrame}.
 *
 * @author Jannis Weis
 * @since 2019
 */
public class PanelPopup extends JPanel implements TabFramePopup {
    private Component content;
    private boolean open;
    private JTabFrame parent;
    private String title;
    private Icon icon;
    private Alignment alignment;
    private int index;

    /**
     * Creates a new Popup that holds one component.
     *
     * @param title   the title of the component.
     * @param content the content of the popup.
     */
    public PanelPopup(final String title, final Component content) {
        this(title, null, content);
    }

    /**
     * Creates a new Popup that holds one component.
     *
     * @param title   the title of the component.
     * @param icon    the icon of the popup.
     * @param content the content of the popup.
     */
    public PanelPopup(final String title, final Icon icon, final Component content) {
        setIcon(icon);
        setTitle(title);
        setContentPane(content);
        setEnabled(false);
    }

    @Override
    public String getUIClassID() {
        return "TabFramePanelPopupUI";
    }

    @Override
    public void setUI(final PanelUI ui) {
        if (!(ui instanceof TabFramePopupUI)) {
            throw new IllegalArgumentException("Ui needs to be of type " + TabFramePopup.class);
        }
        super.setUI(ui);
    }

    public TabFramePopupUI getPopupUI() {
        return (TabFramePopupUI) super.getUI();
    }

    @Override
    public Dimension getSize(final Dimension rv) {
        if (!isEnabled()) {
            return new Dimension(0, 0);
        }
        return super.getSize();
    }

    @Override
    public int getWidth() {
        if (!isEnabled()) {
            return 0;
        }
        return super.getWidth();
    }

    @Override
    public int getHeight() {
        if (!isEnabled()) {
            return 0;
        }
        return super.getHeight();
    }

    @Override
    public Component getContentPane() {
        if (content == null) {
            setContentPane(null);
        }
        return content;
    }

    @Override
    public void setContentPane(final Component component) {
        Component old = this.content;
        this.content = component;
        if (content == null) {
            content = new JPanel();
        }
        firePropertyChange(KEY_CONTENT, old, content);
    }

    @Override
    public Component getComponent() {
        return this;
    }

    @Override
    public void updateContentUI() {
        if (getParent() == null) {
            SwingUtilities.updateComponentTreeUI(this);
        }
    }

    @Override
    public void close() {
        if (parent != null && getAlignment() != null && getIndex() >= 0
            && parent.isSelected(getAlignment(), getIndex())) {
            boolean oldOpen = isOpen();
            parent.closeTab(getAlignment(), getIndex());
            open = false;
            firePropertyChange(KEY_OPEN, oldOpen, false);
        }
    }


    private boolean isOpen() {
        return open;
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

    @Override
    public Alignment getAlignment() {
        return alignment;
    }

    @Override
    public void setAlignment(final Alignment alignment) {
        if (alignment == null || this.alignment == Alignment.CENTER) {
            throw new IllegalArgumentException("Illegal alignment: " + (alignment != null
                                                                        ? alignment.toString() : "null"));
        }
        Alignment old = this.alignment;
        this.alignment = alignment;
        firePropertyChange(KEY_ALIGNMENT, old, alignment);
    }

    @Override
    public void open() {
        if (parent != null && getAlignment() != null && getIndex() >= 0
            && !parent.isSelected(getAlignment(), getIndex())) {
            boolean oldOpen = isOpen();
            parent.openTab(getAlignment(), getIndex());
            open = true;
            firePropertyChange(KEY_OPEN, oldOpen, true);
            requestFocus();
        }
    }

    @Override
    public String getTitle() {
        return title;
    }

    @Override
    public void setTitle(final String title) {
        String old = this.title;
        this.title = title == null ? "" : title;
        firePropertyChange(KEY_TITLE, old, this.title);
    }

    @Override
    public Icon getIcon() {
        return icon;
    }

    @Override
    public void setIcon(final Icon icon) {
        Icon old = this.icon;
        this.icon = icon;
        firePropertyChange(KEY_ICON, old, icon);
    }

    @Override
    public int getIndex() {
        return index;
    }

    @Override
    public void setIndex(final int index) {
        int old = this.index;
        this.index = index;
        firePropertyChange(KEY_INDEX, old, index);
    }
}
