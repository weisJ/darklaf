package com.weis.darklaf.ui.scrollpane;

import org.jetbrains.annotations.Contract;

import javax.swing.*;
import java.awt.*;

public class ScrollLayoutManagerDelegate extends ScrollPaneLayout {
    private final ScrollPaneLayout delegate;

    @Contract(pure = true)
    public ScrollLayoutManagerDelegate(final ScrollPaneLayout delegate) {
        if (delegate == null) {
            throw new IllegalArgumentException("Delegate is null");
        }
        this.delegate = delegate;
    }

    public ScrollPaneLayout getDelegate() {
        return delegate;
    }

    @Override
    public void addLayoutComponent(final String name, final Component comp) {
        delegate.addLayoutComponent(name, comp);
    }

    @Override
    public void removeLayoutComponent(final Component comp) {
        delegate.removeLayoutComponent(comp);
    }

    @Override
    public Dimension preferredLayoutSize(final Container parent) {
        return delegate.preferredLayoutSize(parent);
    }

    @Override
    public Dimension minimumLayoutSize(final Container parent) {
        return delegate.minimumLayoutSize(parent);
    }

    @Override
    public void layoutContainer(final Container parent) {
        delegate.layoutContainer(parent);
    }

    @Override
    public JViewport getRowHeader() {
        return delegate.getRowHeader();
    }

    @Override
    public JViewport getColumnHeader() {
        return delegate.getColumnHeader();
    }

    @Override
    public JScrollBar getVerticalScrollBar() {
        return delegate.getVerticalScrollBar();
    }

    @Override
    public JScrollBar getHorizontalScrollBar() {
        return delegate.getHorizontalScrollBar();
    }

    @Override
    public int getVerticalScrollBarPolicy() {
        return delegate.getVerticalScrollBarPolicy();
    }

    @Override
    public int getHorizontalScrollBarPolicy() {
        return delegate.getHorizontalScrollBarPolicy();
    }

    @Override
    public void setVerticalScrollBarPolicy(final int x) {
        delegate.setVerticalScrollBarPolicy(x);
    }

    @Override
    public void setHorizontalScrollBarPolicy(final int x) {
        delegate.setHorizontalScrollBarPolicy(x);
    }

    @Override
    public JViewport getViewport() {
        return delegate.getViewport();
    }

    @Override
    public Component getCorner(final String key) {
        return delegate.getCorner(key);
    }

    @SuppressWarnings("deprecation")
    @Override
    public Rectangle getViewportBorderBounds(final JScrollPane scrollpane) {
        return delegate.getViewportBorderBounds(scrollpane);
    }
}
