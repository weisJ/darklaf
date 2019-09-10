package com.weis.darklaf.decorators;

import org.jetbrains.annotations.Contract;

import java.awt.*;

public class LayoutManagerDelegate implements LayoutManager {
    private final LayoutManager delegate;

    @Contract(pure = true)
    public LayoutManagerDelegate(final LayoutManager delegate) {
        if (delegate == null) {
            throw new IllegalArgumentException("Delegate is null");
        }
        this.delegate = delegate;
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
}
