/*
 * MIT License
 *
 * Copyright (c) 2019-2021 Jannis Weis
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
package org.pbjar.jxlayer.repaint;

import java.awt.*;

import javax.swing.*;

/**
 * A {@link RepaintManager} that preserves functionality of a wrapped {@code RepaintManager}. All
 * methods will delegate to the wrapped {@code RepaintManager}.
 * <p>
 * When sub classing this class, one must in all overridden methods call the {@code super} method.
 *
 * @author Piet Blok
 * @see RepaintManagerUtils
 * @see RepaintManagerProvider
 */
public abstract class WrappedRepaintManager extends RepaintManager {

    /**
     * The wrapped manager.
     */
    private final RepaintManager delegate;

    /**
     * Construct a {@code RepaintManager} wrapping an existing {@code RepaintManager}.
     *
     * @param delegate an existing RepaintManager
     */
    public WrappedRepaintManager(final RepaintManager delegate) {
        super();
        if (delegate == null) {
            throw new NullPointerException();
        }
        this.delegate = delegate;
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public synchronized void addInvalidComponent(final JComponent invalidComponent) {
        delegate.addInvalidComponent(invalidComponent);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public synchronized void removeInvalidComponent(final JComponent component) {
        delegate.removeInvalidComponent(component);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void addDirtyRegion(final JComponent c, final int x, final int y, final int w, final int h) {
        delegate.addDirtyRegion(c, x, y, w, h);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void addDirtyRegion(final Window window, final int x, final int y, final int w, final int h) {
        delegate.addDirtyRegion(window, x, y, w, h);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public Rectangle getDirtyRegion(final JComponent c) {
        return delegate.getDirtyRegion(c);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void markCompletelyDirty(final JComponent c) {
        delegate.markCompletelyDirty(c);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public boolean isCompletelyDirty(final JComponent c) {
        return delegate.isCompletelyDirty(c);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public Dimension getDoubleBufferMaximumSize() {
        return delegate.getDoubleBufferMaximumSize();
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void markCompletelyClean(final JComponent c) {
        delegate.markCompletelyClean(c);
    }

    /**
     * Get the delegate.
     *
     * @return the delegate
     */
    public RepaintManager getDelegateManager() {
        return delegate;
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void setDoubleBufferMaximumSize(final Dimension d) {
        delegate.setDoubleBufferMaximumSize(d);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void validateInvalidComponents() {
        delegate.validateInvalidComponents();
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void paintDirtyRegions() {
        delegate.paintDirtyRegions();
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public Image getOffscreenBuffer(final Component c, final int proposedWidth, final int proposedHeight) {
        return delegate.getOffscreenBuffer(c, proposedWidth, proposedHeight);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public Image getVolatileOffscreenBuffer(final Component c, final int proposedWidth, final int proposedHeight) {
        return delegate.getVolatileOffscreenBuffer(c, proposedWidth, proposedHeight);
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public boolean isDoubleBufferingEnabled() {
        return delegate.isDoubleBufferingEnabled();
    }

    /**
     * Just delegates. {@inheritDoc}
     */
    @Override
    public void setDoubleBufferingEnabled(final boolean flag) {
        delegate.setDoubleBufferingEnabled(flag);
    }
}
