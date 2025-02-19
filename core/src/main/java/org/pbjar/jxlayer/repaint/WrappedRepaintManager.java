/*
 * Copyright (c) 2009, Piet Blok
 * All rights reserved.
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * <p>
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 * Neither the name of the copyright holder nor the names of the
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * <p>
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
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
 * @author Piet Blok, (adaptions for Darklaf by Jannis Weis)
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
