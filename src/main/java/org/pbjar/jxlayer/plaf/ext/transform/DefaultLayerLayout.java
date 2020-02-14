package org.pbjar.jxlayer.plaf.ext.transform;
/*
 * Copyright (c) 2009, Piet Blok
 * All rights reserved.
 * <p>
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * <p>
 * * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above
 * copyright notice, this list of conditions and the following
 * disclaimer in the documentation and/or other materials provided
 * with the distribution.
 * * Neither the name of the copyright holder nor the names of the
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

import org.jdesktop.jxlayer.JXLayer;

import java.awt.*;
import java.io.Serializable;

/**
 * A copy of the private static inner class in JXLayer.
 *
 * @author Piet Blok
 */
public class DefaultLayerLayout implements LayoutManager, Serializable {
    /**
     * {@inheritDoc}
     */
    public void addLayoutComponent(final String name, final Component comp) {
    }

    /**
     * {@inheritDoc}
     */
    public void removeLayoutComponent(final Component comp) {
    }

    /**
     * {@inheritDoc}
     */

    public Dimension preferredLayoutSize(final Container parent) {
        JXLayer<?> layer = (JXLayer<?>) parent;
        Insets insets = layer.getInsets();
        Dimension ret = new Dimension(insets.left + insets.right, insets.top + insets.bottom);
        Component view = layer.getView();
        if (view != null) {
            Dimension size = view.getPreferredSize();
            if (size.width > 0 && size.height > 0) {
                ret.width += size.width;
                ret.height += size.height;
            }
        }
        return ret;
    }

    /**
     * {@inheritDoc}
     */

    public Dimension minimumLayoutSize(final Container parent) {
        JXLayer<?> layer = (JXLayer<?>) parent;
        Insets insets = layer.getInsets();
        Dimension ret = new Dimension(insets.left + insets.right, insets.top + insets.bottom);
        Component view = layer.getView();
        if (view != null) {
            Dimension size = view.getMinimumSize();
            ret.width += size.width;
            ret.height += size.height;
        }
        if (ret.width == 0 || ret.height == 0) {
            ret.width = 4;
            ret.height = 4;
        }
        return ret;
    }

    /**
     * {@inheritDoc}
     */
    public void layoutContainer(final Container parent) {
        JXLayer<?> layer = (JXLayer<?>) parent;
        Component view = layer.getView();
        Component glassPane = layer.getGlassPane();
        if (view != null) {
            Insets insets = layer.getInsets();
            view.setLocation(insets.left, insets.top);
            view.setSize(
                    layer.getWidth() - insets.left - insets.right,
                    layer.getHeight() - insets.top - insets.bottom);
        }
        if (glassPane != null) {
            glassPane.setLocation(0, 0);
            glassPane.setSize(layer.getWidth(), layer.getHeight());
        }
    }
}
