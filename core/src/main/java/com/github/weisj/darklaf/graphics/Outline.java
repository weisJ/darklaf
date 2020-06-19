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
package com.github.weisj.darklaf.graphics;

import java.awt.*;

public enum Outline {
    error {
        @Override
        public void setGraphicsColor(final Graphics2D g, final boolean focused) {
            if (focused) {
                g.setColor(PaintUtil.getErrorFocusGlow());
            } else {
                g.setColor(PaintUtil.getErrorGlow());
            }
        }
    },

    warning {
        @Override
        public void setGraphicsColor(final Graphics2D g, final boolean focused) {
            g.setColor(PaintUtil.getWarningGlow());
        }
    },

    defaultButton {
        @Override
        public void setGraphicsColor(final Graphics2D g, final boolean focused) {
            if (focused) {
                g.setColor(PaintUtil.getFocusGlow());
            }
        }
    },

    focus {
        @Override
        public void setGraphicsColor(final Graphics2D g, final boolean active) {
            if (active) {
                g.setColor(PaintUtil.getFocusGlow());
            } else {
                g.setColor(PaintUtil.getFocusInactiveGlow());
            }
        }
    };

    public abstract void setGraphicsColor(Graphics2D g, boolean focused);
}
