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
package com.github.weisj.darklaf.ui.html;

import javax.swing.plaf.basic.BasicHTML;
import javax.swing.text.AttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.View;
import javax.swing.text.html.HTML;
import java.awt.*;

public class DarkHTML extends BasicHTML {

    public static int getBaseline(final View view, final int w, final int h) {
        if (hasParagraph(view)) {
            view.setSize(w, h);
            return getBaseline(view, new Rectangle(0, 0, w, h));
        }
        return -1;
    }

    private static int getBaseline(final View view, Shape bounds) {
        if (view.getViewCount() == 0) {
            return -1;
        }
        AttributeSet attributes = view.getElement().getAttributes();
        Object name = null;
        if (attributes != null) {
            name = attributes.getAttribute(StyleConstants.NameAttribute);
        }
        int index = 0;
        if (name == HTML.Tag.HTML && view.getViewCount() > 1) {
            // For html on widgets the header is not visible, skip it.
            index++;
        }
        bounds = view.getChildAllocation(index, bounds);
        if (bounds == null) {
            return -1;
        }
        View child = view.getView(index);
        if (view instanceof javax.swing.text.ParagraphView) {
            Rectangle rect;
            if (bounds instanceof Rectangle) {
                rect = (Rectangle) bounds;
            } else {
                rect = bounds.getBounds();
            }
            return rect.y + (int) (rect.height *
                    child.getAlignment(View.Y_AXIS));
        }
        return getBaseline(child, bounds);
    }

    private static boolean hasParagraph(final View view) {
        if (view instanceof javax.swing.text.ParagraphView) {
            return true;
        }
        if (view.getViewCount() == 0) {
            return false;
        }
        AttributeSet attributes = view.getElement().getAttributes();
        Object name = null;
        if (attributes != null) {
            name = attributes.getAttribute(StyleConstants.NameAttribute);
        }
        int index = 0;
        if (name == HTML.Tag.HTML && view.getViewCount() > 1) {
            // For html on widgets the header is not visible, skip it.
            index = 1;
        }
        return hasParagraph(view.getView(index));
    }
}
