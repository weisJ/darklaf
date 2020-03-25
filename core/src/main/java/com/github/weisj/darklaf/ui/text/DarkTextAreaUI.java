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
package com.github.weisj.darklaf.ui.text;

import com.github.weisj.darklaf.ui.html.DarkHTML;
import com.github.weisj.darklaf.util.PropertyKey;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.plaf.ComponentUI;
import javax.swing.text.*;
import java.awt.*;
import java.beans.PropertyChangeEvent;

/**
 * @author Jannis Weis
 */
public class DarkTextAreaUI extends DarkTextUI {

    public static ComponentUI createUI(final JComponent ta) {
        return new DarkTextAreaUI();
    }

    /*
     * Implementation of BasicTextAreaUI
     */

    /**
     * This method gets called when a bound property is changed on the associated JTextComponent.  This is a hook which
     * UI implementations may change to reflect how the UI displays bound properties of JTextComponent subclasses. This
     * is implemented to rebuild the View when the
     * <em>WrapLine</em> or the <em>WrapStyleWord</em> property changes.
     *
     * @param evt the property change event
     */
    public void propertyChange(final PropertyChangeEvent evt) {
        super.propertyChange(evt);
        if (evt.getPropertyName().equals("lineWrap") ||
            evt.getPropertyName().equals("wrapStyleWord") ||
            evt.getPropertyName().equals("tabSize")) {
            // rebuild the view
            modelChanged();
        } else if (PropertyKey.EDITABLE.equals(evt.getPropertyName())) {
            updateFocusTraversalKeys();
        }
    }

    /**
     * Fetches the name used as a key to look up properties through the UIManager.  This is used as a prefix to all the
     * standard text properties.
     *
     * @return the name ("TextArea")
     */
    protected String getPropertyPrefix() {
        return "TextArea";
    }

    /**
     * The method is overridden to take into account caret width.
     *
     * @param c the editor component
     * @return the preferred size
     * @throws IllegalArgumentException if invalid value is passed
     * @since 1.5
     */
    public Dimension getPreferredSize(final JComponent c) {
        return super.getPreferredSize(c);
        //the fix for 4785160 is undone
    }

    /**
     * The method is overridden to take into account caret width.
     *
     * @param c the editor component
     * @return the minimum size
     * @throws IllegalArgumentException if invalid value is passed
     * @since 1.5
     */
    public Dimension getMinimumSize(final JComponent c) {
        return super.getMinimumSize(c);
        //the fix for 4785160 is undone
    }

    /**
     * Creates the view for an element.  Returns a WrappedPlainView or PlainView.
     *
     * @param elem the element
     * @return the view
     */
    public View create(final Element elem) {
        Document doc = elem.getDocument();
        Object i18nFlag = doc.getProperty("i18n"/*AbstractDocument.I18NProperty*/);
        if ((i18nFlag != null) && i18nFlag.equals(Boolean.TRUE)) {
            // build a view that support bidi
            return createI18N(elem);
        } else {
            JTextComponent c = getComponent();
            if (c instanceof JTextArea) {
                JTextArea area = (JTextArea) c;
                View v;
                if (area.getLineWrap()) {
                    v = new WrappedPlainView(elem, area.getWrapStyleWord());
                } else {
                    v = new PlainView(elem);
                }
                return v;
            }
        }
        return null;
    }

    protected View createI18N(final Element elem) {
        String kind = elem.getName();
        if (kind != null) {
            if (kind.equals(AbstractDocument.ContentElementName)) {
                return new PlainParagraph(elem);
            } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
                return new BoxView(elem, View.Y_AXIS);
            }
        }
        return null;
    }

    /**
     * Returns the baseline.
     *
     * @throws NullPointerException     {@inheritDoc}
     * @throws IllegalArgumentException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public int getBaseline(final JComponent c, final int width, int height) {
        super.getBaseline(c, width, height);
        Object i18nFlag = ((JTextComponent) c).getDocument().getProperty("i18n");
        Insets insets = c.getInsets();
        if (Boolean.TRUE.equals(i18nFlag)) {
            View rootView = getRootView((JTextComponent) c);
            if (rootView.getViewCount() > 0) {
                height = height - insets.top - insets.bottom;
                int baseline = insets.top;
                int fieldBaseline = DarkHTML.getBaseline(rootView.getView(0),
                                                         width - insets.left - insets.right, height);
                if (fieldBaseline < 0) {
                    return -1;
                }
                return baseline + fieldBaseline;
            }
            return -1;
        }
        FontMetrics fm = c.getFontMetrics(c.getFont());
        return insets.top + fm.getAscent();
    }

    /**
     * Returns an enum indicating how the baseline of the component changes as the size changes.
     *
     * @throws NullPointerException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(
        final JComponent c) {
        super.getBaselineResizeBehavior(c);
        return Component.BaselineResizeBehavior.CONSTANT_ASCENT;
    }

    @Override
    protected DarkCaret.CaretStyle getDefaultCaretStyle() {
        return DarkCaret.CaretStyle.VERTICAL_LINE_STYLE;
    }


    /**
     * Paragraph for representing plain-text lines that support bidirectional text.
     */
    protected static class PlainParagraph extends ParagraphView {

        PlainParagraph(final Element elem) {
            super(elem);
            layoutPool = new PlainParagraph.LogicalView(elem);
            layoutPool.setParent(this);
        }

        public void setParent(final View parent) {
            super.setParent(parent);
            if (parent != null) {
                setPropertiesFromAttributes();
            }
        }

        protected void setPropertiesFromAttributes() {
            Component c = getContainer();
            if ((c != null) && (!c.getComponentOrientation().isLeftToRight())) {
                setJustification(StyleConstants.ALIGN_RIGHT);
            } else {
                setJustification(StyleConstants.ALIGN_LEFT);
            }
        }

        /**
         * Fetch the constraining span to flow against for the given child index.
         */
        public int getFlowSpan(final int index) {
            Component c = getContainer();
            if (c instanceof JTextArea) {
                JTextArea area = (JTextArea) c;
                if (!area.getLineWrap()) {
                    // no limit if unwrapped
                    return Integer.MAX_VALUE;
                }
            }
            return super.getFlowSpan(index);
        }

        protected SizeRequirements calculateMinorAxisRequirements(final int axis,
                                                                  final SizeRequirements r) {
            SizeRequirements req = super.calculateMinorAxisRequirements(axis, r);
            Component c = getContainer();
            if (c instanceof JTextArea) {
                JTextArea area = (JTextArea) c;
                if (!area.getLineWrap()) {
                    // min is pref if unwrapped
                    req.minimum = req.preferred;
                } else {
                    req.minimum = 0;
                    req.preferred = getWidth();
                    if (req.preferred == Integer.MAX_VALUE) {
                        // We have been initially set to MAX_VALUE, but we
                        // don't want this as our preferred.
                        req.preferred = 100;
                    }
                }
            }
            return req;
        }

        /**
         * Sets the size of the view.  If the size has changed, layout is redone.  The size is the full size of the view
         * including the inset areas.
         *
         * @param width  the width (non negative)
         * @param height the height (non negative)
         */
        public void setSize(final float width, final float height) {
            if ((int) width != getWidth()) {
                preferenceChanged(null, true, true);
            }
            super.setSize(width, height);
        }

        /**
         * This class can be used to represent a logical view for a flow.  It keeps the children updated to reflect the
         * state of the model, gives the logical child views access to the view hierarchy, and calculates a preferred
         * span.  It doesn't do any rendering, layout, or model/view translation.
         */
        protected static class LogicalView extends CompositeView {

            LogicalView(final Element elem) {
                super(elem);
            }

            protected void loadChildren(final ViewFactory f) {
                Element elem = getElement();
                if (elem.getElementCount() > 0) {
                    super.loadChildren(f);
                } else {
                    View v = new GlyphView(elem);
                    append(v);
                }
            }

            protected boolean isBefore(final int x, final int y, final Rectangle alloc) {
                return false;
            }

            protected boolean isAfter(final int x, final int y, final Rectangle alloc) {
                return false;
            }

            protected View getViewAtPoint(final int x, final int y, final Rectangle alloc) {
                return null;
            }

            protected void childAllocation(final int index, final Rectangle a) {
            }

            // The following methods don't do anything useful, they
            // simply keep the class from being abstract.

            protected int getViewIndexAtPosition(final int pos) {
                Element elem = getElement();
                if (elem.getElementCount() > 0) {
                    return elem.getElementIndex(pos);
                }
                return 0;
            }

            public float getPreferredSpan(final int axis) {
                if (getViewCount() != 1) {
                    throw new Error("One child view is assumed.");
                }

                View v = getView(0);
                return v.getPreferredSpan(axis);
            }

            public void paint(final Graphics g, final Shape allocation) {
            }

            protected boolean updateChildren(final DocumentEvent.ElementChange ec,
                                             final DocumentEvent e, final ViewFactory f) {
                return false;
            }

            /**
             * Forward the DocumentEvent to the given child view.  This is implemented to reparent the child to the
             * logical view (the children may have been parented by a row in the flow if they fit without breaking) and
             * then execute the superclass behavior.
             *
             * @param v the child view to forward the event to.
             * @param e the change information from the associated document
             * @param a the current allocation of the view
             * @param f the factory to use to rebuild if the view has children
             * @see #forwardUpdate
             * @since 1.3
             */
            protected void forwardUpdateToView(final View v, final DocumentEvent e,
                                               final Shape a, final ViewFactory f) {
                v.setParent(this);
                super.forwardUpdateToView(v, e, a, f);
            }
        }
    }
}
