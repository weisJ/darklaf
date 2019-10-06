package com.weis.darklaf.ui.text;

import com.weis.darklaf.ui.html.DarkHTML;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.plaf.basic.BasicTextFieldUI;
import javax.swing.text.AbstractDocument;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.Element;
import javax.swing.text.FieldView;
import javax.swing.text.GlyphView;
import javax.swing.text.JTextComponent;
import javax.swing.text.ParagraphView;
import javax.swing.text.Position;
import javax.swing.text.View;
import javax.swing.text.ViewFactory;
import java.awt.*;

/**
 * This class is an exact copy of the implementation of {@link BasicTextFieldUI}.
 * In this way it is possible to contain all Laf specific methods in {@link DarkTextFieldUI}, without having to
 * extends {@link BasicTextFieldUI} directly and instead extend the {@link DarkTextUI} base class.
 */
public abstract class DarkTextFieldUIBridge extends DarkTextUI {

    /**
     * Fetches the name used as a key to lookup properties through the
     * UIManager.  This is used as a prefix to all the standard
     * text properties.
     *
     * @return the name ("TextField")
     */
    protected String getPropertyPrefix() {
        return "TextField";
    }

    /**
     * Creates a view (FieldView) based on an element.
     *
     * @param elem the element
     * @return the view
     */
    public View create(final Element elem) {
        Document doc = elem.getDocument();
        Object i18nFlag = doc.getProperty("i18n"/*AbstractDocument.I18NProperty*/);
        if (Boolean.TRUE.equals(i18nFlag)) {
            // To support bidirectional text, we build a more heavyweight
            // representation of the field.
            String kind = elem.getName();
            if (kind != null) {
                if (kind.equals(AbstractDocument.ContentElementName)) {
                    return new GlyphView(elem) {
                        @Override
                        public float getMinimumSpan(final int axis) {
                            // no wrap
                            return getPreferredSpan(axis);
                        }
                    };
                } else if (kind.equals(AbstractDocument.ParagraphElementName)) {
                    return new I18nFieldView(elem);
                }
            }
            // this shouldn't happen, should probably throw in this case.
        }
        return new FieldView(elem);
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
        View rootView = getRootView((JTextComponent) c);
        if (rootView.getViewCount() > 0) {
            Insets insets = c.getInsets();
            height = height - insets.top - insets.bottom;
            if (height > 0) {
                int baseline = insets.top;
                View fieldView = rootView.getView(0);
                int vspan = (int) fieldView.getPreferredSpan(View.Y_AXIS);
                if (height != vspan) {
                    int slop = height - vspan;
                    baseline += slop / 2;
                }
                if (fieldView instanceof I18nFieldView) {
                    int fieldBaseline = DarkHTML.getBaseline(
                            fieldView, width - insets.left - insets.right,
                            height);
                    if (fieldBaseline < 0) {
                        return -1;
                    }
                    baseline += fieldBaseline;
                } else {
                    FontMetrics fm = c.getFontMetrics(c.getFont());
                    baseline += fm.getAscent();
                }
                return baseline;
            }
        }
        return -1;
    }

    /**
     * Returns an enum indicating how the baseline of the component
     * changes as the size changes.
     *
     * @throws NullPointerException {@inheritDoc}
     * @see javax.swing.JComponent#getBaseline(int, int)
     * @since 1.6
     */
    public Component.BaselineResizeBehavior getBaselineResizeBehavior(
            final JComponent c) {
        super.getBaselineResizeBehavior(c);
        return Component.BaselineResizeBehavior.CENTER_OFFSET;
    }


    /**
     * A field view that support bidirectional text via the
     * support provided by ParagraphView.
     */
    static class I18nFieldView extends ParagraphView {

        I18nFieldView(final Element elem) {
            super(elem);
        }

        protected void setJustification(final int j) {
            // Justification is done in adjustAllocation(), so disable
            // ParagraphView's justification handling by doing nothing here.
        }

        /**
         * Fetch the constraining span to flow against for
         * the given child index.  There is no limit for
         * a field since it scrolls, so this is implemented to
         * return <code>Integer.MAX_VALUE</code>.
         */
        public int getFlowSpan(final int index) {
            return Integer.MAX_VALUE;
        }

        /**
         * Renders using the given rendering surface and area on that surface.
         * The view may need to do layout and create child views to enable
         * itself to render into the given allocation.
         *
         * @param g the rendering surface to use
         * @param a the allocated region to render into
         * @see View#paint
         */
        public void paint(final Graphics g, final Shape a) {
            Rectangle r = (Rectangle) a;
            g.clipRect(r.x, r.y, r.width, r.height);
            super.paint(g, adjustAllocation(a));
        }

        /**
         * Adjusts the allocation given to the view
         * to be a suitable allocation for a text field.
         * If the view has been allocated more than the
         * preferred span vertically, the allocation is
         * changed to be centered vertically.  Horizontally
         * the view is adjusted according to the horizontal
         * alignment property set on the associated JTextField
         * (if that is the type of the hosting component).
         *
         * @param a the allocation given to the view, which may need
         *          to be adjusted.
         * @return the allocation that the superclass should use.
         */
        Shape adjustAllocation(final Shape a) {
            if (a != null) {
                Rectangle bounds = a.getBounds();
                int vspan = (int) getPreferredSpan(Y_AXIS);
                int hspan = (int) getPreferredSpan(X_AXIS);
                if (bounds.height != vspan) {
                    int slop = bounds.height - vspan;
                    bounds.y += slop / 2;
                    bounds.height -= slop;
                }

                // horizontal adjustments
                Component c = getContainer();
                if (c instanceof JTextField) {
                    JTextField field = (JTextField) c;
                    BoundedRangeModel vis = field.getHorizontalVisibility();
                    int max = Math.max(hspan, bounds.width);
                    int value = vis.getValue();
                    int extent = Math.min(max, bounds.width - 1);
                    if ((value + extent) > max) {
                        value = max - extent;
                    }
                    vis.setRangeProperties(value, extent, vis.getMinimum(),
                                           max, false);
                    if (hspan < bounds.width) {
                        // horizontally align the interior
                        int slop = bounds.width - 1 - hspan;

                        int align = ((JTextField) c).getHorizontalAlignment();
                        if (isLeftToRight(c)) {
                            if (align == LEADING) {
                                align = LEFT;
                            } else if (align == TRAILING) {
                                align = RIGHT;
                            }
                        } else {
                            if (align == LEADING) {
                                align = RIGHT;
                            } else if (align == TRAILING) {
                                align = LEFT;
                            }
                        }

                        switch (align) {
                            case SwingConstants.CENTER:
                                bounds.x += slop / 2;
                                bounds.width -= slop;
                                break;
                            case SwingConstants.RIGHT:
                                bounds.x += slop;
                                bounds.width -= slop;
                                break;
                        }
                    } else {
                        // adjust the allocation to match the bounded range.
                        bounds.width = hspan;
                        bounds.x -= vis.getValue();
                    }
                }
                return bounds;
            }
            return null;
        }

        static boolean isLeftToRight(final java.awt.Component c) {
            return c.getComponentOrientation().isLeftToRight();
        }

        // --- View methods -------------------------------------------

        /**
         * Determines the resizability of the view along the
         * given axis.  A value of 0 or less is not resizable.
         *
         * @param axis View.X_AXIS or View.Y_AXIS
         * @return the weight -> 1 for View.X_AXIS, else 0
         */
        public int getResizeWeight(final int axis) {
            if (axis == View.X_AXIS) {
                return 1;
            }
            return 0;
        }

        /**
         * Provides a mapping from the document model coordinate space
         * to the coordinate space of the view mapped to it.
         *
         * @param pos the position to convert >= 0
         * @param a   the allocated region to render into
         * @return the bounding box of the given position
         * @throws BadLocationException if the given position does not
         *                              represent a valid location in the associated document
         * @see View#modelToView
         */
        public Shape modelToView(final int pos, final Shape a, final Position.Bias b) throws BadLocationException {
            return super.modelToView(pos, adjustAllocation(a), b);
        }

        /**
         * Provides a mapping from the view coordinate space to the logical
         * coordinate space of the model.
         *
         * @param fx the X coordinate >= 0.0f
         * @param fy the Y coordinate >= 0.0f
         * @param a  the allocated region to render into
         * @return the location within the model that best represents the
         * given point in the view
         * @see View#viewToModel
         */
        public int viewToModel(final float fx, final float fy, final Shape a, final Position.Bias[] bias) {
            return super.viewToModel(fx, fy, adjustAllocation(a), bias);
        }

        /**
         * Provides a mapping from the document model coordinate space
         * to the coordinate space of the view mapped to it.
         *
         * @param p0 the position to convert >= 0
         * @param b0 the bias toward the previous character or the
         *           next character represented by p0, in case the
         *           position is a boundary of two views.
         * @param p1 the position to convert >= 0
         * @param b1 the bias toward the previous character or the
         *           next character represented by p1, in case the
         *           position is a boundary of two views.
         * @param a  the allocated region to render into
         * @return the bounding box of the given position is returned
         * @throws BadLocationException     if the given position does
         *                                  not represent a valid location in the associated document
         * @throws IllegalArgumentException for an invalid bias argument
         * @see View#viewToModel
         */
        public Shape modelToView(final int p0, final Position.Bias b0,
                                 final int p1, final Position.Bias b1, final Shape a)
                throws BadLocationException {
            return super.modelToView(p0, b0, p1, b1, adjustAllocation(a));
        }

        /**
         * Gives notification that something was inserted into the document
         * in a location that this view is responsible for.
         *
         * @param changes the change information from the associated document
         * @param a       the current allocation of the view
         * @param f       the factory to use to rebuild if the view has children
         * @see View#insertUpdate
         */
        public void insertUpdate(final DocumentEvent changes, final Shape a, final ViewFactory f) {
            super.insertUpdate(changes, adjustAllocation(a), f);
            updateVisibilityModel();
        }

        /**
         * Update the visibility model with the associated JTextField
         * (if there is one) to reflect the current visibility as a
         * result of changes to the document model.  The bounded
         * range properties are updated.  If the view hasn't yet been
         * shown the extent will be zero and we just set it to be full
         * until determined otherwise.
         */
        void updateVisibilityModel() {
            Component c = getContainer();
            if (c instanceof JTextField) {
                JTextField field = (JTextField) c;
                BoundedRangeModel vis = field.getHorizontalVisibility();
                int hspan = (int) getPreferredSpan(X_AXIS);
                int extent = vis.getExtent();
                int maximum = Math.max(hspan, extent);
                extent = (extent == 0) ? maximum : extent;
                int value = maximum - extent;
                int oldValue = vis.getValue();
                if ((oldValue + extent) > maximum) {
                    oldValue = maximum - extent;
                }
                value = Math.max(0, Math.min(value, oldValue));
                vis.setRangeProperties(value, extent, 0, maximum, false);
            }
        }

        /**
         * Gives notification that something was removed from the document
         * in a location that this view is responsible for.
         *
         * @param changes the change information from the associated document
         * @param a       the current allocation of the view
         * @param f       the factory to use to rebuild if the view has children
         * @see View#removeUpdate
         */
        public void removeUpdate(final DocumentEvent changes, final Shape a, final ViewFactory f) {
            super.removeUpdate(changes, adjustAllocation(a), f);
            updateVisibilityModel();
        }

    }
}
