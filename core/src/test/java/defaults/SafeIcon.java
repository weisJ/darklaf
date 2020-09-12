/*
 * MIT License
 *
 * Copyright (c) 2020 Jannis Weis
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
 *
 */
package defaults;

import java.awt.*;
import java.awt.image.BufferedImage;
import java.lang.reflect.InvocationTargetException;

import javax.swing.*;

/**
 * Thanks to Jeanette for the use of this code found at:
 *
 * <p>
 * https://jdnc-incubator.dev.java.net/source/browse/jdnc-incubator/src/kleopatra/java/org
 * /jdesktop/swingx/renderer/UIPropertiesViewer.java?rev=1.2&view=markup
 *
 * <p>
 * Some ui-icons misbehave in that they unconditionally class-cast to the component type they are
 * mostly painted on. Consequently they blow up if we are trying to paint them anywhere else (f.i.
 * in a renderer).
 *
 * <p>
 * This Icon is an adaption of a cool trick by Darryl Burke found at
 * http://tips4java.wordpress.com/2008/12/18/icon-table-cell-renderer
 *
 * <p>
 * The base idea is to instantiate a component of the type expected by the icon, let it paint into
 * the graphics of a bufferedImage and create an ImageIcon from it. In subsequent calls the
 * ImageIcon is used.
 */
final class SafeIcon implements Icon {
    private final Icon wrappee;
    private Icon standIn;

    SafeIcon(final Icon wrappee) {
        this.wrappee = wrappee;
    }

    @Override
    public void paintIcon(final Component c, final Graphics g, final int x, final int y) {
        if (standIn == this) {
            paintFallback(c, g, x, y);
        } else if (standIn != null) {
            standIn.paintIcon(c, g, x, y);
        } else {
            try {
                wrappee.paintIcon(c, g, x, y);
            } catch (final ClassCastException e) {
                createStandIn(e);
                standIn.paintIcon(c, g, x, y);
            }
        }
    }

    @Override
    public int getIconWidth() {
        return wrappee.getIconWidth();
    }

    @Override
    public int getIconHeight() {
        return wrappee.getIconHeight();
    }

    private void paintFallback(final Component c, final Graphics g, final int x, final int y) {
        g.drawRect(x, y, getIconWidth(), getIconHeight());
        g.drawLine(x, y, x + getIconWidth(), y + getIconHeight());
        g.drawLine(x + getIconWidth(), y, x, y + getIconHeight());
    }

    private void createStandIn(final ClassCastException e) {
        try {
            final Class<?> clazz = getClass(e);
            final JComponent standInComponent = getSubstitute(clazz);
            standIn = createImageIcon(standInComponent);
        } catch (final Exception e1) {
            // something went wrong - fallback to this painting
            standIn = this;
        }
    }

    private Class<?> getClass(final ClassCastException e) throws ClassNotFoundException {
        String className = e.getMessage();
        className = className.substring(className.lastIndexOf(" ") + 1);
        return Class.forName(className);
    }

    private JComponent getSubstitute(final Class<?> clazz) throws IllegalAccessException {
        JComponent standInComponent = null;
        try {
            standInComponent = (JComponent) clazz.getDeclaredConstructor().newInstance();
        } catch (final InstantiationException e) {
            standInComponent = new AbstractButton() {};
            ((AbstractButton) standInComponent).setModel(new DefaultButtonModel());
        } catch (NoSuchMethodException | InvocationTargetException e) {
            e.printStackTrace();
        }
        return standInComponent;
    }

    private Icon createImageIcon(final JComponent standInComponent) {
        final BufferedImage image = new BufferedImage(getIconWidth(), getIconHeight(), BufferedImage.TYPE_INT_ARGB);
        final Graphics g = image.createGraphics();
        try {
            wrappee.paintIcon(standInComponent, g, 0, 0);
            return new ImageIcon(image);
        } finally {
            g.dispose();
        }
    }
}
