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
import java.awt.event.ActionEvent;
import java.io.PrintWriter;
import java.io.StringWriter;

import javax.swing.*;

/**
 * Utility class that ensures that a correct {@link RepaintManager} is set.
 *
 * @author Piet Blok
 */
public final class RepaintManagerUtils {

    private RepaintManagerUtils() {}

    /**
     * Create and return an {@link Action} that will display the delegate structure of the current
     * {@link RepaintManager}.
     *
     * @return an {@link Action} object
     */
    public static Action createRPDisplayAction() {
        return new DisplayAction();
    }

    /**
     * The actual implementation of ensure.
     *
     * @param delegate a delegate RepaintManager
     * @param provider the provider that provides for the type and implementation of a delegated
     *        RepaintManager
     */
    private static void ensureImpl(final RepaintManager delegate, final RepaintManagerProvider provider) {
        /*
         * Setup a traversal variable.
         */
        RepaintManager manager = delegate;

        while (!provider.isAdequate(manager)) {
            if (manager instanceof WrappedRepaintManager) {
                manager = ((WrappedRepaintManager) manager).getDelegateManager();
            } else {
                RepaintManager.setCurrentManager(provider.createWrappedRepaintManager(delegate));
                break;
            }
        }
    }

    /**
     * Ensure that a specific {@link RepaintManager} is set according to the requirements of the
     * {@link RepaintManagerProvider}.
     *
     * @param c a component from which the current repaint manager can be obtained.
     * @param provider the provider
     */
    public static void ensureRepaintManagerSet(final JComponent c, final RepaintManagerProvider provider) {
        ensureImpl(RepaintManager.currentManager(c), provider);
    }

    private static class DisplayAction extends AbstractAction {

        public DisplayAction() {
            super("RPM tree");
        }

        @Override
        public void actionPerformed(final ActionEvent e) {
            JComponent c = (JComponent) e.getSource();
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            pw.println("The tree for the current RepaintManager:");
            pw.println();
            RepaintManager manager = RepaintManager.currentManager(c);
            appendDelegates(pw, manager);
            pw.close();
            String text = sw.toString();
            JTextPane message = new JTextPane();
            message.setFont(Font.decode(Font.MONOSPACED));
            message.setContentType("text/plain");
            message.setText(text);
            message.setEditable(false);
            JOptionPane.showMessageDialog(c, message, "The RepaintManager tree", JOptionPane.INFORMATION_MESSAGE);
        }

        private void appendClass(final PrintWriter writer, final Object obj) {
            Class<?> clazz = obj.getClass();
            String prefix = "Class:   ";
            while (clazz != null) {
                writer.println(prefix + clazz.getName());
                clazz = clazz.getSuperclass();
                prefix = "Extends: ";
            }
        }

        private void appendDelegates(final PrintWriter writer, final Object rp) {
            appendClass(writer, rp);
            RepaintManager delegate;
            if (rp instanceof WrappedRepaintManager) {
                delegate = ((WrappedRepaintManager) rp).getDelegateManager();
            } else {
                delegate = null;
            }
            if (delegate != null) {
                writer.println();
                writer.println("Delegate:");
                appendDelegates(writer, delegate);
            }
        }
    }
}
