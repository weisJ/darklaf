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
