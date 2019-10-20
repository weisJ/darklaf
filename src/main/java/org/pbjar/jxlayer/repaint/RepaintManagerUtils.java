/*
 Copyright (c) 2009, Piet Blok
 All rights reserved.
 <p>
 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 <p>
 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.
 * Neither the name of the copyright holder nor the names of the
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.
 <p>
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package org.pbjar.jxlayer.repaint;

import com.weis.darklaf.log.LogFormatter;
import org.jdesktop.swingx.ForwardingRepaintManager;
import org.jetbrains.annotations.Contract;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.logging.ConsoleHandler;
import java.util.logging.Logger;

/**
 * Utility class that ensures that a correct {@link RepaintManager} is set.
 *
 * @author Piet Blok
 */
public final class RepaintManagerUtils {

    private static final Logger LOGGER = Logger.getLogger(RepaintManagerUtils.class.getName());
    /**
     * Indicates the availability of SwingX on the class path.
     */
    private static final boolean swingX = isSwingXAvailable();

    static {
        LOGGER.setUseParentHandlers(false);
        ConsoleHandler handler = new ConsoleHandler();
        handler.setFormatter(new LogFormatter());
        LOGGER.addHandler(handler);
    }

    @Contract(pure = true)
    private RepaintManagerUtils() {
    }

    /**
     * Create and return an {@link Action} that will display the delegate structure of the current
     * {@link RepaintManager}.
     *
     * @return an {@link Action} object
     */
    @Contract(" -> new")
    @NotNull
    public static Action createRPDisplayAction() {
        return new DisplayAction();
    }

    /**
     * Ensure that a specific {@link RepaintManager} is set according to the requirements of the
     * {@link RepaintManagerProvider}.
     *
     * @param c        a component from which the current repaint manager can be obtained.
     * @param provider the provider
     */
    public static void ensureRepaintManagerSet(
            final Component c, @NotNull final RepaintManagerProvider provider) {
        ensureImpl(RepaintManager.currentManager(c), provider);
    }

    /**
     * The actual implementation of ensure.
     *
     * @param delegate a delegate RepaintManager
     * @param provider the provider that provides for the type and implementation of a delegated
     *                 RepaintManager
     */
    private static void ensureImpl(
            @NotNull final RepaintManager delegate, @NotNull final RepaintManagerProvider provider) {
        /*
         * Setup a traversal variable.
         */
        RepaintManager manager = delegate;

        while (!provider.isAdequate(manager.getClass())) {
            if (swingX) {
                if (manager instanceof ForwardingRepaintManager) {
                    manager = ((ForwardingRepaintManager) manager).getDelegateManager();
                } else {
                    RepaintManager.setCurrentManager(
                            createManager(provider.getForwardingRepaintManagerClass(), delegate));
                    break;
                }
            } else {
                if (manager instanceof WrappedRepaintManager) {
                    manager = ((WrappedRepaintManager) manager).getDelegateManager();
                } else {
                    RepaintManager.setCurrentManager(
                            createManager(provider.getWrappedRepaintManagerClass(), delegate));
                    break;
                }
            }
        }
    }

    @NotNull
    private static RepaintManager createManager(
            @NotNull final Class<? extends RepaintManager> clazz, final RepaintManager delegate) {
        try {
            RepaintManager newManager = clazz.getConstructor(RepaintManager.class).newInstance(delegate);
            return newManager;
        } catch (Throwable t) {
            throw new RuntimeException("Cannot instantiate " + clazz.getName(), t);
        }
    }

    /**
     * Ensure that a specific {@link RepaintManager} is set according to the requirements of the
     * {@link RepaintManagerProvider}.
     *
     * @param c        a component from which the current repaint manager can be obtained.
     * @param provider the provider
     */
    public static void ensureRepaintManagerSet(
            final JComponent c, @NotNull final RepaintManagerProvider provider) {
        ensureImpl(RepaintManager.currentManager(c), provider);
    }

    /**
     * Detect the availability of the ForwardingRepaintManager class.
     *
     * @return {@code} true if available, {@code false} otherwise
     */
    private static boolean isSwingXAvailable() {
        try {
            Class<?> clazz = ForwardingRepaintManager.class;
            LOGGER.info("SwingX is available");
            return clazz != null;
        } catch (Throwable t) {
            LOGGER.info("SwingX is not available");
            return false;
        }
    }

    private static class DisplayAction extends AbstractAction {

        private static final long serialVersionUID = 1L;

        public DisplayAction() {
            super("RPM tree");
        }

        @Override
        public void actionPerformed(@NotNull final ActionEvent e) {
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
            JOptionPane.showMessageDialog(
                    c, message, "The RepaintManager tree", JOptionPane.INFORMATION_MESSAGE);
        }

        private void appendClass(@NotNull final PrintWriter writer, @NotNull final Object obj) {
            Class<?> clazz = obj.getClass();
            String prefix = "Class:   ";
            while (clazz != null) {
                writer.println(prefix + clazz.getName());
                clazz = clazz.getSuperclass();
                prefix = "Extends: ";
            }
        }

        private void appendDelegates(@NotNull final PrintWriter writer, @NotNull final Object rp) {
            appendClass(writer, rp);
            @Nullable RepaintManager delegate;
            if (rp instanceof WrappedRepaintManager) {
                delegate = ((WrappedRepaintManager) rp).getDelegateManager();
            } else if (swingX) {
                if (rp instanceof ForwardingRepaintManager) {
                    delegate = ((ForwardingRepaintManager) rp).getDelegateManager();
                } else {
                    delegate = null;
                }
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
