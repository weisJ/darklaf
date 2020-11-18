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
package com.github.weisj.darklaf.components.popup;

import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.swing.*;

import com.github.weisj.darklaf.components.chooser.ChooserComponent;
import com.github.weisj.darklaf.components.tooltip.ToolTipContext;
import com.github.weisj.darklaf.listener.MouseClickListener;
import com.github.weisj.darklaf.ui.DarkPopupFactory;
import com.github.weisj.darklaf.ui.tooltip.DarkToolTipUI;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.Actions;
import com.github.weisj.darklaf.util.Alignment;
import com.github.weisj.darklaf.util.DarkUIUtil;

public class AttachedPopupComponent extends JToolTip {

    private final JComponent content;
    private static ToolTipContext context;

    protected AttachedPopupComponent(final JComponent parent, final JComponent content) {
        setComponent(parent);
        setLayout(new BorderLayout());
        this.content = content;
        add(content);
        setBackground(content.getBackground());
    }

    public static <K, T extends JComponent & ChooserComponent<K>> void attachChooser(final JComponent component,
            final Supplier<T> chooserSupplier, final Consumer<K> callback,
            final Supplier<K> initialValueSupplier) {
        attachChooser(component, chooserSupplier, callback, initialValueSupplier, true);
    }

    public static <K, T extends JComponent & ChooserComponent<K>> void attachChooser(final JComponent component,
            final Supplier<T> chooserSupplier, final Consumer<K> callback,
            final Supplier<K> initialValueSupplier, final boolean revertOnAbort) {
        attackToComponent(component, () -> {
            T comp = chooserSupplier.get();
            comp.reset(initialValueSupplier.get(), callback);
            return comp;
        }, p -> callback.accept(p.getSelected()), p -> {
            if (revertOnAbort) callback.accept(p.getInitial());
        });
    }

    public static <T extends JComponent> void attackToComponent(final JComponent component,
            final Supplier<T> componentSupplier,
            final Consumer<T> onClose, final Consumer<T> onAbort) {
        AtomicBoolean isShowing = new AtomicBoolean(false);
        Runnable listener = () -> {
            if (!component.isEnabled() || isShowing.get()) return;
            T content = componentSupplier.get();
            if (content == null) return;
            isShowing.set(true);
            showComponent(component, content,
                    () -> onClose.accept(content),
                    () -> onAbort.accept(content),
                    () -> isShowing.set(false));
        };
        if (component instanceof AbstractButton) {
            ((AbstractButton) component).addActionListener(e -> {
                listener.run();
            });
        } else {
            component.addMouseListener((MouseClickListener) e -> {
                if (e.isConsumed()) return;
                listener.run();
            });
        }
    }

    public static void showComponent(final JComponent parent, final JComponent content,
            final Runnable onClose, final Runnable onAbort, final Runnable afterClose) {
        AttachedPopupComponent attachedComp = new AttachedPopupComponent(parent, content);
        /*
         * Position is (0,0) as the ToolTipContext figures out the correct location.
         */
        final Popup popup = PopupFactory.getSharedInstance().getPopup(parent, attachedComp, 0, 0);
        popup.show();
        Window window = DarkUIUtil.getWindow(parent);

        AtomicReference<BiConsumer<AWTEvent, Boolean>> close = new AtomicReference<>();
        AtomicReference<Runnable> listenerRemover = new AtomicReference<>();
        AtomicBoolean open = new AtomicBoolean(true);

        ComponentListener windowListener = new ComponentAdapter() {
            @Override
            public void componentMoved(final ComponentEvent e) {
                close.get().accept(e, true);
                listenerRemover.get().run();
            }

            @Override
            public void componentResized(final ComponentEvent e) {
                close.get().accept(e, true);
                listenerRemover.get().run();
            }
        };
        AWTEventListener listener = event -> {
            if (event instanceof MouseEvent) {
                int id = event.getID();
                if (id != MouseEvent.MOUSE_CLICKED && id != MouseEvent.MOUSE_PRESSED) return;
                if (id == MouseEvent.MOUSE_CLICKED && !open.get()) {
                    listenerRemover.get().run();
                    ((MouseEvent) event).consume();
                    return;
                }
            }
            boolean doClose = event instanceof FocusEvent && (!(DarkUIUtil.hasFocus(attachedComp, (FocusEvent) event)
                    || DarkUIUtil.hasFocus(attachedComp) || DarkUIUtil.hasFocus(parent, (FocusEvent) event)));
            if (!doClose) {
                Point p = MouseInfo.getPointerInfo().getLocation();
                Point p2 = new Point(p);
                Point p3 = new Point(p);
                SwingUtilities.convertPointFromScreen(p2, parent);
                SwingUtilities.convertPointFromScreen(p3, attachedComp);
                doClose = !(parent.contains(p2) || attachedComp.contains(p3));
            }
            if (doClose) {
                close.get().accept(event, true);
                if (event instanceof InputEvent) ((InputEvent) event).consume();
            }
        };
        close.set((e, runCloseAction) -> {
            if (!open.get()) return;
            popup.hide();
            open.set(false);
            if (onClose != null && runCloseAction) onClose.run();
        });
        listenerRemover.set(() -> {
            Toolkit.getDefaultToolkit().removeAWTEventListener(listener);
            if (window != null) window.removeComponentListener(windowListener);
            afterClose.run();
        });
        KeyStroke escape = KeyStroke.getKeyStroke(KeyEvent.VK_ESCAPE, 0);
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        attachedComp.getInputMap(WHEN_IN_FOCUSED_WINDOW).put(escape, "closeEscape");
        attachedComp.getInputMap(WHEN_IN_FOCUSED_WINDOW).put(enter, "closeEnter");
        attachedComp.getActionMap().put("closeEscape", Actions.create(e -> {
            onAbort.run();
            close.get().accept(e, false);
            listenerRemover.get().run();
        }));
        attachedComp.getActionMap().put("closeEnter", Actions.create(e -> {
            close.get().accept(e, true);
            listenerRemover.get().run();
        }));
        SwingUtilities.invokeLater(() -> {
            window.addComponentListener(windowListener);
            Toolkit.getDefaultToolkit().addAWTEventListener(listener,
                    AWTEvent.FOCUS_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK);
        });
    }

    @Override
    public void updateUI() {
        super.updateUI();
        putClientProperty(DarkPopupFactory.KEY_FOCUSABLE_POPUP, true);
        putClientProperty(DarkToolTipUI.KEY_STYLE, ToolTipConstants.VARIANT_BALLOON);
        putClientProperty(DarkToolTipUI.KEY_CONTEXT, getContext());
        super.updateUI();
        if (content != null) setBackground(content.getBackground());
    }

    protected ToolTipContext getContext() {
        if (context == null) context = createToolTipContext();
        return context;
    }

    protected ToolTipContext createToolTipContext() {
        return new ToolTipContext().setAlignment(Alignment.CENTER).setCenterAlignment(Alignment.SOUTH)
                .setUseBestFit(true).setToolTipInsets(new Insets(2, 2, 2, 2)).setFallBackPositionProvider(c -> {
                    Window window = DarkUIUtil.getWindow(c.getTarget());
                    Dimension size = c.getToolTip().getPreferredSize();
                    Rectangle bounds = window.getBounds();
                    return new Point(bounds.x + (bounds.width - size.width) / 2,
                            bounds.y + (bounds.height - size.height) / 2);
                });
    }

    @Override
    public String getTipText() {
        return ToolTipConstants.NO_TEXT;
    }

    @Override
    public Dimension getPreferredSize() {
        if (isPreferredSizeSet()) {
            return super.getPreferredSize();
        }
        if (getLayout() != null) {
            return getLayout().preferredLayoutSize(this);
        }
        return super.getPreferredSize();
    }
}
