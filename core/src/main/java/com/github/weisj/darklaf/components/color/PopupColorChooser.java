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
package com.github.weisj.darklaf.components.color;

import java.awt.*;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.swing.*;

import com.github.weisj.darklaf.components.chooser.ChooserComponent;
import com.github.weisj.darklaf.components.popup.AttachedPopupComponent;
import com.github.weisj.darklaf.components.popup.SharedComponent;
import com.github.weisj.darklaf.util.Lambdas;

public class PopupColorChooser extends JPanel implements ChooserComponent<Color> {

    private static final SharedComponent<SmallColorChooser> sharedChooser =
            new SharedComponent<>(SmallColorChooser::new);
    private final SmallColorChooser chooser;


    protected SmallColorChooser getChooser(final Color initial, final Consumer<Color> callback) {
        SmallColorChooser smallColorChooser = sharedChooser.get();
        smallColorChooser.reset(initial, callback);
        return smallColorChooser;
    }

    public PopupColorChooser(final Color initial, final Consumer<Color> callback) {
        setLayout(new BorderLayout());
        chooser = getChooser(initial, callback);
        add(chooser, BorderLayout.CENTER);
        setBackground(UIManager.getColor("ColorChooser.background"));
    }

    @Override
    public void reset(final Color initial, final Consumer<Color> callback) {
        chooser.reset(initial, callback);
    }

    @Override
    public Color getInitial() {
        return chooser.getInitial();
    }

    @Override
    public Color getSelected() {
        return chooser.getSelected();
    }

    @Override
    public void updateUI() {
        super.updateUI();
        setBackground(UIManager.getColor("ColorChooser.background"));
    }

    public static void attackToComponent(final JComponent component, final Consumer<Color> callback,
            final Supplier<Color> supplier, final Supplier<Boolean> activationCheck,
            final boolean revertOnAbort) {
        AttachedPopupComponent.attachChooser(component, () -> {
            if (!activationCheck.get()) return null;
            return new PopupColorChooser(supplier.get(), callback);
        }, callback, supplier, revertOnAbort);
    }

    public static void showColorChooser(final JComponent parent, final Color initial, final Consumer<Color> callback,
            final Runnable onClose) {
        showColorChooser(parent, initial, callback, onClose, false);
    }

    public static void showColorChooser(final JComponent parent, final Color initial, final Consumer<Color> callback,
            final Runnable onClose, final boolean revertOnEscape) {
        PopupColorChooser chooser = new PopupColorChooser(initial, callback);
        AttachedPopupComponent.showComponent(parent, chooser, onClose, () -> {
            if (revertOnEscape) {
                callback.accept(initial);
            }
            onClose.run();
        }, Lambdas.DO_NOTHING);
    }
}
