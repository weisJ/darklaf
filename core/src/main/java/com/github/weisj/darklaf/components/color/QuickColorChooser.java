/*
 * MIT License
 *
 * Copyright (c) 2021 Jannis Weis
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
import java.awt.dnd.DropTarget;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.function.BiConsumer;
import java.util.function.Consumer;
import java.util.function.Supplier;

import javax.swing.*;

import com.github.weisj.darklaf.components.tooltip.ToolTipStyle;
import com.github.weisj.darklaf.icons.EmptyIcon;
import com.github.weisj.darklaf.icons.SolidColorIcon;
import com.github.weisj.darklaf.transfer.TransferUtil;
import com.github.weisj.darklaf.ui.tooltip.ToolTipConstants;
import com.github.weisj.darklaf.util.Pair;

public class QuickColorChooser extends JPanel {

    private final SolidColorIcon icon;
    private final JCheckBox checkBox;
    private final JLabel colorLabel;
    private final BiConsumer<Boolean, Color> onStatusChange;
    private Pair<DropTarget, AtomicBoolean> dndSupport;

    public QuickColorChooser(final String title, final Color color, final Consumer<Color> onColorChange) {
        this(title, color, (b, c) -> onColorChange.accept(c), false);
    }

    public QuickColorChooser(final String title, final Color color, final BiConsumer<Boolean, Color> onStatusChange,
            final boolean showCheckBox) {
        this(title, color, onStatusChange, showCheckBox, null);
    }

    public QuickColorChooser(final String title, final Color color, final BiConsumer<Boolean, Color> onStatusChange,
            final boolean showCheckBox, final Dimension pickerSize) {
        setLayout(new BoxLayout(this, BoxLayout.X_AXIS));
        setAlignmentX(JComponent.LEFT_ALIGNMENT);
        if (showCheckBox) {
            // u200B is a zero-width space. This is used to enforce a gap with the size of the text-icon-gap
            // property. This way the spacing stays consistent with the spacing of the color label across LaF
            // changes.
            checkBox = new JCheckBox("\u200B");
            checkBox.addActionListener(e -> onStatusChange.accept(isSelected(), getColor()));
            add(checkBox);
        } else {
            checkBox = null;
        }

        icon = pickerSize == null
                ? new SolidColorIcon(color)
                : new SolidColorIcon(color, pickerSize.width, pickerSize.height);
        colorLabel = new JLabel(icon, JLabel.LEFT);
        colorLabel.putClientProperty(ToolTipConstants.KEY_STYLE, ToolTipStyle.BALLOON);
        this.onStatusChange = onStatusChange;
        attachToComponent(colorLabel, this::onColorChange, icon::getColor);

        add(colorLabel);
        if (title != null) {
            add(new JLabel(title, EmptyIcon.create(0), JLabel.LEFT));
        }
    }

    public void setDragEnabled(final boolean dragEnabled) {
        if (GraphicsEnvironment.isHeadless()) return;
        if (dragEnabled && dndSupport == null) {
            dndSupport = TransferUtil.setupDnD(colorLabel, TransferHandler.COPY, Color.class, this::getColor,
                    this::setColor, SolidColorIcon::new);
        }
        if (dndSupport != null) {
            dndSupport.getFirst().setActive(dragEnabled);
            dndSupport.getSecond().set(dragEnabled);
        }
    }

    private void onColorChange(final Color c) {
        onColorChange(c, true);
    }

    private void onColorChange(final Color c, final boolean notifyCallback) {
        boolean notify = notifyCallback;
        if (notify) {
            notify = !Objects.equals(c, icon.getColor());
        }
        if (notify) onStatusChange.accept(isSelected(), c);
        icon.setColor(c);
        colorLabel.repaint();
    }

    public QuickColorChooser(final String title, final Color color, final BiConsumer<Boolean, Color> onStatusChange) {
        this(title, color, onStatusChange, true);
    }

    public static void attachToComponent(final JComponent component, final Consumer<Color> onStatusChange,
            final Supplier<Color> supplier) {
        attachToComponent(component, onStatusChange, supplier, Boolean.TRUE::booleanValue);
    }

    public static void attachToComponent(final JComponent component, final Consumer<Color> onStatusChange,
            final Supplier<Color> supplier, final Supplier<Boolean> activationCheck) {
        PopupColorChooser.attackToComponent(component, onStatusChange, supplier, activationCheck, true);
    }

    @Override
    public void setToolTipText(final String text) {
        colorLabel.setToolTipText(text);
    }

    public boolean isSelected() {
        return checkBox != null && checkBox.isSelected();
    }

    public void setSelected(final boolean selected) {
        if (checkBox != null) checkBox.setSelected(selected);
    }

    public Color getColor() {
        return icon.getColor();
    }

    public void setColor(final Color color) {
        setColor(color, true);
    }

    public void setColor(final Color color, final boolean notifyCallback) {
        onColorChange(color, notifyCallback);
    }
}
